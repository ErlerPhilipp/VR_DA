// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Valve.VR

open OpenTK
open OpenTK.Graphics

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.Rendering.GL
open Aardvark.Application.WinForms
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO

[<AutoOpen>]
module Conversions =
    type HmdMatrix44_t with
        member x.Trafo =
            let t = M44f(x.m0,x.m1,x.m2,x.m3,x.m4,x.m5,x.m6,x.m7,x.m8,x.m9,x.m10,x.m11,x.m12,x.m13,x.m14,x.m15) 
            let t = M44d.op_Explicit(t)
            Trafo3d(t,t.Inverse)

    type HmdMatrix34_t with
        member x.Trafo =
            let m44f = 
                M44f(
                    x.m0,x.m4,x.m8, 0.0f,
                    x.m1,x.m5,x.m9, 0.0f,
                    x.m2,x.m6,x.m10, 0.0f,
                    x.m3,x.m7,x.m11, 1.0f
                ) 
            let t = M44d.op_Explicit(m44f.Transposed)
            Trafo3d(t,t.Inverse)

    
exception VrException of string


type VrDeviceType =
    | Other = 0
    | Hmd = 1
    | Controller = 2
    | TrackingReference = 3

type VrEye =
    | Left = 1
    | Right = 2

[<AutoOpen>]
module private Translations =
    let toDeviceType (c : ETrackedDeviceClass) =
        match c with
            | ETrackedDeviceClass.HMD -> VrDeviceType.Hmd
            | ETrackedDeviceClass.Controller -> VrDeviceType.Controller
            | ETrackedDeviceClass.TrackingReference -> VrDeviceType.TrackingReference
            | _ -> VrDeviceType.Other

    let flip = Trafo3d.FromBasis(V3d.IOO, -V3d.OOI, V3d.OIO, V3d.Zero)

    type VRControllerState_t with
        member x.Item
            with get (i : int) =
                match i with
                    | 0 -> x.rAxis0
                    | 1 -> x.rAxis1
                    | 2 -> x.rAxis2
                    | 3 -> x.rAxis3
                    | _ -> x.rAxis4

type VrAxis(system : CVRSystem, axisType : EVRControllerAxisType, deviceIndex : int, index : int) =
    
    let touched = Mod.init false
    let pressed = Mod.init false
    let position = Mod.init None
    let down = new System.Reactive.Subjects.Subject<unit>()
    let up = new System.Reactive.Subjects.Subject<unit>()

  
    member x.Touched = touched :> IMod<_>
    member x.Pressed = pressed :> IMod<_>
    member x.Position = position :> IMod<_>
    member x.Down = down :> System.IObservable<_>
    member x.Up = up :> System.IObservable<_>

    member x.Update(e : VREvent_t, state : VRControllerState_t) =

        if int e.trackedDeviceIndex = deviceIndex then
            let buttonIndex = int e.data.controller.button |> unbox<EVRButtonId>
            if buttonIndex >= EVRButtonId.k_EButton_Axis0 && buttonIndex <= EVRButtonId.k_EButton_Axis4 then
                let axis = buttonIndex - EVRButtonId.k_EButton_Axis0 |> int


                if axis  = index then
                    let eventType = e.eventType |> int |> unbox<EVREventType>
                    transact (fun () ->
                        match eventType with
                            | EVREventType.VREvent_ButtonTouch -> touched.Value <- true
                            | EVREventType.VREvent_ButtonUntouch -> touched.Value <- false
                            | EVREventType.VREvent_ButtonPress -> 
                                down.OnNext()
                                pressed.Value <- true
                            | EVREventType.VREvent_ButtonUnpress -> 
                                up.OnNext()
                                pressed.Value <- false
                            | _ -> ()

                    )

        if touched.Value then
            let pos = state.[index]
            transact (fun () ->
                position.Value <- Some (V2d(pos.x, pos.y))
            )
        else
            match position.Value with
                | Some _ -> transact (fun () -> position.Value <- None)
                | _ -> ()



type VrDevice(system : CVRSystem, deviceType : VrDeviceType, index : int) =
    
    let getString (prop : ETrackedDeviceProperty) =
        let builder = System.Text.StringBuilder(4096, 4096)
        let mutable err = ETrackedPropertyError.TrackedProp_Success
        let len = system.GetStringTrackedDeviceProperty(uint32 index, prop, builder, uint32 builder.Capacity, &err)
        builder.ToString()

    let getInt (prop : ETrackedDeviceProperty) =
        let mutable err = ETrackedPropertyError.TrackedProp_Success
        let len = system.GetInt32TrackedDeviceProperty(uint32 index, prop, &err)

        len

    let vendor  = lazy ( getString ETrackedDeviceProperty.Prop_ManufacturerName_String )
    let model   = lazy ( getString ETrackedDeviceProperty.Prop_ModelNumber_String )
    
    let axis = 
        [|
            for i in 0..4 do
                let t = getInt (ETrackedDeviceProperty.Prop_Axis0Type_Int32 + unbox i) |> unbox<EVRControllerAxisType>
                if t <> EVRControllerAxisType.k_eControllerAxis_None then
                    yield VrAxis(system, t, index, i)
        |]
    
    let deviceToWorld = Mod.init Trafo3d.Identity
    let worldToDevice = deviceToWorld |> Mod.map (fun t -> t.Inverse)

    member x.Update(e : VREvent_t, poses : TrackedDevicePose_t[]) =
        if axis.Length > 0 then
            let mutable state = VRControllerState_t()
            if system.GetControllerState(uint32 index, &state) then
                for a in axis do
                    a.Update(e, state)

        let currentPose = poses.[index]

        if currentPose.bPoseIsValid then
            transact (fun () -> 
                let theirs = currentPose.mDeviceToAbsoluteTracking.Trafo
                deviceToWorld.Value <- theirs * flip.Inverse
            )


    member x.Type = deviceType
    member x.Index = index
    member x.Vendor = vendor.Value
    member x.Model = model.Value
    member x.Axis = axis
    member x.DeviceToWorld = deviceToWorld
    member x.WorldToDevice = worldToDevice


module VrDriver =
    let inline fail fmt = 
        Printf.kprintf (fun str -> 
            Log.error "%s" str
            raise <| VrException str
        ) fmt

    let inline check e =
        if int e <> 0 then 
            let str = sprintf "[VrDriver] got error %A" e
            Log.error "%s" str
            raise <| VrException str

    [<CompiledName("System")>]
    let system =
        let mutable err = EVRInitError.None
        let sys = OpenVR.Init(&err, EVRApplicationType.VRApplication_Scene)
        if err <> EVRInitError.None then
            fail "[VrDriver] could not initialize: %s" (OpenVR.GetStringForHmdError err)
        sys
        
    [<CompiledName("Compositor")>]
    let compositor =
        OpenVR.Compositor
        
    [<CompiledName("DesiredSize")>]
    let desiredSize =
        let mutable width = 0u
        let mutable height = 0u
        system.GetRecommendedRenderTargetSize(&width,&height)
        V2i(int width, int height)
        
    [<CompiledName("Devices")>]
    let devices =
        [|
            for i in 0u .. OpenVR.k_unMaxTrackedDeviceCount-1u do
                let deviceType = system.GetTrackedDeviceClass i
                if deviceType <> ETrackedDeviceClass.Invalid then
                    yield VrDevice(system, toDeviceType deviceType, int i)
        |]


type VrWindow(runtime : IRuntime, samples : int) =
    let system = VrDriver.system
    let compositor = OpenVR.Compositor

    let size = VrDriver.desiredSize

    let screenSignature =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = 1 }
            DefaultSemantic.Depth,  { format = RenderbufferFormat.Depth24Stencil8; samples = 1}
        ]


    let screenSize = V2i(1024, 768)
    let screenDepth = runtime.CreateRenderbuffer(screenSize, RenderbufferFormat.Depth24Stencil8, 1)
    let screenColor = runtime.CreateTexture(screenSize, TextureFormat.Rgba8, 1, 1, 1)
    let screenFbo = runtime.CreateFramebuffer(screenSignature, [DefaultSemantic.Colors, { texture = screenColor; slice = 0; level = 0 } :> IFramebufferOutput; DefaultSemantic.Depth, screenDepth :> IFramebufferOutput])
    let frustum = Frustum.perspective 60.0 0.1 100.0 (float screenSize.X / float screenSize.Y)

    let signature =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = samples }
            DefaultSemantic.Depth,  { format = RenderbufferFormat.Depth24Stencil8; samples = samples}
        ]

    let depth = runtime.CreateRenderbuffer(size, RenderbufferFormat.Depth24Stencil8, samples)
    let color = runtime.CreateRenderbuffer(size, RenderbufferFormat.Rgba8, samples)
    let fbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, color :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])

    let currentViewTrafo = Mod.init Trafo3d.Identity
    let proj = Mod.init Trafo3d.Identity
    let currentSize = Mod.init VrDriver.desiredSize

    let renderPoses = Array.zeroCreate 16
    let gamePoses = Array.zeroCreate 16

    let hmd = VrDriver.devices |> Array.find (fun d -> d.Type = VrDeviceType.Hmd)
    let clear = runtime.CompileClear(signature, Mod.constant C4f.Black, Mod.constant 1.0)

    let gameWindow = new Aardvark.Application.GameWindow(runtime |> unbox,1)

    let time = Mod.init System.DateTime.Now

    let mutable renderTask = RenderTask.empty

    interface IRenderTarget with
        member x.FramebufferSignature = screenSignature
        member x.Runtime = runtime 
        member x.Time = time :> IMod<_>
        member x.RenderTask
            with get() = x.RenderTask
            and set t = x.RenderTask <- t
        member x.Sizes = currentSize :> IMod<_>
        member x.Samples = samples

    interface IRenderControl with
        member x.Mouse = gameWindow.Mouse 
        member x.Keyboard = gameWindow.Keyboard

    member x.Hmd = hmd
    member x.Runtime = runtime 
    member x.Time = time :> IMod<_>
    member x.Sizes = currentSize :> IMod<_>
    member x.Samples = samples
    member x.Mouse = gameWindow.Mouse 
    member x.Keyboard = gameWindow.Keyboard
    member x.FramebufferSignature = signature
    member x.View = currentViewTrafo :> IMod<_>
    member x.Projection = proj :> IMod<_>

    member x.RenderTask
        with get() = renderTask
        and set t = renderTask <- t

    member x.Run() =
        let mutable evt = Unchecked.defaultof<_>
        compositor.CompositorBringToFront()
        let lHeadToEye = system.GetEyeToHeadTransform(EVREye.Eye_Left).Trafo.Inverse
        let rHeadToEye = system.GetEyeToHeadTransform(EVREye.Eye_Right).Trafo.Inverse
        let lProj = system.GetProjectionMatrix(EVREye.Eye_Left, 0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo
        let rProj = system.GetProjectionMatrix(EVREye.Eye_Right,0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo

        let flip = Trafo3d.FromBasis(V3d.IOO, -V3d.OOI, V3d.OIO, V3d.Zero)

        let l = obj()

        let tex = Mod.custom (fun self -> screenColor :> ITexture)

        let controller = VrDriver.devices
        controller |> Array.iteri (fun _ c ->
            if c.Type = VrDeviceType.Controller then
                c.Axis |> Array.iteri (fun ai a ->
                    a.Position |> Mod.unsafeRegisterCallbackKeepDisposable (fun v ->
                        Log.line "c%d/a%d: %A" c.Index ai v
                    ) |> ignore
                )
        )

        let run () = 
            let renderCtx = ContextHandle.create()


            while true do

                if not (system.PollNextEvent(&evt, sizeof<VREvent_t> |> uint32)) then
                    evt.trackedDeviceIndex <- 0xFFFFFFFFu

                compositor.WaitGetPoses(renderPoses,gamePoses) |> VrDriver.check


                for d in VrDriver.devices do
                    d.Update(evt, renderPoses)

                let pose = renderPoses.[hmd.Index]
                let viewTrafo = hmd.WorldToDevice.GetValue()

                let sw = System.Diagnostics.Stopwatch()

                let runtime = runtime |> unbox<Aardvark.Rendering.GL.Runtime>
                let ctx = runtime.Context

                let renderTask = unbox<Aardvark.Rendering.GL.RenderTasks.RenderTask> renderTask

                using (ctx.RenderingLock renderCtx) (fun _ -> 
                    
                    sw.Restart()
                    renderTask.RenderTaskLock.Run (fun () ->
                        // render left
                        clear.Run(fbo) |> ignore
                        transact(fun () -> 
                            currentViewTrafo.Value <- viewTrafo * lHeadToEye
                            proj.Value <- lProj
                        )
                        renderTask.Run fbo |> ignore
                        //runtime.ResolveMultisamples(color, lTex, ImageTrafo.Rot0)
                        let mutable leftTex = Texture_t(eColorSpace = EColorSpace.Auto, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint (unbox<int> color.Handle))
                        let mutable leftBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
                        compositor.Submit(EVREye.Eye_Left, &leftTex, &leftBounds, EVRSubmitFlags.Submit_GlRenderBuffer) |> VrDriver.check
        

                        // render right
                        clear.Run(fbo) |> ignore
                        transact(fun () -> 
                            currentViewTrafo.Value <- viewTrafo * rHeadToEye
                            proj.Value <- rProj
                        )
                        renderTask.Run fbo |> ignore
                        //runtime.ResolveMultisamples(color, rTex, ImageTrafo.Rot0)
                        let mutable rightTex = Texture_t(eColorSpace = EColorSpace.Auto, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint (unbox<int> color.Handle))
                        let mutable rightBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
                        compositor.Submit(EVREye.Eye_Right, &rightTex, &rightBounds, EVRSubmitFlags.Submit_GlRenderBuffer) |> VrDriver.check

                    )
                    sw.Stop()
                    transact (fun () -> 
                        tex.MarkOutdated()
                        //printfn "%A" sw.Elapsed.TotalMilliseconds
                        //printfn "%A" (System.DateTime.Now - time.Value)
                        time.Value <- System.DateTime.Now
                    )

                )

        System.Threading.Tasks.Task.Factory.StartNew(run) |> ignore

        let fsq =
            Sg.fullScreenQuad
                |> Sg.depthTest (Mod.constant DepthTestMode.None)
                |> Sg.diffuseTexture tex
                |> Sg.effect [ DefaultSurfaces.diffuseTexture |> toEffect ]

        let fsqTask = runtime.CompileRender(signature, fsq)
        gameWindow.RenderTask <- fsqTask

        while true do ()
        //gameWindow.Run()

//            let runtime = runtime |> unbox<Aardvark.Rendering.GL.Runtime>
//            let ctx = runtime.Context
//            let handle = new ContextHandle(gameWindow.Context, gameWindow.WindowInfo)
//            using ( ctx.RenderingLock(handle) ) (fun _ ->  
//                OpenTK.Graphics.OpenGL4.GL.ClearColor(1.0f,0.0f,0.0f,1.0f)
//                OpenTK.Graphics.OpenGL4.GL.Clear(OpenTK.Graphics.OpenGL4.ClearBufferMask.ColorBufferBit)
//                gameWindow.SwapBuffers()
//            )


module Lod =
    open System

    module Helpers = 
        let rand = Random()
        let randomPoints (bounds : Box3d) (pointCount : int) =
            let size = bounds.Size
            let randomV3f() = V3d(rand.NextDouble(), rand.NextDouble(), rand.NextDouble()) * size + bounds.Min |> V3f.op_Explicit
            let randomColor() = C4b(rand.NextDouble(), rand.NextDouble(), rand.NextDouble(), 1.0)

            IndexedGeometry(
                Mode = IndexedGeometryMode.PointList,
                IndexedAttributes = 
                    SymDict.ofList [
                            DefaultSemantic.Positions, Array.init pointCount (fun _ -> randomV3f()) :> Array
                            DefaultSemantic.Colors, Array.init pointCount (fun _ -> randomColor()) :> Array
                    ]
            )

        let randomColor() =
            C4b(128 + rand.Next(127) |> byte, 128 + rand.Next(127) |> byte, 128 + rand.Next(127) |> byte, 255uy)
        let randomColor2 ()  =
            C4b(rand.Next(255) |> byte, rand.Next(255) |> byte, rand.Next(255) |> byte, 255uy)

        let box (color : C4b) (box : Box3d) =

            let randomColor = color //C4b(rand.Next(255) |> byte, rand.Next(255) |> byte, rand.Next(255) |> byte, 255uy)

            let indices =
                [|
                    1;2;6; 1;6;5
                    2;3;7; 2;7;6
                    4;5;6; 4;6;7
                    3;0;4; 3;4;7
                    0;1;5; 0;5;4
                    0;3;2; 0;2;1
                |]

            let positions = 
                [|
                    V3f(box.Min.X, box.Min.Y, box.Min.Z)
                    V3f(box.Max.X, box.Min.Y, box.Min.Z)
                    V3f(box.Max.X, box.Max.Y, box.Min.Z)
                    V3f(box.Min.X, box.Max.Y, box.Min.Z)
                    V3f(box.Min.X, box.Min.Y, box.Max.Z)
                    V3f(box.Max.X, box.Min.Y, box.Max.Z)
                    V3f(box.Max.X, box.Max.Y, box.Max.Z)
                    V3f(box.Min.X, box.Max.Y, box.Max.Z)
                |]

            let normals = 
                [| 
                    V3f.IOO;
                    V3f.OIO;
                    V3f.OOI;

                    -V3f.IOO;
                    -V3f.OIO;
                    -V3f.OOI;
                |]

            IndexedGeometry(
                Mode = IndexedGeometryMode.TriangleList,

                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                        DefaultSemantic.Normals, indices |> Array.mapi (fun ti _ -> normals.[ti / 6]) :> Array
                        DefaultSemantic.Colors, indices |> Array.map (fun _ -> randomColor) :> Array
                    ]

            )

        let wireBox (color : C4b) (box : Box3d) =
            let indices =
                [|
                    1;2; 2;6; 6;5; 5;1;
                    2;3; 3;7; 7;6; 4;5; 
                    7;4; 3;0; 0;4; 0;1;
                |]

            let positions = 
                [|
                    V3f(box.Min.X, box.Min.Y, box.Min.Z)
                    V3f(box.Max.X, box.Min.Y, box.Min.Z)
                    V3f(box.Max.X, box.Max.Y, box.Min.Z)
                    V3f(box.Min.X, box.Max.Y, box.Min.Z)
                    V3f(box.Min.X, box.Min.Y, box.Max.Z)
                    V3f(box.Max.X, box.Min.Y, box.Max.Z)
                    V3f(box.Max.X, box.Max.Y, box.Max.Z)
                    V3f(box.Min.X, box.Max.Y, box.Max.Z)
                |]

            let normals = 
                [| 
                    V3f.IOO;
                    V3f.OIO;
                    V3f.OOI;

                    -V3f.IOO;
                    -V3f.OIO;
                    -V3f.OOI;
                |]

            IndexedGeometry(
                Mode = IndexedGeometryMode.LineList,

                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                        DefaultSemantic.Normals, indices |> Array.mapi (fun ti _ -> normals.[ti / 6]) :> Array
                        DefaultSemantic.Colors, indices |> Array.map (fun _ -> color) :> Array
                    ]

            )

        let frustum (f : IMod<Trafo3d>) (proj : IMod<Trafo3d>) =
            let invViewProj = Mod.map2 (fun (v : Trafo3d) p -> (v * p).Inverse) f proj

            let positions = 
                [|
                    V3f(-1.0, -1.0, -1.0)
                    V3f(1.0, -1.0, -1.0)
                    V3f(1.0, 1.0, -1.0)
                    V3f(-1.0, 1.0, -1.0)
                    V3f(-1.0, -1.0, 1.0)
                    V3f(1.0, -1.0, 1.0)
                    V3f(1.0, 1.0, 1.0)
                    V3f(-1.0, 1.0, 1.0)
                |]

            let indices =
                [|
                    1;2; 2;6; 6;5; 5;1;
                    2;3; 3;7; 7;6; 4;5; 
                    7;4; 3;0; 0;4; 0;1;
                |]

            let geometry =
                IndexedGeometry(
                    Mode = IndexedGeometryMode.LineList,
                    IndexedAttributes =
                        SymDict.ofList [
                            DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                            DefaultSemantic.Colors, Array.create indices.Length C4b.Red :> Array
                        ]
                )

            geometry
                |> Sg.ofIndexedGeometry
                |> Sg.trafo invViewProj

    module Instanced =
        open FShade
        open Aardvark.SceneGraph.Semantics
        type Vertex = { 
                [<Position>] pos : V4d 
                [<Color>] col : V4d
                [<TexCoord>] tc : V2d
                [<Semantic("ZZZInstanceTrafo")>] trafo : M44d
            }

        type Fragment = { 
                [<Color>] col : V4d
                [<TexCoord>] tc : V2d
                [<Normal>] n : V3d
            }
        let trafo (v : Vertex) =
            vertex {
                return { 
                    pos = uniform.ViewProjTrafo * v.trafo * v.pos
                    col = v.col
                    trafo = v.trafo
                    tc = v.tc
                }
            }
            
        let diffuse (v : Effects.Vertex) =
            fragment {
                return V4d(v.c.XYZ * v.n.Z, v.c.W)
            }

        let pointSprite (p : Point<Effects.Vertex>) =
            triangle {
                
                let s = uniform.PointSize
                let pos = p.Value.wp
                let r = uniform.ViewTrafoInv * V4d.IOOO |> Vec.xyz
                let u = uniform.ViewTrafoInv * V4d.OIOO |> Vec.xyz
                let pxyz = pos.XYZ

                let p00 = pxyz - s * r - s * u
                let p01 = pxyz - s * r + s * u
                let p10 = pxyz + s * r - s * u
                let p11 = pxyz + s * r + s * u

                yield { p.Value with pos = uniform.ViewProjTrafo * V4d(p00, 1.0); tc = V2d.OO; }
                yield { p.Value with pos = uniform.ViewProjTrafo * V4d(p10, 1.0); tc = V2d.IO; }
                yield { p.Value with pos = uniform.ViewProjTrafo * V4d(p01, 1.0); tc = V2d.OI; }
                yield { p.Value with pos = uniform.ViewProjTrafo * V4d(p11, 1.0); tc = V2d.II; }
            }

        let pointSpriteFragment (v : Effects.Vertex) =
            fragment {
                let c = 2.0 * v.tc - V2d.II
                if c.Length > 1.0 then
                    discard()

                let z = sqrt (1.0 - c.LengthSquared)
                let n = V3d(c.XY,z)

               
                return { 
                    col = v.c
                    tc = v.tc
                    n = n
                } 
            }


    type DummyDataProvider(root : Box3d) =
    
        interface ILodData with
            member x.BoundingBox = root

            member x.Traverse f =
                let rec traverse (level : int) (b : Box3d) =
                    let box = b
                    let n = 100.0
                    let node = { id = b; level = level; bounds = box; inner = true; granularity = Fun.Cbrt(box.Volume / n); render = true}

                    if f node then
                        let center = b.Center

                        let children =
                            let l = b.Min
                            let u = b.Max
                            let c = center
                            [
                                Box3d(V3d(l.X, l.Y, l.Z), V3d(c.X, c.Y, c.Z))
                                Box3d(V3d(c.X, l.Y, l.Z), V3d(u.X, c.Y, c.Z))
                                Box3d(V3d(l.X, c.Y, l.Z), V3d(c.X, u.Y, c.Z))
                                Box3d(V3d(c.X, c.Y, l.Z), V3d(u.X, u.Y, c.Z))
                                Box3d(V3d(l.X, l.Y, c.Z), V3d(c.X, c.Y, u.Z))
                                Box3d(V3d(c.X, l.Y, c.Z), V3d(u.X, c.Y, u.Z))
                                Box3d(V3d(l.X, c.Y, c.Z), V3d(c.X, u.Y, u.Z))
                                Box3d(V3d(c.X, c.Y, c.Z), V3d(u.X, u.Y, u.Z))
                            ]

                        children |> List.iter (traverse (level + 1))
                    else
                        ()
                traverse 0 root

            member x.Dependencies = []

            member x.GetData (cell : LodDataNode) =
                async {
                    //do! Async.SwitchToThreadPool()
                    let box = cell.bounds
                    let points = 
                        [| for x in 0 .. 9 do
                             for y in 0 .. 9 do
                                for z in 0 .. 9 do
                                    yield V3d(x,y,z)*0.1*box.Size + box.Min |> V3f.op_Explicit
                         |]
                    let colors = Array.create points.Length (Helpers.randomColor())
                    //let points = Helpers.randomPoints cell.bounds 1000
                    //let b = Helpers.box (Helpers.randomColor()) cell.bounds
//                  
                    //do! Async.Sleep(100)
                    let mutable a = 0

//                    for i in 0..(1 <<< 20) do a <- a + 1
//
//                    let a = 
//                        let mutable a = 0
//                        for i in 0..(1 <<< 20) do a <- a + 1
//                        a

                    return Some <| IndexedGeometry(Mode = unbox a, IndexedAttributes = SymDict.ofList [ DefaultSemantic.Positions, points :> Array; DefaultSemantic.Colors, colors :> System.Array])
                }

    let data = DummyDataProvider(Box3d.FromMinAndSize(V3d.OOO, 200.0 * V3d.III)) :> ILodData

    
    let scene (view : IMod<Trafo3d>) (win : VrWindow) (r : IRuntime) =

        let eff =
            let effects = [
                Instanced.trafo |> toEffect           
                DefaultSurfaces.vertexColor  |> toEffect         
            ]
            let e = FShade.SequentialComposition.compose effects
            FShadeSurface(e) :> ISurface 

        let surf = 
            win.Runtime.PrepareSurface(
                win.FramebufferSignature,
                eff
            ) :> ISurface |> Mod.constant

        let f = Frustum.perspective 100.0 0.2 40.0 (1.7) |> Frustum.projTrafo

        let cloud =
            Sg.pointCloud data {
                targetPointDistance     = Mod.constant 200.0
                maxReuseRatio           = 0.5
                minReuseCount           = 1L <<< 20
                pruneInterval           = 500
                customView              = Some view //win.Hmd.WorldToDevice 
                customProjection        = Some (Mod.constant f)
                attributeTypes =
                    Map.ofList [
                        DefaultSemantic.Positions, typeof<V3f>
                        DefaultSemantic.Colors, typeof<C4b>
                    ]
                boundingBoxSurface      = Some surf
            } 
                 
        let somePoints =
            let positions =
                [|
                    for z in 0 .. 50 do
                        yield V3f(0.0f, 0.0f, 0.2f * float32 z)
                |]
            
            IndexedGeometryMode.PointList
                |> Sg.draw
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant positions)
                |> Sg.vertexBufferValue DefaultSemantic.Colors (Mod.constant V4f.IOOI)  
        let sg = 
            Sg.group' [
                cloud
                    |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect                  
                        DefaultSurfaces.vertexColor  |> toEffect         
                        Instanced.pointSprite  |> toEffect     
                        Instanced.pointSpriteFragment  |> toEffect
                        Instanced.diffuse |> toEffect 
                    ]
//                Helpers.frustum win.Hmd.WorldToDevice (Mod.constant f)
//                    |> Sg.effect [
//                        DefaultSurfaces.trafo |> toEffect                  
//                        DefaultSurfaces.constantColor C4f.Green  |> toEffect    
//                    ]

                data.BoundingBox.EnlargedByRelativeEps(0.005)
                    |> Helpers.wireBox C4b.VRVisGreen
                    |> Sg.ofIndexedGeometry
            ]

        let final =
            sg |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect                  
                    DefaultSurfaces.vertexColor  |> toEffect 
                    ]
                |> Sg.uniform "PointSize" (Mod.constant 0.05)
                |> Sg.uniform "ViewportSize" win.Sizes

        final

[<EntryPoint>]
let main argv =
    
    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication()



    let win = VrWindow(app.Runtime, 16)

    let box = Sg.box' C4b.Red Box3d.Unit
        
    let flip = Trafo3d.FromBasis(V3d.IOO, -V3d.OOI, V3d.OIO, V3d.Zero)
    let firstController = VrDriver.devices |> Array.find (fun c -> c.Type = VrDeviceType.Controller)
    let trafo = firstController.DeviceToWorld

    let moveController =
        controller {
            let! state = firstController.Axis.[1].Position

            match state with
                | Some dir -> 
                    let speed = dir.X
                    let! dt = differentiate win.Time
                    return fun (t : Trafo3d) ->
                        let forward = firstController.DeviceToWorld.GetValue().Forward.TransformDir(V3d.OOI)
                        t * Trafo3d.Translation(forward * speed * 8.0 * dt.TotalSeconds)
                | _ ->
                    ()
        }

    let moveTrafo = AFun.integrate moveController Trafo3d.Identity
    let moveTrafoInv = moveTrafo |> Mod.map (fun t -> t.Inverse)

    let finalHandTrafo = Mod.map2 (*) trafo moveTrafoInv

    let controllerBox = 
        Sg.box (Mod.constant C4b.Green) (Mod.constant <| Box3d.FromCenterAndSize(V3d.OOO,V3d.III))
            |> Sg.scale 0.1
            |> Sg.trafo finalHandTrafo

    let beam = 
        Sg.lines (Mod.constant C4b.Red) (finalHandTrafo |> Mod.map (fun d -> 
                let origin = d.Forward.TransformPos(V3d.OOO)
                let target = origin + d.Forward.TransformDir(-V3d.OOI) * 100.0
                [| Line3d(origin,target) |]) 
        ) 

    let lines =
        Sg.ofList [
            Sg.lines (Mod.constant C4b.Red) (Mod.constant [|Line3d(V3d.OOO, V3d.IOO)|])
            Sg.lines (Mod.constant C4b.Green) (Mod.constant [|Line3d(V3d.OOO, V3d.OIO)|])
            Sg.lines (Mod.constant C4b.Blue) (Mod.constant [|Line3d(V3d.OOO, V3d.OOI)|])
            beam
        ]

    let debugStuff =
        Sg.ofList [
            lines
                |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.vertexColor |> toEffect
                ]
            
            controllerBox
                |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.constantColor C4f.White |> toEffect
                    DefaultSurfaces.simpleLighting |> toEffect
                ]
        ]

    let initial = CameraView.lookAt V3d.Zero V3d.IOO V3d.OOI
    let camera = DefaultCameraController.control win.Mouse win.Keyboard win.Time initial



    let scene = 
        [ for x in -5 .. 5 do
            for y in -5 .. 5 do
                for z in -5 .. 5 do
                    if x <> 0 && y <> 0 && z <> 0 then
                        yield Sg.translate (2.0 * float x) (2.0 * float y) (2.0 * float z) box        
            yield debugStuff
        ] 
            |> Sg.ofList
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.simpleLighting |> toEffect
            ]
    
    let bla =
        Sg.box (Mod.constant C4b.Green) (Mod.constant <| Box3d.FromMinAndSize(V3d.Zero,V3d.III))
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
            ]

    let trafo = Mod.map2 (*) moveTrafo win.View

    let scene = 
        Lod.scene trafo win app.Runtime
            |> Sg.andAlso debugStuff

    let models =
        [
            @"C:\Aardwork\Sponza bunt\sponza_cm.obj", Trafo3d.Scale 0.01
            @"C:\Aardwork\witcher\geralt.obj", Trafo3d.Translation(0.0, 0.0, 1.0)
            @"C:\Aardwork\ironman\ironman.obj", Trafo3d.Scale 0.5 * Trafo3d.Translation(2.0, 0.0, 0.0)
            @"C:\Aardwork\Stormtrooper\Stormtrooper.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0)
            @"C:\Aardwork\lara\lara.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-4.0, 0.0, 0.0)
        ]

    
    let scene =
        let flip = Trafo3d.FromBasis(V3d.IOO, V3d.OOI, -V3d.OIO, V3d.Zero)

        models
            |> List.map (fun (file, trafo) -> file |> Loader.Assimp.load |> Sg.AdapterNode |> Sg.transform (flip * trafo))
            |> Sg.ofList
            |> Sg.andAlso debugStuff
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.diffuseTexture |> toEffect
                DefaultSurfaces.normalMap |> toEffect
                DefaultSurfaces.lighting false |> toEffect
            ]


    let sg = 
        scene
            //|> Sg.trafo moveTrafo
            |> Sg.viewTrafo trafo //win.View
            //|> Sg.viewTrafo (Mod.constant Trafo3d.Identity)
            |> Sg.projTrafo win.Projection

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.RenderTask <- task
    win.Run()

    //printfn "%A" hasDevice
    OpenVR.Shutdown()
    0 // return an integer exit code
