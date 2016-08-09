// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.Rendering.GL
open Aardvark.Application.WinForms
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

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

type VrDevice(system : CVRSystem, deviceType : VrDeviceType, index : int) =
    
    let getString (prop : ETrackedDeviceProperty) =
        let builder = System.Text.StringBuilder(4096, 4096)
        let mutable err = ETrackedPropertyError.TrackedProp_Success
        let len = system.GetStringTrackedDeviceProperty(uint32 index, prop, builder, uint32 builder.Capacity, &err)
        builder.ToString()

    let vendor  = lazy ( getString ETrackedDeviceProperty.Prop_ManufacturerName_String )
    let model   = lazy ( getString ETrackedDeviceProperty.Prop_ModelNumber_String )
    
    member x.Type = deviceType
    member x.Index = index
    member x.Vendor = vendor.Value
    member x.Model = model.Value


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

    let signature =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = samples }
            DefaultSemantic.Depth,  { format = RenderbufferFormat.Depth24Stencil8; samples = samples}
        ]


    let lTex = runtime.CreateTexture(size, TextureFormat.Rgba8, 1, 1, 1)
    let rTex = runtime.CreateTexture(size, TextureFormat.Rgba8, 1, 1, 1)

    let depth = runtime.CreateRenderbuffer(size, RenderbufferFormat.Depth24Stencil8, samples)
    let color = runtime.CreateRenderbuffer(size, RenderbufferFormat.Rgba8, samples)
    let fbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, color :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])

    let view = Mod.init Trafo3d.Identity
    let proj = Mod.init Trafo3d.Identity

    let renderPoses = Array.zeroCreate 16
    let gamePoses = Array.zeroCreate 16

    let hmd = VrDriver.devices |> Array.find (fun d -> d.Type = VrDeviceType.Hmd)
    let clear = runtime.CompileClear(signature, Mod.constant C4f.Black, Mod.constant 1.0)

    abstract member RenderFrame : fbo : IFramebuffer * eye : VrEye -> unit
    default x.RenderFrame(_,_) = ()

    member x.FramebufferSignature = signature
    member x.View = view :> IMod<_>
    member x.Projection = proj :> IMod<_>

    member x.Run() =
        let mutable evt = Unchecked.defaultof<_>
        use t = runtime.ContextLock
        compositor.CompositorBringToFront()
        let lHeadToEye = system.GetEyeToHeadTransform(EVREye.Eye_Left).Trafo.Inverse
        let rHeadToEye = system.GetEyeToHeadTransform(EVREye.Eye_Right).Trafo.Inverse

        while true do
            if system.PollNextEvent(&evt, sizeof<VREvent_t> |> uint32) then
                let eType = evt.eventType |> int |> unbox<EVREventType>
                let eName = eType |> system.GetEventTypeNameFromEnum
                printfn "%A" eName

  
            compositor.WaitGetPoses(renderPoses,gamePoses) |> VrDriver.check

            let pose = renderPoses.[hmd.Index]
            
            let flip = Trafo3d.FromBasis(V3d.IOO, -V3d.OOI, V3d.OIO, V3d.Zero)
            let viewTrafo = 
                pose.mDeviceToAbsoluteTracking.Trafo.Inverse

            let lProj = system.GetProjectionMatrix(EVREye.Eye_Left, 0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo
            let rProj = system.GetProjectionMatrix(EVREye.Eye_Right,0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo

            //using runtime.ContextLock (fun _ ->
            // render left
            clear.Run(fbo) |> ignore
            transact(fun () -> 
                view.Value <- flip * viewTrafo * lHeadToEye
                proj.Value <- lProj
            )
            x.RenderFrame(fbo, VrEye.Left)
            //runtime.ResolveMultisamples(color, lTex, ImageTrafo.Rot0)
            let mutable leftTex = Texture_t(eColorSpace = EColorSpace.Auto, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint (unbox<int> color.Handle))
            let mutable leftBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
            compositor.Submit(EVREye.Eye_Left, &leftTex, &leftBounds, EVRSubmitFlags.Submit_GlRenderBuffer) |> VrDriver.check
        

            // render right
            clear.Run(fbo) |> ignore
            transact(fun () -> 
                view.Value <- flip * viewTrafo * rHeadToEye
                proj.Value <- rProj
            )
            x.RenderFrame(fbo, VrEye.Right)
            //runtime.ResolveMultisamples(color, rTex, ImageTrafo.Rot0)
            let mutable rightTex = Texture_t(eColorSpace = EColorSpace.Auto, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint (unbox<int> color.Handle))
            let mutable rightBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
            compositor.Submit(EVREye.Eye_Right, &rightTex, &rightBounds, EVRSubmitFlags.Submit_GlRenderBuffer) |> VrDriver.check
            //)
type VrRenderWindow(runtime : IRuntime, samples : int) =
    inherit VrWindow(runtime, samples)

    let mutable task = RenderTask.empty

    override x.RenderFrame(fbo, eye) =
        task.Run(fbo) |> ignore

    member x.RenderTask
        with get() = task
        and set t = task <- t





let run() =
   
    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication()

    for d in VrDriver.devices do
        Log.start "device %d" d.Index
        Log.line "type:   %A" d.Type
        Log.line "vendor: %s" d.Vendor
        Log.line "model:  %s" d.Model
        Log.stop()

//    let win = VrRenderWindow(app.Runtime, 1)


    let rt = app.Runtime


    let mutable err = EVRInitError.None
    let system = OpenVR.Init(&err, EVRApplicationType.VRApplication_Scene)
 
    let hasDevice = OpenVR.IsHmdPresent()
    let compositor = OpenVR.Compositor

    let mutable width = 0u
    let mutable height = 0u
    system.GetRecommendedRenderTargetSize(&width,&height)

    let s = V2i(int width,int height)
    printfn "%A" s
    
    

    let start = OpenVR.k_unTrackedDeviceIndex_Hmd + 1u
    let cnt = OpenVR.k_unMaxTrackedDeviceCount

    let proj = Mod.init Trafo3d.Identity
    let view = CameraView.lookAt (V3d.III * 4.0) V3d.Zero V3d.OOI |> CameraView.viewTrafo |> Mod.init
  
    let box = Sg.box' C4b.Red Box3d.Unit
    
    let cross =
        Sg.ofList [
            Sg.lines (Mod.constant C4b.Red) (Mod.constant [|Line3d(V3d.OOO, V3d.IOO)|])
            Sg.lines (Mod.constant C4b.Green) (Mod.constant [|Line3d(V3d.OOO, V3d.OIO)|])
            Sg.lines (Mod.constant C4b.Blue) (Mod.constant [|Line3d(V3d.OOO, V3d.OOI)|])
        ]

    let sg = 
        [ for x in -5 .. 5 do
            for y in -5 .. 5 do
                for z in -5 .. 5 do
                    if x <> 0 && y <> 0 && z <> 0 then
                        yield Sg.translate (2.0 * float x) (2.0 * float y) (2.0 * float z) box        
            yield 
                cross
                    |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.vertexColor |> toEffect
                    ]
        ] 
            |> Sg.ofList
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.simpleLighting |> toEffect
            ]
            |> Sg.viewTrafo view
            |> Sg.projTrafo proj

//    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
//    win.RenderTask <- task
//    win.Run()

    

    let signature =
        rt.CreateFramebufferSignature(
            1, 
            [
                DefaultSemantic.Colors, RenderbufferFormat.Rgba8
                DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
            ]
        )

    let depth = rt.CreateRenderbuffer(s, RenderbufferFormat.Depth24Stencil8, 1)
    let leftColor  = rt.CreateTexture(s,TextureFormat.Rgba8,1,1,1)
    let rightColor = rt.CreateTexture(s,TextureFormat.Rgba8,1,1,1)

    let leftFbo = 
        rt.CreateFramebuffer(
            signature, 
            [
                DefaultSemantic.Colors, { texture = leftColor; slice = 0; level = 0 } :> IFramebufferOutput
                DefaultSemantic.Depth, depth :> IFramebufferOutput
            ]
        )

    let rightFbo = 
        rt.CreateFramebuffer(
            signature, 
            [
                DefaultSemantic.Colors, { texture = rightColor; slice = 0; level = 0 } :> IFramebufferOutput
                DefaultSemantic.Depth, depth :> IFramebufferOutput
            ]
        )

    let render = app.Runtime.CompileRender(signature, sg)
    let clear = app.Runtime.CompileClear(signature, Mod.constant C4f.Black, Mod.constant 1.0)

    let task = RenderTask.ofList [ clear; render ]


    let deviceId =
        { start .. cnt - 1u }
            |> Seq.tryPick (fun i ->
                let isConnected = system.IsTrackedDeviceConnected(i)
                if isConnected then Some i
                else None
            )


    for i in start .. cnt - 1u do
        let isConnected = system.IsTrackedDeviceConnected(i)
        ()


    let toTrafo (m : HmdMatrix44_t) =
        let t = M44f(m.m0,m.m1,m.m2,m.m3,m.m4,m.m5,m.m6,m.m7,m.m8,m.m9,m.m10,m.m11,m.m12,m.m13,m.m14,m.m15) 
        let t = M44d.op_Explicit(t)
        Trafo3d(t,t.Inverse)

    let toTrafo34 (m : HmdMatrix34_t) =
        let t = M44f(m.m0,m.m1,m.m2, 0.0f,m.m3,m.m4,m.m5, 0.0f,m.m6,m.m7,m.m8, 0.0f,m.m9,m.m10,m.m11, 1.0f)
        let t = M44d.op_Explicit(t).Transposed
        Trafo3d(t.Inverse,t)


    
    
    use asdf = app.Context.ResourceLock

    let check err =
        if err <> EVRCompositorError.None then
            failwith ""

    compositor.CompositorBringToFront()
    let mutable evnt = Unchecked.defaultof<_>

    let leftEye     = system.GetEyeToHeadTransform(EVREye.Eye_Left).Trafo.Inverse
    let rightEye    = system.GetEyeToHeadTransform(EVREye.Eye_Right).Trafo.Inverse

    while true do
        if system.PollNextEvent(&evnt, sizeof<VREvent_t> |> uint32) then
            let eType = evnt.eventType |> int |> unbox<EVREventType>
            let eName = eType |> system.GetEventTypeNameFromEnum
            printfn "%A" eName

        let poses = Array.zeroCreate 16
        let ur = Array.zeroCreate 16
        compositor.WaitGetPoses(poses,ur) |> check

        let viewTrafo = 
            Trafo3d.FromBasis(V3d.IOO, -V3d.OOI, V3d.OIO, V3d.Zero) * 
            poses.[0].mDeviceToAbsoluteTracking.Trafo


        let leftProj  = system.GetProjectionMatrix(EVREye.Eye_Left, 0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo
        let rightProj = system.GetProjectionMatrix(EVREye.Eye_Right,0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo

        transact(fun () -> 
            view.Value <- viewTrafo * leftEye
            proj.Value <- leftProj
        )
        task.Run(leftFbo) |> ignore
        let mutable leftTex = Texture_t(eColorSpace = EColorSpace.Auto, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint leftColor.Handle)
        let mutable leftBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
        compositor.Submit(EVREye.Eye_Left, &leftTex, &leftBounds, EVRSubmitFlags.Submit_Default) |> check
        

        transact(fun () -> 
            view.Value <- viewTrafo * leftEye
            proj.Value <- rightProj
        )
        task.Run(rightFbo) |> ignore
        let mutable rightTex = Texture_t(eColorSpace = EColorSpace.Auto, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint rightColor.Handle)
        let mutable rightBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
        compositor.Submit(EVREye.Eye_Right, &rightTex, &rightBounds, EVRSubmitFlags.Submit_Default) |> check


    //system.GetStringTrackedDeviceProperty(0u, ETrackedDeviceProperty.Prop_TrackingSystemName_String, )


    //printfn "%A" hasDevice
    OpenVR.Shutdown()


[<EntryPoint>]
let main argv =
    
    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication()
    let win = VrRenderWindow(app.Runtime, 16)

    let box = Sg.box' C4b.Red Box3d.Unit
    
    let cross =
        Sg.ofList [
            Sg.lines (Mod.constant C4b.Red) (Mod.constant [|Line3d(V3d.OOO, V3d.IOO)|])
            Sg.lines (Mod.constant C4b.Green) (Mod.constant [|Line3d(V3d.OOO, V3d.OIO)|])
            Sg.lines (Mod.constant C4b.Blue) (Mod.constant [|Line3d(V3d.OOO, V3d.OOI)|])
        ]

    let sg = 
        [ for x in -5 .. 5 do
            for y in -5 .. 5 do
                for z in -5 .. 5 do
                    if x <> 0 && y <> 0 && z <> 0 then
                        yield Sg.translate (2.0 * float x) (2.0 * float y) (2.0 * float z) box        
            yield 
                cross
                    |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.vertexColor |> toEffect
                    ]
        ] 
            |> Sg.ofList
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.simpleLighting |> toEffect
            ]
            |> Sg.viewTrafo win.View
            |> Sg.projTrafo win.Projection

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.RenderTask <- task
    win.Run()

    //printfn "%A" hasDevice
    OpenVR.Shutdown()
    0 // return an integer exit code
