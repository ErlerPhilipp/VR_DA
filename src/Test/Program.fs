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

[<EntryPoint>]
let main argv =
    
    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication()

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
    
    let sg = 
        [ for x in -5 .. 5 do
            for y in -5 .. 5 do
                for z in -5 .. 5 do
                    yield Sg.translate (2.0 * float x) (2.0 * float y) (2.0 * float z) box        
        ] 
            |> Sg.ofList
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.simpleLighting |> toEffect
            ]
            |> Sg.viewTrafo view
            |> Sg.projTrafo proj

    

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


//    let deviceId =
//        { start .. cnt - 1u }
//            |> Seq.tryPick (fun i ->
//                let isConnected = system.IsTrackedDeviceConnected(i)
//                if isConnected then Some i
//                else None
//            )
//
//
//    for i in start .. cnt - 1u do
//        let isConnected = system.IsTrackedDeviceConnected(i)
//        ()


    let toTrafo (m : HmdMatrix44_t) =
        let t = M44f(m.m0,m.m1,m.m2,m.m3,m.m4,m.m5,m.m6,m.m7,m.m8,m.m9,m.m10,m.m11,m.m12,m.m13,m.m14,m.m15) 
        let t = M44d.op_Explicit(t)
        Trafo3d(t,t.Inverse)

    let toTrafo34 (m : HmdMatrix34_t) =
        let t = M44f(m.m0,m.m1,m.m2, 0.0f,m.m3,m.m4,m.m5, 0.0f,m.m6,m.m7,m.m8, 0.0f,m.m9,m.m10,m.m11, 1.0f)
        let t = M44d.op_Explicit(t).Transposed
        Trafo3d(t.Inverse,t)


    let mutable leftTex = Texture_t()
    let mutable leftBounds = VRTextureBounds_t()

    leftTex.eColorSpace <- EColorSpace.Gamma
    leftTex.eType <- EGraphicsAPIConvention.API_OpenGL
    leftTex.handle <- nativeint 0
    
    leftBounds.uMax <- 1.0f
    leftBounds.vMax <- 1.0f

    use asdf = app.Context.ResourceLock

    let check err =
        if err <> EVRCompositorError.None then
            failwith ""

    compositor.CompositorBringToFront()
    let mutable evnt = Unchecked.defaultof<_>

    let leftEye = (system.GetEyeToHeadTransform(EVREye.Eye_Left) |> toTrafo34).Inverse
    let rightEye = (system.GetEyeToHeadTransform(EVREye.Eye_Right) |> toTrafo34).Inverse

    let trans (t : Trafo3d) = Trafo3d(t.Forward.Transposed, t.Backward.Transposed)


    while true do
        if system.PollNextEvent(&evnt, sizeof<VREvent_t> |> uint32) then
            printfn "%A" evnt.eventType

        let poses = Array.zeroCreate 16
        let ur = Array.zeroCreate 16
        compositor.WaitGetPoses(poses,ur) |> check

        let trafo = Trafo3d.Identity //Trafo3d.FromBasis(V3d.IOO, -V3d.OOI, V3d.OIO, V3d.Zero)

        let mat = Trafo3d.Identity //poses.[0].mDeviceToAbsoluteTracking |> toTrafo34


        let leftMat  = leftEye *  ( system.GetProjectionMatrix(EVREye.Eye_Left, 0.1f,10.0f, EGraphicsAPIConvention.API_OpenGL) |> toTrafo)
        let rightMat = rightEye * (system.GetProjectionMatrix(EVREye.Eye_Right,0.1f,10.0f, EGraphicsAPIConvention.API_OpenGL) |> toTrafo)

        //printfn "%A" (system.GetEyeToHeadTransform(EVREye.Eye_Left) |> toTrafo34)
        transact(fun () -> view.Value <- trafo * mat)
        transact(fun () -> proj.Value <- leftMat)
        task.Run(leftFbo) |> ignore

        transact(fun () -> proj.Value <- rightMat)
        task.Run(rightFbo) |> ignore



        leftTex.handle <- leftColor.Handle |> nativeint    
        compositor.Submit(EVREye.Eye_Left, &leftTex, &leftBounds, EVRSubmitFlags.Submit_Default) |> check
        leftTex.handle <- rightColor.Handle |> nativeint    
        compositor.Submit(EVREye.Eye_Right, &leftTex, &leftBounds, EVRSubmitFlags.Submit_Default) |> check


    //system.GetStringTrackedDeviceProperty(0u, ETrackedDeviceProperty.Prop_TrackingSystemName_String, )


    printfn "%A" hasDevice
    OpenVR.Shutdown()
    0 // return an integer exit code
