namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.Rendering.GL
open Aardvark.SceneGraph
open Aardvark.VR


module NewVrStuff =
    open System
    open System.Threading
    open Aardvark.Rendering.GL

    type VrWindow(runtime : Runtime) =
        let system = VrDriver.system
        let size = VrDriver.desiredSize

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba8; samples = 1 }
                DefaultSemantic.Depth,  { format = RenderbufferFormat.Depth24Stencil8; samples = 1}
            ]

        let depth   = runtime.CreateRenderbuffer(size, RenderbufferFormat.Depth24Stencil8, 1)
        let lColor  = runtime.CreateTexture(size, TextureFormat.Rgba8, 1,1,1)
        let rColor  = runtime.CreateTexture(size, TextureFormat.Rgba8, 1,1,1)
        let lFbo    = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, { texture = lColor; slice = 0; level = 0 } :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])
        let rFbo    = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, { texture = rColor; slice = 0; level = 0 } :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])
        
        let renderCtx       = ContextHandle.create()
        let cancel          = new CancellationTokenSource()
        let ct              = cancel.Token
        let renderPoses     = Array.zeroCreate VrDriver.devices.Length
        let gamePoses       = Array.zeroCreate VrDriver.devices.Length

        let mutable task    = RenderTask.empty
        let clear           = runtime.CompileClear(signature, Mod.constant C4f.Black, Mod.constant 1.0)
        
        let projection      = Mod.init Trafo3d.Identity
        let lProj           = system.GetProjectionMatrix(EVREye.Eye_Left, 0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo
        let rProj           = system.GetProjectionMatrix(EVREye.Eye_Right,0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo
        let time            = Mod.init DateTime.Now

        let mutable update : TimeSpan -> Trafo3d[] -> VREvent_t -> unit = fun _ _ _ -> ()
        let frameWatch = System.Diagnostics.Stopwatch()

        let renderBothEyes =
            Mod.custom (fun self ->
                let lHeadToEye = system.GetEyeToHeadTransform(EVREye.Eye_Left).Trafo.Inverse
                let rHeadToEye = system.GetEyeToHeadTransform(EVREye.Eye_Right).Trafo.Inverse

                task.Use (fun () ->
                            
                    // render left
                    clear.Run(lFbo) |> ignore
                    transact(fun () -> projection.Value <- lHeadToEye * lProj)
                    task.Run(self, OutputDescription.ofFramebuffer lFbo) |> ignore
                    OpenTK.Graphics.OpenGL4.GL.Flush()
                    OpenTK.Graphics.OpenGL4.GL.Finish()


                    // render right
                    clear.Run(rFbo) |> ignore
                    transact(fun () -> projection.Value <- rHeadToEye * rProj)
                    task.Run(self, OutputDescription.ofFramebuffer rFbo) |> ignore
                    OpenTK.Graphics.OpenGL4.GL.Flush()
                    OpenTK.Graphics.OpenGL4.GL.Finish()

                    do
                        let mutable leftTex = Texture_t(eColorSpace = EColorSpace.Gamma, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint (unbox<int> lColor.Handle))
                        let mutable leftBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
                        OpenVR.Compositor.Submit(EVREye.Eye_Left, &leftTex, &leftBounds, EVRSubmitFlags.Submit_Default) |> VrDriver.check

                    do 
                        let mutable rightTex = Texture_t(eColorSpace = EColorSpace.Gamma, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint (unbox<int> rColor.Handle))
                        let mutable rightBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
                        OpenVR.Compositor.Submit(EVREye.Eye_Right, &rightTex, &rightBounds, EVRSubmitFlags.Submit_Default) |> VrDriver.check
                
                ) 
            )

        member x.Update 
            with get() = update
            and set u = update <- u

        member private x.RenderFrame() =
            let ctx = runtime.Context
            use token = ctx.RenderingLock renderCtx

            let mutable evt = VREvent_t()
            if not (system.PollNextEvent(&evt, sizeof<VREvent_t> |> uint32)) then
                evt.trackedDeviceIndex <- 0xFFFFFFFFu

            OpenVR.Compositor.WaitGetPoses(renderPoses,gamePoses) |> VrDriver.check
            let dt = frameWatch.Elapsed
            frameWatch.Restart()
            update dt (renderPoses |> Array.map (fun p -> p.mDeviceToAbsoluteTracking.Trafo)) evt

            renderBothEyes.OutOfDate <- true
            renderBothEyes.GetValue()
            transact (fun () -> time.Value <- DateTime.Now)

        

        member x.Projection = projection
        member x.FramebufferSignature = signature
        member x.Runtime = runtime :> IRuntime
        member x.Time = time :> IMod<_>
        member x.RenderTask
            with get() = task
            and set t = task <- t
        member x.Sizes = Mod.constant size
        member x.Samples = 1


        interface IRenderTarget with
            member x.FramebufferSignature = signature
            member x.Runtime = runtime :> IRuntime
            member x.Time = time :> IMod<_>
            member x.RenderTask
                with get() = task
                and set t = task <- t
            member x.Sizes = Mod.constant size
            member x.Samples = 1

        interface IRenderControl with
            member x.Mouse = failwith ""
            member x.Keyboard = failwith ""


        member x.Run() =
            let mutable evt = Unchecked.defaultof<_>
            OpenVR.Compositor.CompositorBringToFront()
            while true do
                x.RenderFrame()


[<AutoOpen>]
module VrWindow =

    type VrWindow(runtime : IRuntime, samples : int) =
        let system = VrDriver.system
    //    let compositor = OpenVR.Compositor

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
        let lColor = runtime.CreateTexture(size, TextureFormat.Rgba8, 1,1,1)
        let rColor = runtime.CreateTexture(size, TextureFormat.Rgba8, 1,1,1)
        let lFbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, { texture = lColor; slice = 0; level = 0 } :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])
        let rFbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, { texture = rColor; slice = 0; level = 0 } :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])


        let proj = Mod.init Trafo3d.Identity
        let view = Mod.init Trafo3d.Identity
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
        member x.Projection = proj :> IMod<_>
        member x.View = view

        member x.LodView = hmd.WorldToDevice
        member x.LodProj = Frustum.perspective 110.0 0.1 100.0 1.0

//        member option x.ConrtrollerHandle : (fun unit -> unit) = none

        member x.RenderTask
            with get() = renderTask
            and set t = renderTask <- t

        member x.Run() =

            let mutable evt = Unchecked.defaultof<_>
            OpenVR.Compositor.CompositorBringToFront()


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

            let cts = new System.Threading.CancellationTokenSource()

            let run () = 

                do 
                    try
                        let renderCtx = ContextHandle.create()
                        let runtime = runtime |> unbox<Aardvark.Rendering.GL.Runtime>
                        let ctx = runtime.Context
                        use token = ctx.RenderingLock renderCtx

                        while not cts.IsCancellationRequested do

                            if not (system.PollNextEvent(&evt, sizeof<VREvent_t> |> uint32)) then
                                evt.trackedDeviceIndex <- 0xFFFFFFFFu

                            OpenVR.Compositor.WaitGetPoses(renderPoses,gamePoses) |> VrDriver.check


                            for d in VrDriver.devices do
                                d.Update(evt, renderPoses)

                            //let pose = renderPoses.[hmd.Index]
                            let viewTrafo = hmd.WorldToDevice.GetValue()


                            let lProj = system.GetProjectionMatrix(EVREye.Eye_Left, 0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo
                            let rProj = system.GetProjectionMatrix(EVREye.Eye_Right,0.1f,100.0f, EGraphicsAPIConvention.API_OpenGL).Trafo

                            let sw = System.Diagnostics.Stopwatch()


                            let renderTask = unbox<Aardvark.Rendering.GL.RenderTasks.RenderTask> renderTask

                            let bla =
                                Mod.custom (fun self ->
                                    transact (fun () -> view.Value <- hmd.WorldToDevice.GetValue())
                                    renderTask.Use (fun () ->
                                        let lHeadToEye = system.GetEyeToHeadTransform(EVREye.Eye_Left).Trafo.Inverse
                            
                                        sw.Restart()
                                        // render left
                                        clear.Run(lFbo) |> ignore
                                        transact(fun () -> 
                                            proj.Value <- lHeadToEye * lProj
                                        )
                                        renderTask.Run(self, OutputDescription.ofFramebuffer lFbo) |> ignore

                                        OpenTK.Graphics.OpenGL4.GL.Flush()
                                        OpenTK.Graphics.OpenGL4.GL.Finish()

                                        let rHeadToEye = system.GetEyeToHeadTransform(EVREye.Eye_Right).Trafo.Inverse

                                        // render right
                                        clear.Run(rFbo) |> ignore
                                        transact(fun () -> 
                                            proj.Value <- rHeadToEye * rProj
                                        )
                                        renderTask.Run(self, OutputDescription.ofFramebuffer rFbo) |> ignore


                                        //runtime.ResolveMultisamples(color, rTex, ImageTrafo.Rot0)
                                        let mutable rightTex = Texture_t(eColorSpace = EColorSpace.Gamma, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint (unbox<int> rColor.Handle))
                                        let mutable rightBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
                                        OpenVR.Compositor.Submit(EVREye.Eye_Right, &rightTex, &rightBounds, EVRSubmitFlags.Submit_Default) |> VrDriver.check

                                                                    
                                        OpenTK.Graphics.OpenGL4.GL.Flush()
                                        OpenTK.Graphics.OpenGL4.GL.Finish()
                                        //runtime.ResolveMultisamples(color, lTex, ImageTrafo.Rot0)
                                        let mutable leftTex = Texture_t(eColorSpace = EColorSpace.Gamma, eType = EGraphicsAPIConvention.API_OpenGL, handle = nativeint (unbox<int> lColor.Handle))
                                        let mutable leftBounds = VRTextureBounds_t(uMin = 0.0f, uMax = 1.0f, vMin = 0.0f, vMax = 1.0f)
                                        OpenVR.Compositor.Submit(EVREye.Eye_Left, &leftTex, &leftBounds, EVRSubmitFlags.Submit_Default) |> VrDriver.check


                                    )
                                )

                            bla.GetValue()
                                //OpenTK.Graphics.OpenGL4.GL.Flush()

                                //System.Threading.Thread.Sleep 20
        //                            OpenTK.Graphics.OpenGL4.GL.Flush()
        //                            OpenTK.Graphics.OpenGL4.GL.Finish()

                    
                            sw.Stop()
                            //printfn "time : %A" (1000.0 / float sw.Elapsed.TotalMilliseconds)

                            transact (fun () -> 
                                time.Value <- System.DateTime.Now
                            )
                    with e ->
                        Log.error "exn: %A" e

            let task = System.Threading.Tasks.Task.Factory.StartNew(run,cts.Token)
            System.Windows.Forms.Application.ApplicationExit.Add(fun _ -> 
                cts.Cancel()
            )
            System.Threading.Tasks.Task.Factory.StartNew(run) |> ignore

            let fsq =
                Sg.fullScreenQuad
                    |> Sg.depthTest (Mod.constant DepthTestMode.None)
                    |> Sg.diffuseTexture tex
                    |> Sg.effect [ DefaultSurfaces.diffuseTexture |> toEffect ]

            let fsqTask = runtime.CompileRender(signature, fsq)
            gameWindow.RenderTask <- fsqTask

//            while true do System.Threading.Thread.Sleep 10
            //gameWindow.Run()

    //            let runtime = runtime |> unbox<Aardvark.Rendering.GL.Runtime>
    //            let ctx = runtime.Context
    //            let handle = new ContextHandle(gameWindow.Context, gameWindow.WindowInfo)
    //            using ( ctx.RenderingLock(handle) ) (fun _ ->  
    //                OpenTK.Graphics.OpenGL4.GL.ClearColor(1.0f,0.0f,0.0f,1.0f)
    //                OpenTK.Graphics.OpenGL4.GL.Clear(OpenTK.Graphics.OpenGL4.ClearBufferMask.ColorBufferBit)
    //                gameWindow.SwapBuffers()
    //            )

