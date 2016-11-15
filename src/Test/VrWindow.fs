namespace Aardvark.VR

open System
open System.Threading

open OpenTK.Graphics
open OpenTK.Graphics.OpenGL4

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.Rendering.GL

module VrWindow =
    open VrConversions
    open VrDriver
    
    type VrEye =
        | Left = 1
        | Right = 2

    type BlitWindow(b : ref<Framebuffer>, c : ref<Framebuffer>) =
        inherit OpenTK.GameWindow(
            1024, 768,
            GraphicsMode(
                ColorFormat(Config.BitsPerPixel), 
                0, 
                0, 
                1, 
                OpenTK.Graphics.ColorFormat(8,8,8,8),
                Config.Buffers, 
                false
            ),
            "Aardvark rocks \\o/",
            OpenTK.GameWindowFlags.Default,
            OpenTK.DisplayDevice.Default,

            Config.MajorVersion, 
            Config.MinorVersion, 
            Config.ContextFlags
        )

        let check() =
            let err = GL.GetError()
            if err <> ErrorCode.NoError then printfn "err: %A" err

        let mutable ctx = Unchecked.defaultof<_>
        let mutable loaded = false

        override x.OnLoad(e) =
            base.OnLoad(e)
            ctx <- ContextHandle(x.Context, x.WindowInfo)
            ContextHandle.Current <- Some ctx
            loaded <- true
            
        override x.OnRenderFrame(e) =
            if loaded then
                base.OnRenderFrame(e)
                let fbo = !c
                GL.BindFramebuffer(FramebufferTarget.ReadFramebuffer, fbo.Handle)
                check()
                GL.BindFramebuffer(FramebufferTarget.DrawFramebuffer, 0)
                check()
                GL.ReadBuffer(ReadBufferMode.ColorAttachment0)
                GL.DrawBuffer(DrawBufferMode.Back)

                GL.BlitFramebuffer(0, 0, fbo.Size.X, fbo.Size.Y, 0, 0, x.Width, x.Height, ClearBufferMask.ColorBufferBit, BlitFramebufferFilter.Nearest)
                check()
                GL.BindFramebuffer(FramebufferTarget.ReadFramebuffer, 0)
                check()
                GL.BindFramebuffer(FramebufferTarget.DrawFramebuffer, 0)
                check()
                x.SwapBuffers()
                lock b (fun () -> Fun.Swap(&b.contents, &c.contents))

    type ScreenWindow =
        {
            win : IRenderWindow
            fbo : IFramebuffer
        }

    type VrWindow(runtime : Runtime, createWindow : bool) =
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
        
        let screenA, screenB, screenC =
            if createWindow then
                let aTex  = runtime.CreateTexture(V2i(1024, 768), TextureFormat.Rgba8, 1,1,1)
                let bTex  = runtime.CreateTexture(V2i(1024, 768), TextureFormat.Rgba8, 1,1,1)
                let cTex  = runtime.CreateTexture(V2i(1024, 768), TextureFormat.Rgba8, 1,1,1)
                let depth   = runtime.CreateRenderbuffer(V2i(1024, 768), RenderbufferFormat.Depth24Stencil8, 1)
                let aFbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, { texture = aTex; slice = 0; level = 0 } :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])
                let bFbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, { texture = bTex; slice = 0; level = 0 } :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])
                let cFbo = runtime.CreateFramebuffer(signature, [DefaultSemantic.Colors, { texture = cTex; slice = 0; level = 0 } :> IFramebufferOutput; DefaultSemantic.Depth, depth :> IFramebufferOutput])

                (aFbo |> unbox<Framebuffer> |> ref, bFbo |> unbox<Framebuffer> |> ref, cFbo |> unbox<Framebuffer> |> ref)
            else
                Unchecked.defaultof<_>

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
        let watch = System.Diagnostics.Stopwatch()

        let ctx = runtime.Context

        let defaultFramebuffer = 
            new Framebuffer(
                ctx, signature, 
                (fun _ -> 0), 
                ignore, 
                [0, DefaultSemantic.Colors, Renderbuffer(ctx, 0, V2i.Zero, RenderbufferFormat.Rgba8, 1, 0L) :> IFramebufferOutput], None
            )

        do if createWindow then
                async {
                    do! Async.SwitchToNewThread()
                    let win = new BlitWindow(screenB, screenC)
                    win.Run(10.0, 30.0)
                } |> Async.Start

        let renderBothEyes =
            Mod.custom (fun self ->
                let lastFrameTook = watch.Elapsed.TotalSeconds
                watch.Restart()
                let timeRemaining = OpenVR.Compositor.GetFrameTimeRemaining() |> float
                //printfn "rem: %A" timeRemaining

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

                    if timeRemaining > lastFrameTook then    
                        Thread.Sleep(1000.0 * (timeRemaining - lastFrameTook) |> int)
                    else
                        printfn "long frame; %Ams %Ams" (lastFrameTook  * 1000.0) (timeRemaining * 1000.0)

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
            use token = ctx.RenderingLock renderCtx

            let mutable evt = VREvent_t()
            if not (system.PollNextEvent(&evt, sizeof<VREvent_t> |> uint32)) then
                evt.trackedDeviceIndex <- 0xFFFFFFFFu

            let mutable renderPose = TrackedDevicePose_t()
            let mutable gamePose = TrackedDevicePose_t()
            
            compositor.WaitGetPoses(renderPoses,gamePoses) |> VrDriver.check
            
            inputDevices.hmd.Update (renderPoses.[assignedInputs.hmdId])
            inputDevices.controller1.Update (renderPoses.[assignedInputs.controller1Id])
            inputDevices.controller2.Update (renderPoses.[assignedInputs.controller2Id])
            inputDevices.cam1.Update (renderPoses.[assignedInputs.cam1Id])
            inputDevices.cam1.Update (renderPoses.[assignedInputs.cam1Id])

            let dt = frameWatch.Elapsed
            frameWatch.Restart()
            update dt (renderPoses |> Array.map (fun p -> p.mDeviceToAbsoluteTracking.Trafo)) evt

            renderBothEyes.OutOfDate <- true
            renderBothEyes.GetValue()

            if createWindow then
                let fbo = !screenA
                let proj = Frustum.perspective 60.0 0.1 100.0 (float fbo.Size.X / float fbo.Size.Y) |> Frustum.projTrafo
                transact(fun () -> projection.Value <- proj)
                clear.Run(fbo) |> ignore
                task.Run(fbo) |> ignore
                lock screenB (fun () -> Fun.Swap(&screenA.contents, &screenB.contents))

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


