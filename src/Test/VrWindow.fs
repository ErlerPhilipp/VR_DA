﻿namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.Rendering.GL
open Aardvark.SceneGraph
open Aardvark.VR

[<AutoOpen>]
module NewVrStuff =

    open System
    open System.Threading
    open Aardvark.Rendering.GL
    open OpenTK.Graphics
    open OpenTK.Graphics.OpenGL4

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
        
        let ctx = runtime.Context

        let defaultFramebuffer = 
            new Framebuffer(
                ctx, signature, 
                (fun _ -> 0), 
                ignore, 
                [0, DefaultSemantic.Colors, Renderbuffer(ctx, 0, V2i.Zero, RenderbufferFormat.Rgba8, 1, 0L) :> IFramebufferOutput], None
            )

        let screenWindow =
            if createWindow then
                let f = new System.Windows.Forms.Form(Text = "Aardvark rocks \\o/", Width = 1024, Height = 768)
                let ctrl = 
                    new OpenTK.GLControl(
                        GraphicsMode(
                            ColorFormat(Config.BitsPerPixel), 
                            Config.DepthBits, 
                            Config.StencilBits, 
                            1, 
                            OpenTK.Graphics.ColorFormat.Empty,
                            Config.Buffers, 
                            false
                        ), 
                        Config.MajorVersion, 
                        Config.MinorVersion, 
                        Config.ContextFlags, 
                        VSync = false
                    )
                ctrl.Dock <- System.Windows.Forms.DockStyle.Fill
                f.Controls.Add ctrl

                f.Show()

                Some (ctrl, ContextHandle(ctrl.Context, ctrl.WindowInfo))
            else
                None 

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


            match screenWindow with
                | Some (win, handle) ->
                    use token = ctx.RenderingLock handle

                    let proj = Frustum.perspective 60.0 0.1 100.0 (float win.Width / float win.Height) |> Frustum.projTrafo
                            
                    transact(fun () -> projection.Value <- proj)
                    GL.Viewport(0, 0, win.Width, win.Height)
                    GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
                    GL.ClearDepth(1.0)
                    GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
                    defaultFramebuffer.Size <- V2i(win.Width, win.Height)

                    task.Run(defaultFramebuffer) |> ignore
                    win.SwapBuffers()
                    System.Windows.Forms.Application.DoEvents()

                | None ->
                    ()


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


