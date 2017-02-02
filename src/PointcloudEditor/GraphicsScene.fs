namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text
open Aardvark.SceneGraph

open OmnidirShadows

module GraphicsScene =
    open LogicalSceneTypes
    open GraphicsSceneTypes
    open OmnidirShadows
    open VrInteractions
    open VrWindow
    open InteractiveSegmentation.OctreeHelper

    type Conversion private() =
        static member Create(o : Object) =
            {
                original = o
                trafo = Mod.init o.trafo
                model = Mod.init o.model
                tilingFactor = Mod.init o.tilingFactor
                visible = Mod.init o.visible
            }

        static member Create(s : Scene) =
            let lightPos = getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects).Forward.TransformPos(V3d())
            let centroidTrafo = getTrafoOfFirstObjectWithId(s.specialObjectIds.centroidId, s.objects)
            {
                original            = s
                graphicsObjects     = CSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map Conversion.Create)
                viewTrafo           = Mod.init s.viewTrafo
                lightPos            = Mod.init lightPos
                lightColor          = Mod.init s.lightColor
                selVolPath          = Mod.init (Array.append s.interactionInfo1.selectionVolumePath s.interactionInfo2.selectionVolumePath)

                scoreTrafo          = Mod.init s.scoreTrafo
                scoreText           = Mod.init s.scoreText
                pointCloudTrafo     = Mod.init (s.pointCloudTrafo * centroidTrafo)
            }

        static member Update(mo : GraphicsObject, o : Object) =
            if not (System.Object.ReferenceEquals(mo.original, o)) then
                mo.original <- o
                mo.trafo.Value <- o.trafo
                mo.tilingFactor.Value <- o.tilingFactor
                mo.visible.Value <- o.visible

        static member Update(ms : GraphicsScene, s : Scene) =
            if not (System.Object.ReferenceEquals(ms.original, s)) then
                let lightPos = getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects).Forward.TransformPos(V3d())
                let centroidTrafo = getTrafoOfFirstObjectWithId(s.specialObjectIds.centroidId, s.objects)

                ms.original <- s
                ms.viewTrafo.Value <- s.viewTrafo
                ms.lightPos.Value <- lightPos
                ms.lightColor.Value <- s.lightColor
                ms.selVolPath.Value <- (Array.append s.interactionInfo1.selectionVolumePath s.interactionInfo2.selectionVolumePath)
                
                ms.scoreTrafo.Value <- s.scoreTrafo
                ms.scoreText.Value <- s.scoreText
                ms.pointCloudTrafo.Value <- (s.pointCloudTrafo * centroidTrafo)

                let table = 
                    ms.graphicsObjects |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                for t in PersistentHashSet.toSeq s.objects do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t)
                        | _ ->
                            let mo = Conversion.Create(t)
                            ms.graphicsObjects.Add mo |> ignore
                
                ms.graphicsObjects.ExceptWith table.Values
         
    let createScene (initialScene : Scene) (win : VrWindow) =
        let mutable scene = initialScene
        let graphicsScene = Conversion.Create initialScene

        let deviceCount = VrDriver.devices.Length
        let oldTrafos = Array.zeroCreate deviceCount
        let mutable oldTrackingToWorld = Trafo3d.Identity
        let update (dt : System.TimeSpan) (trafos : Trafo3d[]) (e : VREvent_t) =
            
            scene <- LogicalScene.update scene StartFrame

            let trackingToWorldHasChanged = oldTrackingToWorld <> scene.trackingToWorld
            oldTrackingToWorld <- scene.trackingToWorld

            for i in 0 .. VrDriver.devices.Length-1 do
                let t = trafos.[i]
                if oldTrafos.[i] <> t || trackingToWorldHasChanged then
                    oldTrafos.[i] <- t
                    scene <- LogicalScene.update scene (DeviceMove(i, t * scene.trackingToWorld))
                
            if e.trackedDeviceIndex >= 0u && e.trackedDeviceIndex < uint32 deviceCount then
                let deviceId = e.trackedDeviceIndex |> int
                let button = int e.data.controller.button |> unbox<EVRButtonId>
                let axis = button - EVRButtonId.k_EButton_Axis0 |> int
                let trafo = trafos.[deviceId]

                match unbox<EVREventType> (int e.eventType) with
                    | EVREventType.VREvent_ButtonPress -> scene <- LogicalScene.update scene (DevicePress(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonUnpress -> scene <- LogicalScene.update scene (DeviceRelease(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonTouch -> scene <- LogicalScene.update scene (DeviceTouch(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonUntouch -> scene <- LogicalScene.update scene (DeviceUntouch(deviceId, axis, trafo))
                    | _ -> () //printfn "%A" (e.eventType)
                    
            scene <- LogicalScene.update scene (TimeElapsed dt)
  
            scene <- LogicalScene.update scene EndFrame

            transact (fun () ->
                Conversion.Update(graphicsScene, scene)
            )
            ()

        win.Update <- update

        let sg = OmnidirShadows.init(win, graphicsScene)

//        let textSg =
////            Sg.text (new Font("Arial",FontStyle.Bold)) C4b.Red graphicsScene.scoreText :> ISg
//            Sg.markdown MarkdownConfig.light graphicsScene.scoreText
//                |> Sg.trafo graphicsScene.scoreTrafo
                
        // Stencil Mode for Addtivie Selection (AND, OR, XOR, SINGLE)
        let Additive = Rendering.StencilMode(
                                IsEnabled       = true,
                                CompareFront    = StencilFunction(Rendering.StencilCompareFunction.Always, 0x01, 0xFFu),
                                OperationFront  = StencilOperation(StencilOperationFunction.Keep, StencilOperationFunction.DecrementWrap, StencilOperationFunction.Keep),
                                CompareBack     = StencilFunction(Rendering.StencilCompareFunction.Always, 0x01, 0xFFu),
                                OperationBack   = StencilOperation(StencilOperationFunction.Keep, StencilOperationFunction.IncrementWrap, StencilOperationFunction.Keep)
                            ) 

        let selectionPath = 
            Sg.instancedGeometry (graphicsScene.selVolPath) (SelectionVolume.makeSelectionVolumeGeometry())
                |> Sg.surface (Mod.constant (SelectionVolume.makeSelectionVolumeSurfaceInstance(win)))
                |> Sg.blendMode(Mod.constant (BlendMode(true)))
                |> Sg.stencilMode (Mod.constant Additive)
                |> Sg.writeBuffers (Some (Set.singleton DefaultSemantic.Stencil))
                |> Sg.uniform "PointcloudToWorld" (graphicsScene.pointCloudTrafo)
                |> Sg.pass (Renderpasses.SelectionPass)
                
        let stencilModeHighLightOnes = Rendering.StencilMode(
                                        IsEnabled   = true,
                                        Compare     = Rendering.StencilFunction(Rendering.StencilCompareFunction.LessOrEqual, 1, 0xFFu),
                                        Operation   = Rendering.StencilOperation(Rendering.StencilOperationFunction.Keep, Rendering.StencilOperationFunction.Keep, Rendering.StencilOperationFunction.Keep)
                                   ) 
        let fullscreenQuad = 
            InteractiveSegmentation.OctreeHelper.fullscreenQuadSg
                |> Sg.trafo (Mod.constant Trafo3d.Identity)
                |> Sg.viewTrafo (Mod.constant Trafo3d.Identity)
                |> Sg.projTrafo (Mod.constant Trafo3d.Identity)
                |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor (C4f.Green) |> toEffect]
                |> Sg.depthTest      (Rendering.DepthTestMode.None          |> Mod.constant)                     
                |> Sg.blendMode      (BlendMode.Blend                       |> Mod.constant) 
                |> Sg.stencilMode    (stencilModeHighLightOnes              |> Mod.constant)  
                |> Sg.writeBuffers (Some (Set.singleton DefaultSemantic.Colors))
                |> Sg.pass (Renderpasses.HighlightPass)
           
        let fakeView = Mod.map2 (fun (m : Trafo3d) v -> m * v) graphicsScene.pointCloudTrafo graphicsScene.viewTrafo 
        let pointCloudSg = initialScene.pointCloudSg fakeView

        Sg.ofList ([selectionPath; fullscreenQuad; sg; pointCloudSg |> Sg.trafo (graphicsScene.pointCloudTrafo)])
            |> Sg.viewTrafo graphicsScene.viewTrafo
            |> Sg.projTrafo win.Projection
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)