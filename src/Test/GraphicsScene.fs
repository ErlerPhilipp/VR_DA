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

    let getScoredState (o : Object) =
        if o.hasScored then 3
        elif o.willReset then 1
        //elif o.hitLowerTrigger then 2
        //elif o.hitUpperTrigger then 1
        else 0

    type Conversion private() =
        static member Create(o : Object) =
            {
                original = o
                mtrafo = Mod.init o.trafo
                mmodel = Mod.init o.model
                mhasHighlight = Mod.init (o.isGrabbable <> GrabbableOptions.NoGrab)
                mscoredState = Mod.init (getScoredState(o))
                mtilingFactor = Mod.init o.tilingFactor
            }

        static member Create(s : Scene) =
            let lightPos = getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects).Forward.TransformPos(V3d())
            {
                original           = s
                graphicsObjects    = CSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map Conversion.Create)
                viewTrafo          = Mod.init s.viewTrafo
                lightPos           = Mod.init lightPos

                scoreTrafo         = Mod.init s.gameInfo.scoreTrafo
                scoreText          = Mod.init s.gameInfo.scoreText
                
                raycastMods1       =    {
                                            hasRayCastHit       = Mod.init s.interactionInfo1.raycastInfo.rayCastHasHit
                                            drawRayCastDir      = Mod.init (s.interactionInfo1.interactionType = VrInteractionTechnique.Flying || s.interactionInfo1.interactionType = VrInteractionTechnique.TeleportPos || s.interactionInfo1.interactionType = VrInteractionTechnique.TeleportArea)
                                            drawHitPoint        = Mod.init (s.interactionInfo1.interactionType = VrInteractionTechnique.TeleportPos)
                                            drawHitArea         = Mod.init (s.interactionInfo1.interactionType = VrInteractionTechnique.TeleportArea)
                                            hasRayCastDir       = Mod.init (getTrafoOfFirstObjectWithId(s.specialObjectIds.controller1ObjectId, s.objects))
                                            rayCastHitTrafo     = Mod.init (Trafo3d.Translation(s.interactionInfo1.raycastInfo.rayCastHitPoint))
                                            rayCastCam          = Mod.init (Trafo3d.Translation(s.interactionInfo1.raycastInfo.rayCastHitPoint))
                                        }
                raycastMods2       =    {
                                            hasRayCastHit       = Mod.init s.interactionInfo2.raycastInfo.rayCastHasHit
                                            drawRayCastDir      = Mod.init (s.interactionInfo2.interactionType = VrInteractionTechnique.Flying || s.interactionInfo2.interactionType = VrInteractionTechnique.TeleportPos || s.interactionInfo2.interactionType = VrInteractionTechnique.TeleportArea)
                                            drawHitPoint        = Mod.init (s.interactionInfo2.interactionType = VrInteractionTechnique.TeleportPos)
                                            drawHitArea         = Mod.init (s.interactionInfo2.interactionType = VrInteractionTechnique.TeleportArea)
                                            hasRayCastDir       = Mod.init (getTrafoOfFirstObjectWithId(s.specialObjectIds.controller2ObjectId, s.objects))
                                            rayCastHitTrafo     = Mod.init (Trafo3d.Translation(s.interactionInfo2.raycastInfo.rayCastHitPoint))
                                            rayCastCam          = Mod.init (Trafo3d.Translation(s.interactionInfo2.raycastInfo.rayCastHitPoint))
                                        }
            }

        static member Update(mo : GraphicsObject, o : Object) =
            if not (System.Object.ReferenceEquals(mo.original, o)) then
                mo.original <- o
                mo.mtrafo.Value <- o.trafo
                mo.mhasHighlight.Value <- (o.isGrabbable <> GrabbableOptions.NoGrab)
                mo.mscoredState.Value <- (getScoredState(o))
                mo.mtilingFactor.Value <- o.tilingFactor

        static member Update(ms : GraphicsScene, s : Scene) =
            if not (System.Object.ReferenceEquals(ms.original, s)) then
                let lightPos = getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects).Forward.TransformPos(V3d())

                ms.original <- s
                ms.viewTrafo.Value <- s.viewTrafo
                ms.lightPos.Value <- lightPos

                
                ms.scoreTrafo.Value <- s.gameInfo.scoreTrafo
                //ms.scoreText.Value <- s.gameInfo.scoreText

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
                
                ms.raycastMods1.hasRayCastHit.Value <- s.interactionInfo1.raycastInfo.rayCastHasHit
                ms.raycastMods1.drawRayCastDir.Value <- (s.interactionInfo1.interactionType = VrInteractionTechnique.Flying || s.interactionInfo1.interactionType = VrInteractionTechnique.TeleportPos || s.interactionInfo1.interactionType = VrInteractionTechnique.TeleportArea)
                ms.raycastMods1.drawHitPoint.Value <- s.interactionInfo1.interactionType = VrInteractions.VrInteractionTechnique.TeleportPos
                ms.raycastMods1.drawHitArea.Value <- s.interactionInfo1.interactionType = VrInteractions.VrInteractionTechnique.TeleportArea
                let hmdWorldTrafo = getTrafoOfFirstObjectWithId(s.specialObjectIds.headId, s.objects)
                let recenter = s.interactionInfo1.interactionType = VrInteractions.VrInteractionTechnique.TeleportPos
                let newTrackingToWorld = VrInteractions.getTeleportTrafo(s.trackingToWorld, hmdWorldTrafo, s.interactionInfo1.raycastInfo.rayCastHitPoint, s.interactionInfo1.raycastInfo.rayCastHitNormal, recenter)
                ms.raycastMods1.rayCastCam.Value <- (hmdWorldTrafo * s.trackingToWorld.Inverse * newTrackingToWorld)
                ms.raycastMods1.hasRayCastDir.Value <- (getTrafoOfFirstObjectWithId(s.specialObjectIds.controller1ObjectId, s.objects))
                ms.raycastMods1.rayCastHitTrafo.Value <- if recenter then Trafo3d.Translation(s.interactionInfo1.raycastInfo.rayCastHitPoint) else newTrackingToWorld
                
                ms.raycastMods2.hasRayCastHit.Value <- s.interactionInfo2.raycastInfo.rayCastHasHit
                ms.raycastMods2.drawRayCastDir.Value <- (s.interactionInfo2.interactionType = VrInteractionTechnique.Flying || s.interactionInfo2.interactionType = VrInteractionTechnique.TeleportPos || s.interactionInfo2.interactionType = VrInteractionTechnique.TeleportArea)
                ms.raycastMods2.drawHitPoint.Value <- s.interactionInfo2.interactionType = VrInteractions.VrInteractionTechnique.TeleportPos
                ms.raycastMods2.drawHitArea.Value <- s.interactionInfo2.interactionType = VrInteractions.VrInteractionTechnique.TeleportArea
                let hmdWorldTrafo = getTrafoOfFirstObjectWithId(s.specialObjectIds.headId, s.objects)
                let recenter = s.interactionInfo2.interactionType = VrInteractions.VrInteractionTechnique.TeleportPos
                let newTrackingToWorld = VrInteractions.getTeleportTrafo(s.trackingToWorld, hmdWorldTrafo, s.interactionInfo2.raycastInfo.rayCastHitPoint, s.interactionInfo2.raycastInfo.rayCastHitNormal, recenter)
                ms.raycastMods2.rayCastCam.Value <- (hmdWorldTrafo * s.trackingToWorld.Inverse * newTrackingToWorld)
                ms.raycastMods2.hasRayCastDir.Value <- (getTrafoOfFirstObjectWithId(s.specialObjectIds.controller2ObjectId, s.objects))
                ms.raycastMods2.rayCastHitTrafo.Value <- if recenter then Trafo3d.Translation(s.interactionInfo2.raycastInfo.rayCastHitPoint) else newTrackingToWorld
         
    let createScene (initialScene : Scene) (win : VrWindow) =
        let mutable scene = initialScene
        let graphicsScene = Conversion.Create initialScene

        let deviceCount = VrDriver.devices.Length
        let oldTrafos = Array.zeroCreate deviceCount
        let mutable oldTrackingToWorld = Trafo3d.Identity
        let update (dt : System.TimeSpan) (trafos : Trafo3d[]) (e : VREvent_t) =
            
            scene <- LogicalScene.update scene StartFrame

            let timeStepThreshold = 0.5
            if dt.TotalSeconds < timeStepThreshold && scene.physicsInfo.enablePhysics then
                scene <- PhysicsScene.stepSimulation dt scene 
                PhysicsScene.debugDrawer.flush()
                    
            scene <- LogicalScene.update scene (TimeElapsed dt)

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
  
            transact (fun () ->
                Conversion.Update(graphicsScene, scene)
            )
            ()

        win.Update <- update

        let sg = OmnidirShadows.init(win, scene, graphicsScene)

//        let textSg =
////            Sg.text (new Font("Arial",FontStyle.Bold)) C4b.Red graphicsScene.scoreText :> ISg
//            Sg.markdown MarkdownConfig.light graphicsScene.scoreText
//                |> Sg.trafo graphicsScene.scoreTrafo
                
        let makeRaycastSg (raycastMods : RaycastMods) =
            let rayCastDirSg =
                initialScene.rayCastDirSg
                    |> Sg.trafo raycastMods.hasRayCastDir
                    |> Sg.onOff raycastMods.drawRayCastDir
                
            let rayCastHitPointSg =
                initialScene.rayCastHitPointSg
                    |> Sg.trafo raycastMods.rayCastHitTrafo
                    |> Sg.onOff raycastMods.hasRayCastHit
                    |> Sg.onOff raycastMods.drawHitPoint

            let rayCastHitAreaSg =
                initialScene.rayCastHitAreaSg
                    |> Sg.trafo raycastMods.rayCastHitTrafo
                    |> Sg.onOff raycastMods.hasRayCastHit
                    |> Sg.onOff raycastMods.drawHitArea
                
            let rayCastCamSg =
                initialScene.rayCastCamSg
                    |> Sg.trafo raycastMods.rayCastCam
                    |> Sg.onOff raycastMods.hasRayCastHit

            [rayCastDirSg; rayCastHitPointSg; rayCastHitAreaSg; rayCastCamSg]
        
        let raycastSg1 = makeRaycastSg(graphicsScene.raycastMods1)
        let raycastSg2 = makeRaycastSg(graphicsScene.raycastMods2)

        // scene.scoreSg at last because markdown messes with stencil buffer
        Sg.ofList ([sg; PhysicsScene.debugDrawer.debugDrawerSg] @ raycastSg1 @ raycastSg2)
            |> Sg.viewTrafo graphicsScene.viewTrafo
            |> Sg.projTrafo win.Projection
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)