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
                effect = o.effect
                mhasHighlight = Mod.init o.isGrabbable
                mscoredState = Mod.init (getScoredState(o))
                mtilingFactor = Mod.init o.tilingFactor
            }

        static member Create(s : Scene) =
            let lightTrafo = getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects)
            {
                original           = s
                graphicsObjects    = CSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map Conversion.Create)
                viewTrafo          = Mod.init s.viewTrafo
                lightTrafo         = Mod.init lightTrafo

                scoreTrafo         = Mod.init s.gameInfo.scoreTrafo
                scoreText          = Mod.init s.gameInfo.scoreText
                
                hasRayCastHit      = Mod.init s.raycastInfo.rayCastHasHit
                drawHitPoint       = Mod.init (s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos)
                drawHitArea        = Mod.init (s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportArea)
                hasRayCastDir      = Mod.init (getTrafoOfFirstObjectWithId(s.specialObjectIds.controller1ObjectId, s.objects))
                rayCastHitTrafo    = Mod.init (Trafo3d.Translation(s.raycastInfo.rayCastHitPoint))
                rayCastCam         = Mod.init (Trafo3d.Translation(s.raycastInfo.rayCastHitPoint))
            }

        static member Update(mo : GraphicsObject, o : Object) =
            if not (System.Object.ReferenceEquals(mo.original, o)) then
                mo.original <- o
                mo.mmodel.Value <- o.model
                mo.mtrafo.Value <- o.trafo
                mo.mhasHighlight.Value <- o.isGrabbable
                mo.mscoredState.Value <- (getScoredState(o))
                mo.mtilingFactor.Value <- o.tilingFactor

        static member Update(ms : GraphicsScene, s : Scene) =
            if not (System.Object.ReferenceEquals(ms.original, s)) then
                let lightTrafo = getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects)

                ms.original <- s
                ms.viewTrafo.Value <- s.viewTrafo
                ms.lightTrafo.Value <- lightTrafo

                ms.scoreTrafo.Value <- s.gameInfo.scoreTrafo
//                ms.scoreText.Value <- s.scoreText // TODO: crash!

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
                
                ms.hasRayCastHit.Value <- s.raycastInfo.rayCastHasHit
                ms.drawHitPoint.Value <- s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos
                ms.drawHitArea.Value <- s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportArea
                
                let hmdWorldTrafo = getTrafoOfFirstObjectWithId(s.specialObjectIds.headId, s.objects)
                let recenter = s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos
                let newTrackingToWorld = VrInteractions.getTeleportTrafo(s.trackingToWorld, hmdWorldTrafo, s.raycastInfo.rayCastHitPoint, s.raycastInfo.rayCastHitNormal, recenter)
                ms.rayCastCam.Value <- (hmdWorldTrafo * s.trackingToWorld.Inverse * newTrackingToWorld)
                ms.hasRayCastDir.Value <- (getTrafoOfFirstObjectWithId(s.specialObjectIds.controller1ObjectId, s.objects))
                ms.rayCastHitTrafo.Value <- if recenter then Trafo3d.Translation(s.raycastInfo.rayCastHitPoint) else newTrackingToWorld
            
    // renders the shadows to a texture
//    let private renderShadows (runtime : IRuntime) (shadowCasterSg : ISg) =
//        
//        let trafo = OmnidirShadows.lightSpaceViewProjTrafo s
//        let cullBack = Mod.constant CullMode.Clockwise
//
//        let texture =
//            shadowCasterSg
//                |> Sg.cullMode cullBack
//                |> OmnidirShadows.createTexture runtime s 
//
//        (texture, trafo)

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

        let textSg =
//            Sg.text (new Font("Arial",FontStyle.Bold)) C4b.Red graphicsScene.scoreText :> ISg
            Sg.markdown MarkdownConfig.light graphicsScene.scoreText
                |> Sg.trafo graphicsScene.scoreTrafo
                
        let rayCastDirSg =
            initialScene.raycastInfo.rayCastDirSg
                |> Sg.trafo graphicsScene.hasRayCastDir
                
        let rayCastHitPointSg =
            initialScene.raycastInfo.rayCastHitPointSg
                |> Sg.trafo graphicsScene.rayCastHitTrafo
                |> Sg.onOff graphicsScene.hasRayCastHit
                |> Sg.onOff graphicsScene.drawHitPoint

        let rayCastHitAreaSg =
            initialScene.raycastInfo.rayCastHitAreaSg
                |> Sg.trafo graphicsScene.rayCastHitTrafo
                |> Sg.onOff graphicsScene.hasRayCastHit
                |> Sg.onOff graphicsScene.drawHitArea
                
        let rayCastCamSg =
            initialScene.raycastInfo.rayCastCamSg
                |> Sg.trafo graphicsScene.rayCastCam
                |> Sg.onOff graphicsScene.hasRayCastHit
                
        // scene.scoreSg at last because markdown messes with stencil buffer
        Sg.ofList [sg; rayCastDirSg; rayCastHitPointSg; rayCastHitAreaSg; rayCastCamSg; PhysicsScene.debugDrawer.debugDrawerSg; textSg]
            |> Sg.viewTrafo graphicsScene.viewTrafo
            |> Sg.projTrafo win.Projection
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)