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
//        elif o.willReset then 1
        //elif o.hitLowerTrigger then 2
        //elif o.hitUpperTrigger then 1
        else 0

    type Conversion private() =
        static member Create(o : Object) =
            {
                original = o
                trafo = Mod.init o.trafo
                model = Mod.init o.model
                hasHighlight = Mod.init (o.hasHighlight)
                scoredState = Mod.init (getScoredState(o))
                tilingFactor = Mod.init o.tilingFactor
                visible = Mod.init o.visible
            }

        static member Create(s : Scene) =
            let lightPos = getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects).Forward.TransformPos(V3d())
            let ballSgIsTarget = [| for i in 0 .. s.specialObjectIds.ballObjectIds.Length-1 -> Mod.init (i = s.gameInfo.targetBallIndex) |]
            {
                original           = s
                graphicsObjects    = CSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map Conversion.Create)
                viewTrafo          = Mod.init s.viewTrafo
                lightPos           = Mod.init lightPos
                lightColor         = Mod.init s.lightColor

                scoreTrafo         = Mod.init s.gameInfo.scoreTrafo
                scoreText          = Mod.init s.gameInfo.scoreText
                ballSgIsTarget     = ballSgIsTarget
            }

        static member Update(mo : GraphicsObject, o : Object) =
            if not (System.Object.ReferenceEquals(mo.original, o)) then
                mo.original <- o
                mo.trafo.Value <- o.trafo
                mo.hasHighlight.Value <- (o.hasHighlight)
                mo.scoredState.Value <- (getScoredState(o))
                mo.tilingFactor.Value <- o.tilingFactor
                mo.visible.Value <- o.visible

        static member Update(ms : GraphicsScene, s : Scene) =
            if not (System.Object.ReferenceEquals(ms.original, s)) then
                let lightPos = getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects).Forward.TransformPos(V3d())

                ms.original <- s
                ms.viewTrafo.Value <- s.viewTrafo
                ms.lightPos.Value <- lightPos
                ms.lightColor.Value <- s.lightColor
                
                ms.scoreTrafo.Value <- s.gameInfo.scoreTrafo
                ms.scoreText.Value <- s.gameInfo.scoreText

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
                
                for i in 0 .. s.specialObjectIds.ballObjectIds.Length-1 do
                    ms.ballSgIsTarget.[i].Value <- (i = s.gameInfo.targetBallIndex)
         
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

            scene <- LogicalScene.update scene AfterPhysics

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

        let sg = OmnidirShadows.init(win, scene, graphicsScene)

        let textSg =
//            Sg.text (new Font("Arial",FontStyle.Bold)) C4b.Red graphicsScene.scoreText :> ISg
            Sg.markdown MarkdownConfig.light graphicsScene.scoreText
                |> Sg.trafo graphicsScene.scoreTrafo
                
        Sg.ofList ([sg; textSg; PhysicsScene.debugDrawer.debugDrawerSg])
            |> Sg.viewTrafo graphicsScene.viewTrafo
            |> Sg.projTrafo win.Projection
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)