﻿namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text
open Aardvark.SceneGraph

open OmnidirShadows

module GraphicsScene =
    open LogicalScene
//    open OmnidirShadows
    open VrInteractions
    open VrWindow

    type MObject =
        {
            mutable original    : Object
            mtrafo              : ModRef<Trafo3d>
            mmodel              : ModRef<ISg>
            mhasHighlight       : ModRef<bool>
            mscoredState        : ModRef<int>
            mtilingFactor       : ModRef<V2d>
        }

    type MScene =
        {
            mutable original    : Scene
            mobjects            : cset<MObject>
            mviewTrafo          : ModRef<Trafo3d>
            mlightPos           : ModRef<V3d>

            mscoreTrafo         : ModRef<Trafo3d>
            mscoreText          : ModRef<string>
            
            mhasRayCastHit      : ModRef<bool>
            mdrawHitPoint       : ModRef<bool>
            mdrawHitArea        : ModRef<bool>
            mhasRayCastDir      : ModRef<Trafo3d>
            mrayCastHitTrafo    : ModRef<Trafo3d>
            mrayCastCam         : ModRef<Trafo3d>
        }

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
                mhasHighlight = Mod.init o.isGrabbable
                mscoredState = Mod.init (getScoredState(o))
                mtilingFactor = Mod.init o.tilingFactor
            }

        static member Create(s : Scene) =
            let lightPos = LogicalScene.getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects).Forward.TransformPos(V3d())
            {
                original            = s
                mobjects            = CSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map Conversion.Create)
                mviewTrafo          = Mod.init s.viewTrafo
                mlightPos           = Mod.init lightPos

                mscoreTrafo         = Mod.init s.gameInfo.scoreTrafo
                mscoreText          = Mod.init s.gameInfo.scoreText
                
                mhasRayCastHit      = Mod.init s.raycastInfo.rayCastHasHit
                mdrawHitPoint       = Mod.init (s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos)
                mdrawHitArea        = Mod.init (s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportArea)
                mhasRayCastDir      = Mod.init (LogicalScene.getTrafoOfFirstObjectWithId(s.specialObjectIds.controller1ObjectId, s.objects))
                mrayCastHitTrafo    = Mod.init (Trafo3d.Translation(s.raycastInfo.rayCastHitPoint))
                mrayCastCam         = Mod.init (Trafo3d.Translation(s.raycastInfo.rayCastHitPoint))
            }

        static member Update(mo : MObject, o : Object) =
            if not (System.Object.ReferenceEquals(mo.original, o)) then
                mo.original <- o
                mo.mmodel.Value <- o.model
                mo.mtrafo.Value <- o.trafo
                mo.mhasHighlight.Value <- o.isGrabbable
                mo.mscoredState.Value <- (getScoredState(o))
                mo.mtilingFactor.Value <- o.tilingFactor

        static member Update(ms : MScene, s : Scene) =
            if not (System.Object.ReferenceEquals(ms.original, s)) then
                let lightPos = LogicalScene.getTrafoOfFirstObjectWithId(s.specialObjectIds.lightId, s.objects).Forward.TransformPos(V3d())

                ms.original <- s
                ms.mviewTrafo.Value <- s.viewTrafo
                ms.mlightPos.Value <- lightPos

                ms.mscoreTrafo.Value <- s.gameInfo.scoreTrafo
//                ms.mscoreText.Value <- s.scoreText // TODO: crash!

                let table = 
                    ms.mobjects |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                for t in PersistentHashSet.toSeq s.objects do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t)
                        | _ ->
                            let mo = Conversion.Create(t)
                            ms.mobjects.Add mo |> ignore
                
                ms.mobjects.ExceptWith table.Values
                
                ms.mhasRayCastHit.Value <- s.raycastInfo.rayCastHasHit
                ms.mdrawHitPoint.Value <- s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos
                ms.mdrawHitArea.Value <- s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportArea
                
                let hmdWorldTrafo = getTrafoOfFirstObjectWithId(s.specialObjectIds.headId, s.objects)
                let recenter = s.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos
                let newTrackingToWorld = VrInteractions.getTeleportTrafo(s.trackingToWorld, hmdWorldTrafo, s.raycastInfo.rayCastHitPoint, s.raycastInfo.rayCastHitNormal, recenter)
                ms.mrayCastCam.Value <- (hmdWorldTrafo * s.trackingToWorld.Inverse * newTrackingToWorld)
                ms.mhasRayCastDir.Value <- (LogicalScene.getTrafoOfFirstObjectWithId(s.specialObjectIds.controller1ObjectId, s.objects))
                ms.mrayCastHitTrafo.Value <- if recenter then Trafo3d.Translation(s.raycastInfo.rayCastHitPoint) else newTrackingToWorld
            
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
        let mscene = Conversion.Create initialScene

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
                Conversion.Update(mscene, scene)
            )
            ()

        win.Update <- update

        let toSg (t : MObject) =
            t.mmodel
                |> Sg.dynamic
                |> Sg.uniform "isHighlighted" t.mhasHighlight
                |> Sg.uniform "scoredState" t.mscoredState
                |> Sg.uniform "tilingFactor" t.mtilingFactor
                |> Sg.trafo t.mtrafo
        
        let objectsInScene = 
            mscene.mobjects
                |> ASet.map toSg
                |> Sg.set     
        
        let toShadowCasterSg (t : MObject) =
            if t.original.castsShadow then
                t.mmodel
                    |> Sg.dynamic
                    |> Sg.uniform "isHighlighted" t.mhasHighlight
                    |> Sg.uniform "scoredState" t.mscoredState
                    |> Sg.uniform "tilingFactor" t.mtilingFactor
                    |> Sg.trafo t.mtrafo
                    |> Sg.effect [DefaultSurfaces.trafo |> toEffect]
            else
                Sg.ofList []

        let shadowCasterInScene = 
            mscene.mobjects
                |> ASet.map toShadowCasterSg
                |> Sg.set

        let sgs = 
            objectsInScene
                |> Sg.uniform "LightLocation" mscene.mlightPos
                |> Sg.uniform "SpecularExponent" (Mod.constant 32)
                |> Sg.uniform "AmbientFactor" (Mod.constant 0.03)
                |> Sg.uniform "LinearAttenuation" (Mod.constant 0.05)
                |> Sg.blendMode(Mod.constant (BlendMode(false)))

        let newSg = OmnidirShadows.init(win, shadowCasterInScene, sgs)

        let textSg =
//            Sg.text (new Font("Arial",FontStyle.Bold)) C4b.Red mscene.mscoreText :> ISg
            Sg.markdown MarkdownConfig.light mscene.mscoreText
                |> Sg.trafo mscene.mscoreTrafo
                
        let rayCastDirSg =
            initialScene.raycastInfo.rayCastDirSg
                |> Sg.trafo mscene.mhasRayCastDir
                
        let rayCastHitPointSg =
            initialScene.raycastInfo.rayCastHitPointSg
                |> Sg.trafo mscene.mrayCastHitTrafo
                |> Sg.onOff mscene.mhasRayCastHit
                |> Sg.onOff mscene.mdrawHitPoint

        let rayCastHitAreaSg =
            initialScene.raycastInfo.rayCastHitAreaSg
                |> Sg.trafo mscene.mrayCastHitTrafo
                |> Sg.onOff mscene.mhasRayCastHit
                |> Sg.onOff mscene.mdrawHitArea
                
        let rayCastCamSg =
            initialScene.raycastInfo.rayCastCamSg
                |> Sg.trafo mscene.mrayCastCam
                |> Sg.onOff mscene.mhasRayCastHit
                
        // scene.scoreSg at last because markdown messes with stencil buffer
        Sg.ofList [newSg; rayCastDirSg; rayCastHitPointSg; rayCastHitAreaSg; rayCastCamSg; PhysicsScene.debugDrawer.debugDrawerSg; textSg]
            |> Sg.viewTrafo mscene.mviewTrafo
