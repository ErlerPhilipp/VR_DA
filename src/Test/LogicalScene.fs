namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

module LogicalScene =
    open LogicalSceneTypes
    open VrTypes
    open VrInteractions
    open VrDriver
            
    let controller1OverlayColor = Mod.init (C4f())
    let controller2OverlayColor = Mod.init (C4f())

    let update (scene : Scene) (message : Message) : Scene =

        let makeSceneWithInteractionInfo(firstController : bool, newInteractionInfo : InteractionInfo) =
            if firstController then
                { scene with interactionInfo1 = newInteractionInfo}
            else
                { scene with interactionInfo2 = newInteractionInfo}

        match message with
            // move HMD
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.hmdId ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.headId, t, scene.objects, scene.physicsInfo.deltaTime)
                { scene with objects = newObjects; viewTrafo = t.Inverse }
            // move cam 1
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam1Id ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.cam1ObjectId, t, scene.objects, scene.physicsInfo.deltaTime)
                { scene with objects = newObjects }
            // move cam 2
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam2Id ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.cam2ObjectId, t, scene.objects, scene.physicsInfo.deltaTime)
                { scene with objects = newObjects }

            // move controller
            | DeviceMove(deviceId, t) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let (controllerObjectId, grabTriggerId, interactionInfo) = 
                    if deviceId = assignedInputs.controller1Id then
                        (scene.specialObjectIds.controller1ObjectId, scene.specialObjectIds.grabTrigger1Id, scene.interactionInfo1)
                    else // if deviceId = assignedInputs.controller2Id
                        (scene.specialObjectIds.controller2ObjectId, scene.specialObjectIds.grabTrigger2Id, scene.interactionInfo2)

                let updateObjects(virtualHandTrafo : Trafo3d) = 
                    let deltaTrafo = interactionInfo.lastContrTrafo.Inverse * virtualHandTrafo
                    let newObjects = setTrafoOfObjectsWithId(controllerObjectId, virtualHandTrafo, scene.objects, scene.physicsInfo.deltaTime)
                                        |> PersistentHashSet.map (fun a ->
                                            if (a.isGrabbed = GrabbedOptions.Controller1 && firstController) || 
                                               (a.isGrabbed = GrabbedOptions.Controller2 && not firstController) then 
                                                { a with trafo = a.trafo * deltaTrafo } else a
                                        )
                    let newObjects = setTrafoOfObjectsWithId(grabTriggerId, virtualHandTrafo, newObjects, scene.physicsInfo.deltaTime)

        //                // attach light to grabbing hand
//                        let lightPos = virtualHandTrafo
//                        let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.lightId, lightPos, newObjects, scene.physicsInfo.deltaTime)

                    newObjects

                match interactionInfo.interactionType with
                    | VrInteractionTechnique.VirtualHand ->
                        let virtualHandTrafo = t
                        let newObjects = updateObjects(virtualHandTrafo)
                        let newInteractionInfo = { interactionInfo with armExtensionFactor = 1.0; lastContrTrafo = virtualHandTrafo }
                        let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo)
                        { newScene with objects = newObjects}
                    | VrInteractionTechnique.GoGo ->
                        let virtualHandTrafo, extension = VrInteractions.getVirtualHandTrafoAndExtensionFactor(t, scene.viewTrafo, scene.trackingToWorld)
                        let newObjects = updateObjects(virtualHandTrafo)
                        let newInteractionInfo = { interactionInfo with armExtensionFactor = extension; lastContrTrafo = virtualHandTrafo }
                        let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo)
                        { newScene with objects = newObjects}
                    | VrInteractionTechnique.Flying ->
                        let newObjects = setTrafoOfObjectsWithId(controllerObjectId, t, scene.objects, scene.physicsInfo.deltaTime)
                        let direction = t.Forward.TransformDir(V3d.OOI)
                        let newInteractionInfo = { interactionInfo with moveDirection = direction }
                        let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo)
                        { newScene with objects = newObjects}
                    | VrInteractionTechnique.TeleportArea
                    | VrInteractionTechnique.TeleportPos ->     
                        let newObjects = setTrafoOfObjectsWithId(controllerObjectId, t, scene.objects, scene.physicsInfo.deltaTime)
                        let direction = t.Forward.TransformDir(V3d.OOI)
                        let rayCastStart = t.Forward.TransformPos(V3d())
                        let newInteractionInfo = { interactionInfo with 
                                                    moveDirection = direction 
                                                    raycastInfo =   { interactionInfo.raycastInfo with
                                                                        wantsRayCast = true
                                                                        rayCastStart = rayCastStart
                                                                        rayCastEnd = rayCastStart + -100.0 * direction
                                                                    }
                                                 }
                        let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo)
                        { newScene with objects = newObjects}
                    | _ -> failwith "Not implemented"
                 
            // press track pad
            | DevicePress(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trackpad) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = 
                    if deviceId = assignedInputs.controller1Id then
                        scene.interactionInfo1
                    else // if deviceId = assignedInputs.controller2Id
                        scene.interactionInfo2
                        
                let newInteractionTechnique = VrInteractions.nextInteractionTechnique interactionInfo.interactionType
                let newInteractionInfo = { interactionInfo with 
                                            interactionType = newInteractionTechnique
                                            raycastInfo = { interactionInfo.raycastInfo with wantsRayCast = false; rayCastHasHit = false }
                                         }

                if deviceId = assignedInputs.controller1Id then
                    transact ( fun _ -> Mod.change controller1OverlayColor (VrInteractions.colorForInteractionTechnique newInteractionTechnique) )
                else // deviceId = assignedInputs.controller2Id then
                    transact ( fun _ -> Mod.change controller2OverlayColor (VrInteractions.colorForInteractionTechnique newInteractionTechnique) )

                makeSceneWithInteractionInfo(firstController, newInteractionInfo)

            // press menu button     
//            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id && a = 2 ->
//                { scene with
//                    physicsInfo = { scene.physicsInfo with enablePhysics = not scene.physicsInfo.enablePhysics }
//                }
            // press trigger
            | DeviceTouch(deviceId, a, t) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trigger) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if deviceId = assignedInputs.controller1Id then scene.interactionInfo1 else scene.interactionInfo2
                        
                match interactionInfo.interactionType with
                    | VrInteractionTechnique.VirtualHand
                    | VrInteractionTechnique.GoGo ->
                        let newObjects = 
                            scene.objects 
                                |> PersistentHashSet.map (fun o ->
                                        if o.isManipulable && (o.isGrabbable = GrabbableOptions.Controller1 && firstController) || 
                                                              (o.isGrabbable = GrabbableOptions.Controller2 && not firstController) || 
                                                               o.isGrabbable = GrabbableOptions.BothControllers then 
                                            { o with 
                                                isGrabbed = if firstController then GrabbedOptions.Controller1 else GrabbedOptions.Controller2
        //                                        trafo = controller2Trafo // snap ball to controller
                                            } 
                                        else o
                                    ) 
                                |> PersistentHashSet.map (fun a -> if a.hitLowerTrigger then { a with hitLowerTrigger = false } else a) 
                                |> PersistentHashSet.map (fun a -> if a.hitUpperTrigger then { a with hitUpperTrigger = false } else a)
                                |> PersistentHashSet.map (fun a -> if a.hasScored then { a with hasScored = false } else a)
                        { scene with objects = newObjects }
                    | VrInteractionTechnique.Flying -> scene // handled in time elapsed message
                    | VrInteractionTechnique.TeleportArea
                    | VrInteractionTechnique.TeleportPos ->
                        if interactionInfo.raycastInfo.rayCastHasHit then
                            let hmdTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.headId, scene.objects)
                            let recenter = interactionInfo.interactionType = VrInteractions.VrInteractionTechnique.TeleportPos
                            let newTrackingToWorld = VrInteractions.getTeleportTrafo(scene.trackingToWorld, hmdTrafo, interactionInfo.raycastInfo.rayCastHitPoint, interactionInfo.raycastInfo.rayCastHitNormal, recenter)
                            { scene with 
                                trackingToWorld = newTrackingToWorld 
                                physicsInfo = { scene.physicsInfo with gravity = newTrackingToWorld.Forward.TransformDir(V3d(0.0, -9.81, 0.0)) }
                            }
                        else
                            scene
                    | _ -> failwith "Not implemented"
                    
            // release trigger
            | DeviceUntouch(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trigger) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if deviceId = assignedInputs.controller1Id then scene.interactionInfo1 else scene.interactionInfo2

                let newObjects = 
                    scene.objects 
                        |> PersistentHashSet.map (fun a ->
                                if a.isGrabbed = GrabbedOptions.NoGrab then a else { a with isGrabbed = GrabbedOptions.NoGrab }
                            ) 
                let newInteractionInfo = {interactionInfo with lastContrTrafo = Trafo3d.Identity }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo)
                { newScene with objects = newObjects}

            | StartFrame ->
                let newObjects = 
                    scene.objects 
                        |> PersistentHashSet.map (fun o -> 
                                if o.isGrabbable <> GrabbableOptions.NoGrab then { o with isGrabbable = GrabbableOptions.NoGrab } else o
                            )
                        |> PersistentHashSet.map (fun o -> 
                                let reset = o.willReset && (scene.gameInfo.timeSinceStart > o.timeToReset)
                                if reset then
                                    //printfn "Ball reset at %A" scene.timeSinceStart 
                                    { o with 
                                        hasScored = false
                                        hitLowerTrigger = false
                                        hitUpperTrigger = false
                                        isGrabbed = GrabbedOptions.NoGrab
                                        willReset = false
                                        timeToReset = 0.0
                                        trafo = Trafo3d.Translation(0.0, 0.5, 0.0)
                                        linearVelocity = V3d()
                                        angularVelocity = V3d()
                                    }
                                else
                                    o
                            )
                            
                let culture = System.Globalization.CultureInfo.CreateSpecificCulture("en-US")
                let newText = sprintf "Score: %A\r\nTime: %A" (scene.gameInfo.score.ToString("000", culture)) (scene.gameInfo.timeSinceStart.ToString("000.00", culture))

                { scene with 
                    objects = newObjects
                    gameInfo = {scene.gameInfo with scoreText = newText }
                    interactionInfo1 = { scene.interactionInfo1 with vibrationStrength = 0.0 }
                    interactionInfo2 = { scene.interactionInfo2 with vibrationStrength = 0.0 }
                }

            | TimeElapsed(dt) ->
                let newObjects = scene.objects |> PersistentHashSet.map (fun o -> 
                        { o with 
                            wasGrabbed = o.isGrabbed
                        }
                    ) 
                // reset balls below the ground
                let newObjects = newObjects |> PersistentHashSet.map (fun o -> 
                        if o.trafo.Forward.TransformPos(V3d()).Y < -100.0 then
                            { o with 
                                willReset = true
                                timeToReset = scene.gameInfo.timeSinceStart 
                            } 
                        else
                            o
                    )

                let getAxisValue(deviceId : uint32) = 
                    let mutable state = VRControllerState_t()
                    let axisPosition =
                        if system.GetControllerState(deviceId, &state) then
                            Some (V2d(state.[1].x, state.[1].y))
                        else None
                    let axisValue = if axisPosition.IsSome then axisPosition.Value.X else 0.0
                    axisValue

                let newTrackingToWorld = 
                    if scene.interactionInfo1.interactionType = VrInteractions.VrInteractionTechnique.Flying then
                        VrInteractions.getTrafoAfterFlying(scene.trackingToWorld, scene.interactionInfo1.moveDirection, dt.TotalSeconds, getAxisValue(uint32 assignedInputs.controller1Id))
                    else
                        scene.trackingToWorld
                let newTrackingToWorld = 
                    if scene.interactionInfo2.interactionType = VrInteractions.VrInteractionTechnique.Flying then
                        VrInteractions.getTrafoAfterFlying(newTrackingToWorld, scene.interactionInfo2.moveDirection, dt.TotalSeconds, getAxisValue(uint32 assignedInputs.controller2Id))
                    else
                        newTrackingToWorld
                
//                let lightRotation = Trafo3d.RotationYInDegrees(90.0 * dt.TotalSeconds)
//                let newObjects = transformTrafoOfObjectsWithId(scene.specialObjectIds.lightId, lightRotation, newObjects, scene.physicsInfo.deltaTime)

                let newTimeSinceStart = scene.gameInfo.timeSinceStart + dt.TotalSeconds

                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller1Id)
                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller2Id)

                // hit object
                if scene.interactionInfo1.vibStrLastFrame = 0.0 && scene.interactionInfo1.vibrationStrength <> 0.0 then
                    Vibration.vibrate(Vibration.HitObject, uint32 assignedInputs.controller1Id, 0.1, 0.5)
                if scene.interactionInfo2.vibStrLastFrame = 0.0 && scene.interactionInfo2.vibrationStrength <> 0.0 then
                    Vibration.vibrate(Vibration.HitObject, uint32 assignedInputs.controller2Id, 0.1, 0.5)
                    
                Vibration.vibrate(Vibration.OverlappingObject, uint32 assignedInputs.controller1Id, 1.0, scene.interactionInfo1.vibrationStrength)
                Vibration.vibrate(Vibration.OverlappingObject, uint32 assignedInputs.controller2Id, 1.0, scene.interactionInfo2.vibrationStrength)
                
                Vibration.updateVibration(uint32 assignedInputs.controller1Id, dt.TotalSeconds)
                Vibration.updateVibration(uint32 assignedInputs.controller2Id, dt.TotalSeconds)

                { scene with
                    objects = newObjects
                    trackingToWorld = newTrackingToWorld
                    physicsInfo = { scene.physicsInfo with deltaTime = dt.TotalSeconds }
                    gameInfo = { scene.gameInfo with timeSinceStart = newTimeSinceStart }
                    interactionInfo1 = { scene.interactionInfo1 with vibStrLastFrame = scene.interactionInfo1.vibrationStrength }
                    interactionInfo2 = { scene.interactionInfo2 with vibStrLastFrame = scene.interactionInfo2.vibrationStrength }
                }
            
            | Collision (ghostId, colliderId) ->
                let mutable newScore = scene.gameInfo.score
                let mutable newCtr1VibStrength = scene.interactionInfo1.vibrationStrength
                let mutable newCtr2VibStrength = scene.interactionInfo2.vibrationStrength
                let newObjects = 
                    scene.objects 
                        // hit upper hoop trigger
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithUpperHoop = ghostId = scene.specialObjectIds.upperHoopTriggerId && o.id = colliderId
                                let hitUpperTrigger = collidingWithUpperHoop && not o.hitUpperTrigger && not o.hitLowerTrigger && o.isGrabbed = GrabbedOptions.NoGrab && o.linearVelocity.Y < 0.0
                                if hitUpperTrigger then
                                    //printfn "hit upper trigger at %A" scene.timeSinceStart
                                    { o with hitUpperTrigger = true } 
                                else 
                                    o
                            )
                        // hit lower hoop trigger
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithLowerHoop = ghostId = scene.specialObjectIds.lowerHoopTriggerId && o.id = colliderId
                                let hitLowerTrigger = collidingWithLowerHoop && not o.hitLowerTrigger && o.isGrabbed = GrabbedOptions.NoGrab
                                if hitLowerTrigger then
                                    //printfn "hit lower trigger at %A" scene.timeSinceStart
                                    { o with hitLowerTrigger = true } 
                                else 
                                    o
                            )
                        // check score
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithLowerHoop = ghostId = scene.specialObjectIds.lowerHoopTriggerId && o.id = colliderId
                                let scored = collidingWithLowerHoop && o.hitLowerTrigger && o.hitUpperTrigger && not o.hasScored && o.linearVelocity.Y < 0.0
                                if scored then
                                    newScore <- newScore + 1
                                    printfn "Scored %A at %A" newScore scene.gameInfo.timeSinceStart
                                    Vibration.stopVibration(Vibration.Score, uint32 assignedInputs.controller1Id)
                                    Vibration.stopVibration(Vibration.Score, uint32 assignedInputs.controller2Id)
                                    Vibration.sinusiodFunctionPulses(3, 15, 0.3, Vibration.Score, uint32 assignedInputs.controller1Id, 1.0)
                                    Vibration.sinusiodFunctionPulses(3, 15, 0.3, Vibration.Score, uint32 assignedInputs.controller2Id, 1.0)
                                    { o with 
                                        hasScored = true
                                    } 
                                else 
                                    o
                            )
                        // check grabbable with controller
                        |> PersistentHashSet.map (fun o -> 
                                // TODO: only if virtual hand or gogo
//                                let interactionInfo = if deviceId = assignedInputs.controller1Id then scene.interactionInfo1 else scene.interactionInfo2
    
                                let colliderObject = getObjectWithId(colliderId, scene.objects)
                                let collidingWithController = o.id = colliderId && 
                                                              ((ghostId = scene.specialObjectIds.grabTrigger1Id && 
                                                                colliderId <> scene.specialObjectIds.controller1ObjectId) ||
                                                               (ghostId = scene.specialObjectIds.grabTrigger2Id && 
                                                                colliderId <> scene.specialObjectIds.controller2ObjectId))
                                if collidingWithController then
                                    let velToStrength(vel : float) = 
                                        let maxTrackingNoiseLevel = 0.1
                                        let velWithoutTrackingNoise = (max (vel - maxTrackingNoiseLevel) 0.0) * 1.0 / (1.0 - maxTrackingNoiseLevel) // -a, clamp, to 0..1

                                        let velocityToStrength = 0.6
                                        let constVibrationOffset = 0.1
                                        let linearStrength = constVibrationOffset + velWithoutTrackingNoise * velocityToStrength

                                        let clampedStrength = clamp 0.0 1.0 linearStrength
                                        clampedStrength
                                        
                                    let ghostLinearVelocity = getObjectWithId(ghostId, scene.objects).linearVelocity
                                    let colliderLinearVelocity = o.linearVelocity
                                    let relativeVel = (ghostLinearVelocity - colliderLinearVelocity).Length
                                    let strength = velToStrength(relativeVel)
//                                     printfn "ghostLinearVelocity: %A, colliderLinearVelocity: %A, strength %A" (ghostLinearVelocity.Length) (colliderLinearVelocity.Length) strength
                                    
                                    let firstController = ghostId = scene.specialObjectIds.grabTrigger1Id
                                    if firstController then
                                        if strength > newCtr1VibStrength then newCtr1VibStrength <- strength
                                    else
                                        if strength > newCtr2VibStrength then newCtr2VibStrength <- strength

                                    if colliderObject.isManipulable then
                                        let newGrabbableState = 
                                            match o.isGrabbable with
                                                | GrabbableOptions.NoGrab -> if firstController then GrabbableOptions.Controller1 else GrabbableOptions.Controller2
                                                | GrabbableOptions.Controller1 -> if firstController then GrabbableOptions.Controller1 else GrabbableOptions.BothControllers
                                                | GrabbableOptions.Controller2 -> if firstController then GrabbableOptions.BothControllers else GrabbableOptions.Controller2
                                                | GrabbableOptions.BothControllers -> GrabbableOptions.BothControllers
                                        { o with isGrabbable = newGrabbableState } 
                                    else o
                                else  o
                            )
                        // check reset on ground
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithGround = ghostId = scene.specialObjectIds.groundObjectId && o.id = colliderId && o.isManipulable && not o.willReset && o.isGrabbed = GrabbedOptions.NoGrab
                                let resetDelay = 3.0
                                if collidingWithGround then 
                                    { o with 
                                        willReset = true
                                        timeToReset = scene.gameInfo.timeSinceStart + resetDelay 
                                    } 
                                else 
                                    o
                            )
                                 
                { scene with 
                    objects = newObjects
                    gameInfo = { scene.gameInfo with score = newScore }
                    interactionInfo1 = { scene.interactionInfo1 with vibrationStrength = newCtr1VibStrength }
                    interactionInfo2 = { scene.interactionInfo2 with vibrationStrength = newCtr2VibStrength }
                }
            | RayCastResult (source, hasHit, hitPoint, hitNormal) ->
                let firstController = source = 0
                let oldInteractionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                let newInteractionInfo = { oldInteractionInfo with
                                            raycastInfo = { oldInteractionInfo.raycastInfo with 
                                                                rayCastHasHit = hasHit
                                                                rayCastHitPoint = hitPoint
                                                                rayCastHitNormal = hitNormal
                                                                wantsRayCast = false
                                                          }
                                         }
                makeSceneWithInteractionInfo(firstController, newInteractionInfo)
            | _ -> scene
