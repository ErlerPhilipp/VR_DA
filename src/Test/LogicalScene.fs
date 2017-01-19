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

    let randomNumberGen = System.Random()
    let resetDelay = 1.0
            
    let controller1OverlayColor = Mod.init (VrInteractions.colorForInteractionTechnique VrInteractionTechnique.VirtualHand)
    let controller2OverlayColor = Mod.init (VrInteractions.colorForInteractionTechnique VrInteractionTechnique.VirtualHand)
    
    let makeSceneWithInteractionInfo(firstController : bool, newInteractionInfo : InteractionInfo, scene : Scene) =
        if firstController then
            { scene with interactionInfo1 = newInteractionInfo}
        else
            { scene with interactionInfo2 = newInteractionInfo}

    let update (scene : Scene) (message : Message) : Scene =

        match message with
            // move HMD
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.hmdId ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.headId, t, scene.objects, scene.physicsInfo.deltaTime)

                let hmdPos = t.Forward.TransformPos(V3d())
                let hmdLookAt = t.Forward.TransformDir(-V3d.OOI)
                let hmdUp = t.Forward.TransformDir(V3d.OIO)
                Audio.setListenerPosition(hmdPos)
                Audio.setListenerOrientation(hmdLookAt, hmdUp)

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
                    if firstController then
                        (scene.specialObjectIds.controller1ObjectId, scene.specialObjectIds.grabTrigger1Id, scene.interactionInfo1)
                    else
                        (scene.specialObjectIds.controller2ObjectId, scene.specialObjectIds.grabTrigger2Id, scene.interactionInfo2)
                        
                let updateControllerObjects(virtualHandTrafo : Trafo3d, objects : PersistentHashSet<Object>) = 
                    let deltaTrafo = interactionInfo.lastContrTrafo.Inverse * virtualHandTrafo
                    let newObjects = setTrafoOfObjectsWithId(controllerObjectId, virtualHandTrafo, objects, scene.physicsInfo.deltaTime)
                    let newObjects = setTrafoOfObjectsWithId(grabTriggerId, virtualHandTrafo, newObjects, scene.physicsInfo.deltaTime)
        //                // attach light to grabbing hand
//                        let lightPos = virtualHandTrafo
//                        let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.lightId, lightPos, newObjects, scene.physicsInfo.deltaTime)
                    newObjects

                let updateGrabbedObjects(virtualHandTrafo : Trafo3d, objects : PersistentHashSet<Object>) = 
                    let deltaTrafo = interactionInfo.lastContrTrafo.Inverse * virtualHandTrafo
                    objects
                        |> PersistentHashSet.map (fun a ->
                            if (a.isGrabbed = GrabbedOptions.Controller1 && firstController) || 
                                (a.isGrabbed = GrabbedOptions.Controller2 && not firstController) then 
                                { a with trafo = a.trafo * deltaTrafo } else a
                        )


                match interactionInfo.interactionType with
                    | VrInteractionTechnique.VirtualHand ->
                        let newObjects = updateControllerObjects(t, scene.objects)
                        let newObjects = updateGrabbedObjects(t, newObjects)
                        let newInteractionInfo = { interactionInfo with armExtensionFactor = 1.0; lastContrTrafo = t }
                        let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                        { newScene with objects = newObjects}
                    | VrInteractionTechnique.GoGo ->
                        let virtualHandTrafo, extension = VrInteractions.getVirtualHandTrafoAndExtensionFactor(t, scene.viewTrafo, scene.trackingToWorld)
                        let newObjects = updateControllerObjects(virtualHandTrafo, scene.objects)
                        let newObjects = updateGrabbedObjects(virtualHandTrafo, newObjects)
                        let newInteractionInfo = { interactionInfo with armExtensionFactor = extension; lastContrTrafo = virtualHandTrafo }
                        let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                        { newScene with objects = newObjects}
                    | VrInteractionTechnique.Flying ->
                        let direction = t.Forward.TransformDir(V3d.OOI)
                        let newObjects = updateControllerObjects(t, scene.objects)
                        let newInteractionInfo = { interactionInfo with moveDirection = direction }
                        let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                        { newScene with objects = newObjects}
                    | VrInteractionTechnique.TeleportArea
                    | VrInteractionTechnique.TeleportPos ->     
                        let direction = t.Forward.TransformDir(V3d.OOI)
                        let newObjects = updateControllerObjects(t, scene.objects)
                        let rayCastStart = t.Forward.TransformPos(V3d())
                        let newInteractionInfo = { interactionInfo with 
                                                    moveDirection = direction 
                                                    raycastInfo =   { interactionInfo.raycastInfo with
                                                                        wantsRayCast = true
                                                                        rayCastStart = rayCastStart
                                                                        rayCastEnd = rayCastStart + -100.0 * direction
                                                                    }
                                                 }
                        let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                        { newScene with objects = newObjects}
                    | _ -> failwith "Not implemented"
                 
            // press track pad
            | DevicePress(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trackpad) ->
                if scene.enableExperimental then
                    let firstController = deviceId = assignedInputs.controller1Id
                    let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                        
                    let newInteractionTechnique = VrInteractions.nextInteractionTechnique interactionInfo.interactionType
                    let newInteractionInfo = { interactionInfo with 
                                                interactionType = newInteractionTechnique
                                                raycastInfo = { interactionInfo.raycastInfo with wantsRayCast = false; rayCastHasHit = false }
                                             }

                    if firstController then
                        transact ( fun _ -> Mod.change controller1OverlayColor (VrInteractions.colorForInteractionTechnique newInteractionTechnique) )
                    else
                        transact ( fun _ -> Mod.change controller2OverlayColor (VrInteractions.colorForInteractionTechnique newInteractionTechnique) )

                    makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                else
                    scene

            // press menu button     
//            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id && a = 2 ->
//                { scene with
//                    physicsInfo = { scene.physicsInfo with enablePhysics = not scene.physicsInfo.enablePhysics }
//                }
            // press trigger
            | DeviceTouch(deviceId, a, t) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trigger) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                        
                let scene = if not interactionInfo.triggerPressed then
                                    let newInteractionInfo = { interactionInfo with triggerPressed = true}
                                    makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                                else
                                    scene

                match interactionInfo.interactionType with
                    | VrInteractionTechnique.VirtualHand
                    | VrInteractionTechnique.GoGo ->
                        let newObjects = 
                            scene.objects 
                                |> PersistentHashSet.map (fun o ->
                                        if o.isManipulable && o.isGrabbed = GrabbedOptions.NoGrab &&
                                           ((o.isGrabbable = GrabbableOptions.Controller1 && firstController) || 
                                            (o.isGrabbable = GrabbableOptions.Controller2 && not firstController) || 
                                            o.isGrabbable = GrabbableOptions.BothControllers) then 
                                            { o with 
                                                isGrabbed = if firstController then GrabbedOptions.Controller1 else GrabbedOptions.Controller2
                                                hitLowerTrigger = false
                                                hitUpperTrigger = false
                                                hasScored = false
        //                                        trafo = controller2Trafo // snap ball to controller
                                            } 
                                        else o
                                    ) 
//                                |> PersistentHashSet.map (fun a -> if a.hitLowerTrigger then { a with hitLowerTrigger = false } else a) 
//                                |> PersistentHashSet.map (fun a -> if a.hitUpperTrigger then { a with hitUpperTrigger = false } else a)
//                                |> PersistentHashSet.map (fun a -> if a.hasScored then { a with hasScored = false } else a)
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
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                
                let newObjects = 
                    scene.objects 
                        |> PersistentHashSet.map (fun a ->
                                if a.isManipulable then
                                    let newGrabbedState = match a.isGrabbed with
                                                            | GrabbedOptions.NoGrab -> GrabbedOptions.NoGrab
                                                            | GrabbedOptions.Controller1 when firstController -> GrabbedOptions.NoGrab
                                                            | GrabbedOptions.Controller1 when not firstController -> GrabbedOptions.Controller1
                                                            | GrabbedOptions.Controller2 when firstController -> GrabbedOptions.Controller2
                                                            | GrabbedOptions.Controller2 when not firstController -> GrabbedOptions.NoGrab
                                                            | _ -> GrabbedOptions.NoGrab // should never happen
                                    if a.isGrabbed = newGrabbedState then a else { a with isGrabbed = newGrabbedState }
                                else a
                            ) 
                let newInteractionInfo = {interactionInfo with lastContrTrafo = Trafo3d.Identity; triggerPressed = false }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                { newScene with objects = newObjects}

            | StartFrame ->
                let newObjects = 
                    scene.objects 
                        |> PersistentHashSet.map (fun o -> 
                                if o.isGrabbable <> GrabbableOptions.NoGrab then { o with isGrabbable = GrabbableOptions.NoGrab } else o
                            )
                { scene with 
                    objects = newObjects
                    interactionInfo1 = { scene.interactionInfo1 with vibrationStrength = 0.0 }
                    interactionInfo2 = { scene.interactionInfo2 with vibrationStrength = 0.0 }
                }

            | TimeElapsed(dt) ->
                let dt = dt.TotalSeconds

                // flying
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
                        VrInteractions.getTrafoAfterFlying(scene.trackingToWorld, scene.interactionInfo1.moveDirection, dt, getAxisValue(uint32 assignedInputs.controller1Id))
                    else
                        scene.trackingToWorld
                let newTrackingToWorld = 
                    if scene.interactionInfo2.interactionType = VrInteractions.VrInteractionTechnique.Flying then
                        VrInteractions.getTrafoAfterFlying(newTrackingToWorld, scene.interactionInfo2.moveDirection, dt, getAxisValue(uint32 assignedInputs.controller2Id))
                    else
                        newTrackingToWorld
                
//                let lightRotation = Trafo3d.RotationYInDegrees(90.0 * dt)
//                let newObjects = transformTrafoOfObjectsWithId(scene.specialObjectIds.lightId, lightRotation, newObjects, scene.physicsInfo.deltaTime)

                let newTimeSinceStart = scene.gameInfo.timeSinceStart + dt 
                
                let newObjects = 
                    scene.objects 
                        |> PersistentHashSet.map (fun o -> 
                                let newTimeToReset = o.timeToReset - dt
                                let reset = o.willReset && newTimeToReset < 0.0
                                if reset then
                                    scene.popSoundSource.Location <- scene.ballResetPos
                                    scene.popSoundSource.Play()
                                    { o with 
                                        hasScored = false
                                        hitLowerTrigger = false
                                        hitUpperTrigger = false
                                        isGrabbed = GrabbedOptions.NoGrab
                                        willReset = false
                                        timeToReset = 0.0
                                        trafo = Trafo3d.Translation(scene.ballResetPos)
                                        linearVelocity = V3d()
                                        angularVelocity = V3d()
                                    }
                                else
                                    { o with timeToReset = newTimeToReset }
                            )
                            
                let timePerRound = 3.0 * 60.0 
                let remainingTime = timePerRound - scene.gameInfo.timeSinceStart
                let newGameInfo =   if remainingTime < 0.0 && scene.gameInfo.running then
                                        printfn "%A: Time's up!" scene.gameInfo.timeSinceStart
                                        if not (scene.sireneSoundSource.IsPlaying()) then scene.sireneSoundSource.Play()
                                        { scene.gameInfo with
                                            score = 0
                                            timeSinceStart = 0.0
                                            running = false
                                            lastRoundScore = scene.gameInfo.score
                                        }
                                    else
                                        scene.gameInfo
                            
                let culture = System.Globalization.CultureInfo.CreateSpecificCulture("en-US")
                let newText =   if not scene.gameInfo.running then 
                                    let pointsUntilStart = 3
                                    let remainingScoreString = ((pointsUntilStart - scene.gameInfo.score).ToString("0", culture))
                                    let lastRoundScoreString = (scene.gameInfo.lastRoundScore.ToString("000", culture))
                                    "Score:    " + lastRoundScoreString + "\r\n" + 
                                    "Start in:    " + remainingScoreString
                                else
                                    let remainingTimeString = (remainingTime.ToString("000", culture))
                                    let scoreString = (scene.gameInfo.score.ToString("000", culture))
//                                    let timeString = (scene.gameInfo.timeSinceStart.ToString("000.00", culture))
                                    "Score:    " + scoreString + "\r\n" + 
                                    "Time:    " + remainingTimeString

                let newGameInfo = {newGameInfo with scoreText = newText; timeSinceStart = newTimeSinceStart}

                { scene with
                    objects = newObjects
                    trackingToWorld = newTrackingToWorld
                    gameInfo = newGameInfo
                    physicsInfo = { scene.physicsInfo with deltaTime = dt }
                }
            
            | EndFrame ->
                let newObjects = scene.objects |> PersistentHashSet.map (fun o -> 
                        if o.isManipulable then
                            { o with 
                                wasGrabbed = o.isGrabbed
                            }
                        else o
                    )
                // reset balls below the ground
                let newObjects = newObjects |> PersistentHashSet.map (fun o -> 
                        if o.trafo.Forward.TransformPos(V3d()).Y < -100.0 then
                            { o with 
                                willReset = true
                                timeToReset = resetDelay
                            } 
                        else
                            o
                    )

                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller1Id)
                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller2Id)

                // hit object
                if scene.interactionInfo1.vibStrLastFrame = 0.0 && scene.interactionInfo1.vibrationStrength <> 0.0 then
                    Vibration.vibrate(Vibration.HitObject, uint32 assignedInputs.controller1Id, 0.1, 0.5)
                if scene.interactionInfo2.vibStrLastFrame = 0.0 && scene.interactionInfo2.vibrationStrength <> 0.0 then
                    Vibration.vibrate(Vibration.HitObject, uint32 assignedInputs.controller2Id, 0.1, 0.5)
                
                // overlap
                Vibration.vibrate(Vibration.OverlappingObject, uint32 assignedInputs.controller1Id, 1.0, scene.interactionInfo1.vibrationStrength)
                Vibration.vibrate(Vibration.OverlappingObject, uint32 assignedInputs.controller2Id, 1.0, scene.interactionInfo2.vibrationStrength)
                
                Vibration.updateVibration(uint32 assignedInputs.controller1Id, scene.physicsInfo.deltaTime)
                Vibration.updateVibration(uint32 assignedInputs.controller2Id, scene.physicsInfo.deltaTime)

                { scene with
                    objects = newObjects
                    interactionInfo1 = { scene.interactionInfo1 with vibStrLastFrame = scene.interactionInfo1.vibrationStrength }
                    interactionInfo2 = { scene.interactionInfo2 with vibStrLastFrame = scene.interactionInfo2.vibrationStrength }
                }

            | Collision (ghostId, colliderId) ->
                let mutable newGameInfo = scene.gameInfo
                let mutable newCtr1VibStrength = scene.interactionInfo1.vibrationStrength
                let mutable newCtr2VibStrength = scene.interactionInfo2.vibrationStrength
                let newObjects = 
                    scene.objects 
                        // hit upper hoop trigger
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithUpperHoop = ghostId = scene.specialObjectIds.upperHoopTriggerId && o.id = colliderId
                                let hitUpperTrigger = collidingWithUpperHoop && not o.hitUpperTrigger && not o.hitLowerTrigger && o.isGrabbed = GrabbedOptions.NoGrab && o.linearVelocity.Y < 0.0
                                if hitUpperTrigger then
                                    { o with hitUpperTrigger = true } 
                                else 
                                    o
                            )
                        // hit lower hoop trigger
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithLowerHoop = ghostId = scene.specialObjectIds.lowerHoopTriggerId && o.id = colliderId
                                let hitLowerTrigger = collidingWithLowerHoop && not o.hitLowerTrigger && o.isGrabbed = GrabbedOptions.NoGrab
                                if hitLowerTrigger then
                                    { o with hitLowerTrigger = true } 
                                else 
                                    o
                            )
                        // check score
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithLowerHoop = ghostId = scene.specialObjectIds.lowerHoopTriggerId && o.id = colliderId
                                let scored = collidingWithLowerHoop && o.hitLowerTrigger && o.hitUpperTrigger && not o.hasScored && o.linearVelocity.Y < 0.0
                                if scored then
                                    scene.sireneSoundSource.Play()
                                    newGameInfo <- {newGameInfo with score = newGameInfo.score + 1}
                                    printfn "%A: Scored %A" scene.gameInfo.timeSinceStart newGameInfo.score
                                    if not scene.gameInfo.running && newGameInfo.score = 3 then
                                        newGameInfo <- {newGameInfo with running = true; numRounds = newGameInfo.numRounds + 1; timeSinceStart = 0.0; score = 0}
                                        printfn "%A: Warm-up finished, starting round %A!" newGameInfo.timeSinceStart newGameInfo.numRounds
                                    Vibration.stopVibration(Vibration.Score, uint32 assignedInputs.controller1Id)
                                    Vibration.stopVibration(Vibration.Score, uint32 assignedInputs.controller2Id)
                                    Vibration.sinusiodFunctionPulses(3, 15, 0.3, Vibration.Score, uint32 assignedInputs.controller1Id, 1.0)
                                    Vibration.sinusiodFunctionPulses(3, 15, 0.3, Vibration.Score, uint32 assignedInputs.controller2Id, 1.0)
                                    { o with 
                                        hasScored = true
                                        willReset = true
                                        timeToReset = resetDelay 
                                    } 
                                else 
                                    o
                            )
                        // grab trigger
                        |> PersistentHashSet.map (fun o -> 
                                let colliderObject = getObjectWithId(colliderId, scene.objects)
                                let collidingWithController = o.id = colliderId && 
                                                              ((ghostId = scene.specialObjectIds.grabTrigger1Id && 
                                                                colliderId <> scene.specialObjectIds.controller1ObjectId) ||
                                                               (ghostId = scene.specialObjectIds.grabTrigger2Id && 
                                                                colliderId <> scene.specialObjectIds.controller2ObjectId))
                                                                
                                let firstController = ghostId = scene.specialObjectIds.grabTrigger1Id
                                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                                let thisControllerCanGrab = interactionInfo.interactionType = VrInteractionTechnique.VirtualHand || interactionInfo.interactionType = VrInteractionTechnique.GoGo

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
                                    
                                    if firstController then
                                        if strength > newCtr1VibStrength then newCtr1VibStrength <- strength
                                    else
                                        if strength > newCtr2VibStrength then newCtr2VibStrength <- strength

                                    // check grabbable with
                                    if colliderObject.isManipulable && thisControllerCanGrab then
                                        let newGrabbableState = 
                                            match o.isGrabbable with
                                                | GrabbableOptions.NoGrab -> if firstController then GrabbableOptions.Controller1 else GrabbableOptions.Controller2
                                                | GrabbableOptions.Controller1 -> if firstController then GrabbableOptions.Controller1 else GrabbableOptions.BothControllers
                                                | GrabbableOptions.Controller2 -> if firstController then GrabbableOptions.BothControllers else GrabbableOptions.Controller2
                                                | GrabbableOptions.BothControllers -> GrabbableOptions.BothControllers
                                                
                                        let grabNow = interactionInfo.triggerPressed && o.isManipulable && o.isGrabbed = GrabbedOptions.NoGrab &&
                                                       ((newGrabbableState = GrabbableOptions.Controller1 && firstController) || 
                                                        (newGrabbableState = GrabbableOptions.Controller2 && not firstController) || 
                                                        newGrabbableState = GrabbableOptions.BothControllers)

                                        if grabNow then 
                                            { o with 
                                                isGrabbable = newGrabbableState 
                                                isGrabbed = if firstController then GrabbedOptions.Controller1 else GrabbedOptions.Controller2
                                                hitLowerTrigger = false
                                                hitUpperTrigger = false
                                                hasScored = false
        //                                        trafo = controller2Trafo // snap ball to controller
                                            } 
                                        else
                                            { o with 
                                                isGrabbable = newGrabbableState 
                                            }
                                    else o
                                else  o
                            )
                        // check reset on ground
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithGround = ghostId = scene.specialObjectIds.groundTriggerId && o.id = colliderId && o.isManipulable && not o.willReset && o.isGrabbed = GrabbedOptions.NoGrab
                                if collidingWithGround then 
                                    { o with 
                                        willReset = true
                                        timeToReset = resetDelay 
                                    } 
                                else 
                                    o
                            )
                
                { scene with 
                    objects = newObjects
                    gameInfo = newGameInfo
                    interactionInfo1 = { scene.interactionInfo1 with vibrationStrength = newCtr1VibStrength }
                    interactionInfo2 = { scene.interactionInfo2 with vibrationStrength = newCtr2VibStrength }
                }
            | CollisionAdded (collider0Id, collider1Id, contactNormal, contactWorldPos : V3d) ->
                // play bounce sound if the ball collides with something
                let obj0 = getObjectWithId(collider0Id, scene.objects)
                let obj1 = getObjectWithId(collider1Id, scene.objects)
                if (obj0.objectType = ObjectTypes.Dynamic || obj1.objectType = ObjectTypes.Dynamic) then
                    let firstObjIsBall = obj0.objectType = ObjectTypes.Dynamic
                    let ballObj = if firstObjIsBall then obj0 else obj1
                    let otherObj = if firstObjIsBall then obj1 else obj0

                    if otherObj.id <> scene.specialObjectIds.grabTrigger1Id && otherObj.id <> scene.specialObjectIds.grabTrigger2Id then
                        let mutable startedSound = false
//                        printfn "ball %A collides with %A" ballObj.id otherObj.id
                        for bounceSound in scene.bounceSoundSources do
                            if not startedSound && not (bounceSound.IsPlaying()) then
//                              let impulseStrength = (obj0.linearVelocity - obj1.linearVelocity).Length //.Dot(contactNormal)
//                              let impulseStrength = obj0.linearVelocity.Dot(obj1.linearVelocity)
                                let impulseStrength = contactNormal.Length
                                if impulseStrength > 0.1 then
                                    let volume = clamp 0.0 1.0 (impulseStrength * 0.5)
//                                    printfn "play bounce of %A with %A, impulse: %A, vol: %A at %A" ballObj.id otherObj.id impulseStrength volume contactWorldPos
                                    bounceSound.Location <- ballObj.trafo.Forward.TransformPos(V3d())
                                    bounceSound.Volume <- volume
                                    let pitchOffset = 0.8 * (randomNumberGen.NextDouble() - 0.5)
                                    bounceSound.Pitch <- 1.0 + pitchOffset
                                    bounceSound.Play()
                                    startedSound <- true

                // vibrate if controller collides with something
                let mutable newCtr1VibStrength = scene.interactionInfo1.vibrationStrength
                let mutable newCtr2VibStrength = scene.interactionInfo2.vibrationStrength
                let obj0IsController = (obj0.id = scene.specialObjectIds.controller1ObjectId) || (obj0.id = scene.specialObjectIds.controller2ObjectId)
                let obj1IsController = (obj1.id = scene.specialObjectIds.controller1ObjectId) || (obj1.id = scene.specialObjectIds.controller2ObjectId)
                let controllerCollidesWithSomething = obj0IsController <> obj1IsController // only 1 controller
                    
                if controllerCollidesWithSomething then
//                        printfn "controllerCollidesWithSomething"
                    let controllerObj = if obj0IsController then obj0 else obj1
                    let otherObj = if obj0IsController then obj1 else obj0
                    let firstController = controllerObj.id = scene.specialObjectIds.controller1ObjectId

                    let hitImpulseToStrength(impulse : float) = 
                        let maxTrackingNoiseLevel = 0.1
                        let velWithoutTrackingNoise = (max (impulse - maxTrackingNoiseLevel) 0.0) * 1.0 / (1.0 - maxTrackingNoiseLevel) // -a, clamp, to 0..1

                        let velocityToStrength = 5.0
                        let constVibrationOffset = 0.2
                        let linearStrength = constVibrationOffset + velWithoutTrackingNoise * velocityToStrength

                        let clampedStrength = clamp 0.0 1.0 linearStrength
                        clampedStrength
                                      
                    let impulseStrength = contactNormal.Length
                    if impulseStrength > 0.01 then
                        let strength = hitImpulseToStrength(impulseStrength)
//                        printfn "impulseStrength %A -> strength %A" impulseStrength strength
                                    
                        if firstController then
                            if strength > newCtr1VibStrength then newCtr1VibStrength <- strength
                        else
                            if strength > newCtr2VibStrength then newCtr2VibStrength <- strength
                         
                { scene with 
                    interactionInfo1 = { scene.interactionInfo1 with vibrationStrength = newCtr1VibStrength }
                    interactionInfo2 = { scene.interactionInfo2 with vibrationStrength = newCtr2VibStrength }
                }
//                scene
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
                makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
            | _ -> scene
