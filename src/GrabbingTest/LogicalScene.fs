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
    
    let seed = 42
    let mutable seededRandomNumberGen = System.Random(seed)

    let makeSceneWithInteractionInfo(firstController : bool, newInteractionInfo : InteractionInfo, scene : Scene) =
        if firstController then
            { scene with interactionInfo1 = newInteractionInfo}
        else
            { scene with interactionInfo2 = newInteractionInfo}
            
    let getBallIndex(ballId : int, scene : Scene) = 
        array.FindIndex (scene.specialObjectIds.ballObjectIds, (fun i -> i = ballId))

    let resetBallObject(ballIndex : int, scene : Scene, ballObject : Object) = 
            if ballObject.id = scene.specialObjectIds.ballObjectIds.[ballIndex] then
                scene.popSoundSource.Location <- scene.ballResetPos.[ballIndex]
                scene.popSoundSource.Play()
                { ballObject with 
                    hasScored = false
                    hitLowerTrigger = false
                    hitUpperTrigger = false
                    isGrabbed = GrabbedOptions.NoGrab
                    trafo = Trafo3d.Translation(scene.ballResetPos.[ballIndex])
                    linearVelocity = V3d()
                    angularVelocity = V3d()
                }
            else
                ballObject

    let resetBall(ballIndex : int, scene : Scene, objects : PersistentHashSet<Object>) = 
        objects 
            |> PersistentHashSet.map (fun o -> 
                    resetBallObject(ballIndex, scene, o)
                )

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

                let newObjects = updateControllerObjects(t, scene.objects)
                let newObjects = updateGrabbedObjects(t, newObjects)
                let newInteractionInfo = { interactionInfo with lastContrTrafo = t }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                { newScene with objects = newObjects}
                 
            // press trigger
            | DeviceTouch(deviceId, a, t) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trigger) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                        
                let scene = if not interactionInfo.triggerPressed then
                                    let newInteractionInfo = { interactionInfo with triggerPressed = true}
                                    makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                                else
                                    scene

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
                                    } 
                                else o
                            ) 
                { scene with objects = newObjects }
                    
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

                let newGameInfo = scene.gameInfo
                let newObjects = scene.objects

                let timePerRound = 30.0 
                let remainingTime = timePerRound - newGameInfo.timeSinceStart
                let (newObjects, newGameInfo) =   
                    if remainingTime < 0.0 && newGameInfo.running then
                        Logging.log (newGameInfo.timeSinceStart.ToString() + ": Time's up!")
                                        
                        let newRound = newGameInfo.numRounds + 1
                        let lastRound = 5

                        let (newObjects, newGameInfo) =
                            if newRound = lastRound then
                                seededRandomNumberGen <- System.Random(seed)
                                (newObjects, {newGameInfo with 
                                                warmupScore = 0
                                                numRounds = 0
                                                running = false
                                                timeSinceStart = 0.0
                                             }
                                )
                            else
                                (newObjects, {newGameInfo with numRounds = newRound})
                        
                        if not (scene.sireneSoundSource.IsPlaying()) then scene.sireneSoundSource.Play()
                        (newObjects, { newGameInfo with timeSinceStart = 0.0 })
                    else
                        (newObjects, { newGameInfo with timeSinceStart = newGameInfo.timeSinceStart + dt })
                            
                let culture = System.Globalization.CultureInfo.CreateSpecificCulture("en-US")
                let newText =   if not newGameInfo.running then 
                                    let pointsUntilStart = 3
                                    let remainingScoreString = ((pointsUntilStart - newGameInfo.warmupScore).ToString("0", culture))
                                    let scoreString = (newGameInfo.score.ToString("000", culture))
                                    "Score:    " + scoreString + "\r\n" + 
                                    "Start in:    " + remainingScoreString
                                else
                                    let remainingTimeString = (remainingTime.ToString("000", culture))
                                    let scoreString = (newGameInfo.score.ToString("000", culture))
//                                    let timeString = (newGameInfo.timeSinceStart.ToString("000.00", culture))
                                    "Score:    " + scoreString + "\r\n" + 
                                    "Time:    " + remainingTimeString
                                    
                let newGameInfo = {newGameInfo with scoreText = newText}

                { scene with
                    objects = newObjects
                    gameInfo = newGameInfo
                    physicsInfo = { scene.physicsInfo with deltaTime = dt }
                }
            
            | EndFrame ->
                let mutable newObjects = scene.objects |> PersistentHashSet.map (fun o -> 
                        if o.isManipulable then
                            { o with 
                                wasGrabbed = o.isGrabbed
                            }
                        else o
                    )
                // reset balls below the ground
                let mutable ballIndex = 0
                for id in scene.specialObjectIds.ballObjectIds do
                    let ballObject = getObjectWithId(id, newObjects)
                    let ballPos = ballObject.trafo.Forward.TransformPos(V3d())
                    if ballPos.Y < -10.0 then
                        newObjects <- resetBall(ballIndex, scene, newObjects)
                    ballIndex <- ballIndex + 1

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
                let mutable hasScored = false
                let mutable newCtr1VibStrength = scene.interactionInfo1.vibrationStrength
                let mutable newCtr2VibStrength = scene.interactionInfo2.vibrationStrength
                let newObjects = 
                    scene.objects 
                        // hit upper hoop trigger
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithUpperHoop = ghostId = scene.specialObjectIds.upperHoopTriggerId && o.id = colliderId
                                let hitUpperTrigger = collidingWithUpperHoop && not o.hitUpperTrigger && not o.hitLowerTrigger && o.isGrabbed = GrabbedOptions.NoGrab && o.linearVelocity.Y < 0.0
                                if hitUpperTrigger && o.isManipulable then
                                    { o with hitUpperTrigger = true }
                                else 
                                    o
                            )
                        // hit lower hoop trigger
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithLowerHoop = ghostId = scene.specialObjectIds.lowerHoopTriggerId && o.id = colliderId
                                let hitLowerTrigger = collidingWithLowerHoop && not o.hitLowerTrigger && o.isGrabbed = GrabbedOptions.NoGrab
                                if hitLowerTrigger && o.isManipulable then
                                    { o with hitLowerTrigger = true } 
                                else 
                                    o
                            )
                        // check score
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithLowerHoop = ghostId = scene.specialObjectIds.lowerHoopTriggerId && o.id = colliderId
                                let scored = collidingWithLowerHoop && o.hitLowerTrigger && o.hitUpperTrigger && not o.hasScored && o.linearVelocity.Y < 0.0
                                if scored then
                                    let isTargetBall = o.isManipulable && getBallIndex(o.id, scene) = scene.gameInfo.targetBallIndex
                                    if isTargetBall then 
                                        hasScored <- true
                                        scene.sireneSoundSource.Play()
                                        newGameInfo <-  if newGameInfo.running then 
                                                            {newGameInfo with score = newGameInfo.score + 1}
                                                        else
                                                            {newGameInfo with warmupScore = newGameInfo.warmupScore + 1}
                                        Logging.log (newGameInfo.timeSinceStart.ToString() + ": Scored")
                                        let scoreUntilStart = 3
                                        if not newGameInfo.running && newGameInfo.warmupScore = scoreUntilStart then
                                            newGameInfo <- {newGameInfo with running = true; timeSinceStart = 0.0; score = 0}
                                            Logging.log (newGameInfo.timeSinceStart.ToString() + ": Warm-up finished, starting round " + newGameInfo.numRounds.ToString())
                                        Vibration.stopVibration(Vibration.Score, uint32 assignedInputs.controller1Id)
                                        Vibration.stopVibration(Vibration.Score, uint32 assignedInputs.controller2Id)
                                        Vibration.sinusiodFunctionPulses(3, 15, 0.3, Vibration.Score, uint32 assignedInputs.controller1Id, 1.0)
                                        Vibration.sinusiodFunctionPulses(3, 15, 0.3, Vibration.Score, uint32 assignedInputs.controller2Id, 1.0)
                                    
                                        { o with 
                                            hasScored = true
                                        } 
                                    else
                                        Logging.log (newGameInfo.timeSinceStart.ToString() + ": Wrong ball scored!")
                                        { o with 
                                            hasScored = true
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
                                    if colliderObject.isManipulable then
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

                let newObjects = 
                    if hasScored then
                        // reset all
                        let mutable mutableNewObjects = newObjects
                        for ballIndex in 0..scene.specialObjectIds.ballObjectIds.Length-1 do
                            mutableNewObjects <- resetBall(ballIndex, scene, mutableNewObjects)

                        mutableNewObjects
                    else
                        newObjects

                // new target ball
                let newTargetBallIndex = 
                    if hasScored then
                        seededRandomNumberGen.Next(scene.specialObjectIds.ballObjectIds.Length - 1)
                    else
                        scene.gameInfo.targetBallIndex

                // update visibility of target balls
                let newObjects =
                    if hasScored then
                        newObjects
                            |> PersistentHashSet.map (fun o -> 
                                                            let isStaticBall = array.Exists (scene.specialObjectIds.staticBallObjectIds, (fun id -> o.id = id ))
                                                            let isTargetBall = o.id = scene.specialObjectIds.staticBallObjectIds.[newTargetBallIndex]
                                                            if isStaticBall then
                                                                { o with visible = isTargetBall }
                                                            else
                                                                o
                                                        )

                    else
                        newObjects


                { scene with 
                    objects = newObjects
                    gameInfo = { newGameInfo with targetBallIndex = newTargetBallIndex }
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
                scene
            | _ -> scene
