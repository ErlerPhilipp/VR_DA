namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

module LogicalScene =
    open VrTypes
    open VrInteractions
    open VrDriver
        
    let virtualHandColor = Mod.init (C4f.White)

    let mutable currentId = 0
    let newId() = 
        currentId <- currentId + 1
        currentId

    type ObjectTypes = 
        | Static        // static collider, never moving
        | Dynamic       // moved by physics
        | Ghost         // tracks its collisions
        | Kinematic     // not moved by physics but by game logic

    type pset<'a> = PersistentHashSet<'a>
    type Object =
        {
            id                : int
            
            castsShadow       : bool
            objectType        : ObjectTypes
            isColliding       : bool
            isManipulable     : bool
            isGrabbable       : bool
            isGrabbed         : bool
            wasGrabbed        : bool
            hitUpperTrigger   : bool
            hitLowerTrigger   : bool
            hasScored         : bool
            willReset         : bool
            timeToReset       : float

            trafo             : Trafo3d
            model             : ISg
            tilingFactor      : V2d

            linearVelocity    : V3d
            angularVelocity   : V3d
            mass              : float32
            restitution       : float32
            friction          : float32
            ccdSpeedThreshold : float32
            ccdSphereRadius   : float32
            rollingFriction   : float32
            collisionShape    : Option<BulletHelper.Shape> 
        }

    let defaultObject = 
        {
            id = newId()

            castsShadow         = true
            objectType          = ObjectTypes.Static
            isColliding         = true
            isManipulable       = false
            isGrabbable         = false
            isGrabbed           = false
            wasGrabbed          = false
            hitUpperTrigger     = false
            hitLowerTrigger     = false
            hasScored           = false
            willReset           = false
            timeToReset         = 0.0

            trafo               = Trafo3d.Identity
            model               = Sg.group []
            tilingFactor        = V2d(1.0, 1.0)
            
            linearVelocity      = V3d()
            angularVelocity     = V3d()
            mass                = 0.0f
            restitution         = 0.0f
            friction            = 1.0f
            ccdSpeedThreshold   = 0.0f // ccd disabled
            ccdSphereRadius     = 0.0f
            rollingFriction     = 0.0f
            collisionShape      = None
        }

    type SpecialObjectIds =
        {
            cam1ObjectId        : int
            cam2ObjectId        : int
            controller1ObjectId : int
            controller2ObjectId : int
            headId              : int
            lowerHoopTriggerId  : int
            upperHoopTriggerId  : int
            lightId             : int
            groundObjectId      : int
        }

    type InteractionInfo =
        {
            interactionType     : VrInteractions.VrInteractionTechnique
            armExtensionFactor  : float
            movementType        : VrInteractions.VrMovementTechnique
            moveDirection       : V3d
        }

    let DefaultInteractionInfo = 
        {
            interactionType     = VrInteractions.VrInteractionTechnique.VirtualHand
            armExtensionFactor  = 1.0
            movementType        = VrInteractions.VrMovementTechnique.Flying
            moveDirection       = V3d.Zero
        }

    type GameInfo = 
        {
            score               : int
            timeSinceStart      : float
            scoreTrafo          : Trafo3d
            scoreText           : string
            running             : bool
        }

    let DefaultGameInfo (scoreTrafo : Trafo3d) = 
        {
            score               = 0
            timeSinceStart      = 0.0
            scoreTrafo          = scoreTrafo
            scoreText           = "Score: 000\r\nTime: 000.00"
            running             = false
        }

    type PhysicsInfo = 
        {
            deltaTime           : float
            enablePhysics       : bool
            physicsDebugDraw    : bool
            gravity             : V3d
            numSubSteps         : int
            subStepTime         : float
        }

    let DefaultPhysicsInfo = 
        {
            deltaTime           = 0.0
            enablePhysics       = true
            gravity             = V3d(0.0, -9.81, 0.0)
            physicsDebugDraw    = false
            numSubSteps         = 3
            subStepTime         = 1.0 / 180.0
        }

    type RaycastInfo = 
        {
            wantsRayCast        : bool
            rayCastStart        : V3d
            rayCastEnd          : V3d
            rayCastHasHit       : bool
            rayCastHitPoint     : V3d
            rayCastHitNormal    : V3d
            rayCastHitAreaSg    : ISg
            rayCastHitPointSg   : ISg
            rayCastCamSg        : ISg
        }

    let DefaultRaycastInfo (rayCastHitPointSg : ISg, rayCastHitAreaSg : ISg, rayCastCamSg : ISg) = 
        {
            wantsRayCast        = false
            rayCastStart        = V3d()
            rayCastEnd          = V3d()
            rayCastHasHit       = false
            rayCastHitPoint     = V3d()
            rayCastHitNormal    = V3d()
            rayCastHitPointSg   = rayCastHitPointSg
            rayCastHitAreaSg    = rayCastHitAreaSg
            rayCastCamSg        = rayCastCamSg
        }

    type VibrationInfo = 
        {
            ctr1VibStr          : float
            ctr1VibStrLastFrame : float
            ctr2VibStr          : float
            ctr2VibStrLastFrame : float
        }

    let DefaultVibrationInfo = 
        {
            ctr1VibStr          = 0.0
            ctr1VibStrLastFrame = 0.0
            ctr2VibStr          = 0.0
            ctr2VibStrLastFrame = 0.0
        }

    type Scene =
        {
            objects             : pset<Object>
            viewTrafo           : Trafo3d
            lastContr2Trafo     : Trafo3d
            trackingToWorld     : Trafo3d
            
            specialObjectIds    : SpecialObjectIds
            interactionInfo     : InteractionInfo
            gameInfo            : GameInfo
            physicsInfo         : PhysicsInfo
            raycastInfo         : RaycastInfo
            vibrationInfo       : VibrationInfo
        }
        
    type Message =
        | DevicePress of int * int * Trafo3d
        | DeviceRelease of int * int * Trafo3d
        | DeviceTouch of int * int * Trafo3d
        | DeviceUntouch of int * int * Trafo3d
        | DeviceMove of int * Trafo3d
        | StartFrame
        | TimeElapsed of System.TimeSpan
        | UpdateViewTrafo of Trafo3d
        | Collision of int * int
        | RayCastResult of bool * V3d * V3d
        
    let setTrafoOfObjectsWithId(id : int, t : Trafo3d, objects : pset<Object>, dt : float) = 
        let newObjects = objects |> PersistentHashSet.map (fun o -> 
            if o.id = id then
                let newTrafo = t
                if o.objectType = ObjectTypes.Ghost then
                    let linVel = (newTrafo.Forward.TransformPos(V3d()) - o.trafo.Forward.TransformPos(V3d())) / dt
                    {o with 
                        trafo = newTrafo
                        linearVelocity = linVel
                    }
                else
                    {o with trafo = newTrafo}
            else
                o
            ) 
        newObjects

    let transformTrafoOfObjectsWithId(id : int, t : Trafo3d, objects : pset<Object>, dt : float) = 
        let newObjects = objects |> PersistentHashSet.map (fun o -> 
            if o.id = id then
                let newTrafo = o.trafo * t
                if o.objectType = ObjectTypes.Ghost then
                    let linVel = (newTrafo.Forward.TransformPos(V3d()) - o.trafo.Forward.TransformPos(V3d())) / dt
                    {o with 
                        trafo = newTrafo
                        linearVelocity = linVel
                    }
                else
                    {o with trafo = newTrafo}
            else
                o
            ) 
        newObjects

    let getTrafoOfFirstObjectWithId(id : int, objects : pset<Object>) = 
        let filteredObjects = objects |> PersistentHashSet.filter (fun o -> 
                o.id = id
            )
        if PersistentHashSet.isEmpty filteredObjects then
            failwith "Id not found!"
        else
            (PersistentHashSet.toList filteredObjects).[0].trafo

    let getObjectWithId(id : int, objects : pset<Object>) =
        let objectWithId = 
            objects |> PersistentHashSet.fold ( fun found current -> if current.id = id then Some current else found) None
        
        match objectWithId with
            | Some foundObject -> foundObject
            | None -> failwith(sprintf "Object with id %A not found!" id) 
            
    let update (scene : Scene) (message : Message) : Scene =

        match message with
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.hmdId ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.headId, t, scene.objects, scene.physicsInfo.deltaTime)
                { scene with 
                    objects = newObjects
                    viewTrafo = t.Inverse
                }
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.controller1ObjectId, t, scene.objects, scene.physicsInfo.deltaTime)
                let direction = t.Forward.TransformDir(V3d.OOI)
                let rayCastStart = t.Forward.TransformPos(V3d())
                
                if scene.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportArea || scene.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos then
                    { scene with 
                        objects = newObjects
                        interactionInfo = { scene.interactionInfo with moveDirection = direction }
                        raycastInfo = { scene.raycastInfo with
                                            wantsRayCast = true
                                            rayCastStart = rayCastStart
                                            rayCastEnd = rayCastStart + -100.0 * direction
                                      }
                    }
                else
                    { scene with 
                        objects = newObjects
                        interactionInfo = { scene.interactionInfo with moveDirection = direction }
                    }

            | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller2Id ->
                match scene.interactionInfo.interactionType with
                    | VrInteractionTechnique.VirtualHand ->
                        let virtualHandTrafo = t
                        let deltaTrafo = scene.lastContr2Trafo.Inverse * virtualHandTrafo
                        let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.controller2ObjectId, virtualHandTrafo, scene.objects, scene.physicsInfo.deltaTime)
                                         |> PersistentHashSet.map (fun a ->
                                            if a.isGrabbed then { a with trafo = a.trafo * deltaTrafo } else a
                                         )
                        { scene with 
                            objects = newObjects
                            interactionInfo = { scene.interactionInfo with armExtensionFactor = 1.0 }
                            lastContr2Trafo = virtualHandTrafo
                        }
                    | VrInteractionTechnique.GoGo ->
                        let virtualHandTrafo, extension = VrInteractions.getVirtualHandTrafoAndExtensionFactor(t, scene.viewTrafo, scene.trackingToWorld)
                        let virtualHandPos = virtualHandTrafo.Forward.TransformPos(V3d.OOO)
                        let deltaTrafo = scene.lastContr2Trafo.Inverse * virtualHandTrafo
                        let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.controller2ObjectId, virtualHandTrafo, scene.objects, scene.physicsInfo.deltaTime)
                                         |> PersistentHashSet.map (fun a ->
                                            if a.isGrabbed then { a with trafo = a.trafo * deltaTrafo } else a
                                         )

        //                // attach light to grabbing hand
        //                let lightPos = virtualHandTrafo
        //                let newObjects = setTrafoOfObjectsWithId(scene.lightId, lightPos, newObjects)

                        { scene with 
                            objects = newObjects
                            interactionInfo = { scene.interactionInfo with armExtensionFactor = extension }
                            lastContr2Trafo = virtualHandTrafo
                        }
                    | _ -> failwith "Not implemented"
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam1Id ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.cam1ObjectId, t, scene.objects, scene.physicsInfo.deltaTime)
                { scene with objects = newObjects }
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam2Id ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.cam2ObjectId, t, scene.objects, scene.physicsInfo.deltaTime)
                { scene with objects = newObjects }

            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 0 ->
                let newInteractionTechnique = VrInteractions.nextInteractionTechnique scene.interactionInfo.interactionType
                transact ( fun _ -> Mod.change virtualHandColor (VrInteractions.colorForInteractionTechnique newInteractionTechnique) )
                { scene with
                    interactionInfo = { scene.interactionInfo with interactionType = newInteractionTechnique }
                }
            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller1Id && a = 0 ->
                let newMovementTechnique = VrInteractions.nextMovementTechnique scene.interactionInfo.movementType
                { scene with
                    interactionInfo = { scene.interactionInfo with movementType = newMovementTechnique }
                    raycastInfo = { scene.raycastInfo with 
                                        wantsRayCast = false
                                        rayCastHasHit = false
                                  }
                }
            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller1Id && a = 2 ->
                { scene with
                    physicsInfo = { scene.physicsInfo with enablePhysics = not scene.physicsInfo.enablePhysics }
                }
            | DeviceTouch(deviceId, a, t) when deviceId = assignedInputs.controller1Id && a = 1 ->
                if (scene.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportArea || 
                    scene.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos) && scene.raycastInfo.rayCastHasHit then
                    let hmdTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.headId, scene.objects)
                    let recenter = scene.interactionInfo.movementType = VrInteractions.VrMovementTechnique.TeleportPos
                    let newTrackingToWorld = VrInteractions.getTeleportTrafo(scene.trackingToWorld, hmdTrafo, scene.raycastInfo.rayCastHitPoint, scene.raycastInfo.rayCastHitNormal, recenter)
                    { scene with 
                        trackingToWorld = newTrackingToWorld 
                        physicsInfo = { scene.physicsInfo with gravity = newTrackingToWorld.Forward.TransformDir(V3d(0.0, -9.81, 0.0)) }
                    }
                else
                    scene
                    
            | DeviceTouch(deviceId, a, t) when deviceId = assignedInputs.controller2Id && a = 1 ->
                let newObjects = 
                    scene.objects 
                        |> PersistentHashSet.map (fun o ->
                                if o.isManipulable && o.isGrabbable then { o with isGrabbed = true; } else o
                            ) 
                        |> PersistentHashSet.map (fun a ->
                                if a.hitLowerTrigger then { a with hitLowerTrigger = false } else a
                            ) 
                        |> PersistentHashSet.map (fun a ->
                                if a.hitUpperTrigger then { a with hitUpperTrigger = false }  else a
                            )
                        |> PersistentHashSet.map (fun a ->
                                if a.hasScored then { a with hasScored = false }  else a
                            )

//                VrDriver.vibrate(uint32 assignedInputs.controller2Id, 1000000)

                { scene with objects = newObjects }
                    
            | DeviceUntouch(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 1 ->
                let newObjects = 
                    scene.objects 
                        |> PersistentHashSet.map (fun a ->
                                if not a.isGrabbed then a else { a with isGrabbed = false }
                            ) 

                { scene with 
                    objects = newObjects 
                    lastContr2Trafo = Trafo3d.Identity
                }

            | StartFrame ->
                let newObjects = 
                    scene.objects 
                        |> PersistentHashSet.map (fun o -> 
                                if o.isGrabbable <> false then { o with isGrabbable = false } else o
                            )
                        |> PersistentHashSet.map (fun o -> 
                                let reset = o.willReset && (scene.gameInfo.timeSinceStart > o.timeToReset)
                                if reset then
                                    //printfn "Ball reset at %A" scene.timeSinceStart 
                                    { o with 
                                        hasScored = false
                                        hitLowerTrigger = false
                                        hitUpperTrigger = false
                                        isGrabbed = false
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
                    vibrationInfo = { scene.vibrationInfo with 
                                        ctr1VibStr = 0.0
                                        ctr2VibStr = 0.0
                                    }
                }

            | TimeElapsed(dt) ->
                let mutable state = VRControllerState_t()
                let axisPosition =
                    if system.GetControllerState(uint32 assignedInputs.controller1Id, &state) then
                        Some (V2d(state.[1].x, state.[1].y))
                    else None

                let axisValue = if axisPosition.IsSome then axisPosition.Value.X else 0.0

                let newObjects = scene.objects |> PersistentHashSet.map (fun o -> 
                        { o with 
                            wasGrabbed = o.isGrabbed
                        }
                    ) 
                let newObjects = newObjects |> PersistentHashSet.map (fun o -> 
                        if o.trafo.Forward.TransformPos(V3d()).Y < -100.0 then
                            { o with 
                                willReset = true
                                timeToReset = scene.gameInfo.timeSinceStart 
                            } 
                        else
                            o
                    )

                let newTrackingToWorld = 
                    if scene.interactionInfo.movementType = VrInteractions.VrMovementTechnique.Flying then
                        VrInteractions.getTrafoAfterFlying(scene.trackingToWorld, scene.interactionInfo.moveDirection, dt.TotalSeconds, axisValue)
                    else
                        scene.trackingToWorld
                
//                let lightRotation = Trafo3d.RotationYInDegrees(90.0 * dt.TotalSeconds)
//                let newObjects = transformTrafoOfObjectsWithId(scene.lightId, lightRotation, newObjects)

                let newTimeSinceStart = scene.gameInfo.timeSinceStart + dt.TotalSeconds

                let axisPosition =
                    if system.GetControllerState(uint32 assignedInputs.controller2Id, &state) then
                        Some (V2d(state.[1].x, state.[1].y))
                    else None
                let axisValue = if axisPosition.IsSome then axisPosition.Value.X else 0.0
//                let duration = uint16 (axisValue * 1999.0)
//                printfn "%A: duration: %A" newTimeSinceStart duration

                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller1Id)
                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller2Id)

                // hit object
                if scene.vibrationInfo.ctr2VibStrLastFrame = 0.0 && scene.vibrationInfo.ctr2VibStr <> 0.0 then
                    Vibration.vibrate(Vibration.HitObject, uint32 assignedInputs.controller2Id, 0.1, 0.5)
                    
//                Vibration.vibrate(Vibration.OverlappingObject, uint32 assignedInputs.controller1Id, int 1000, axisValue)
                Vibration.vibrate(Vibration.OverlappingObject, uint32 assignedInputs.controller2Id, 1.0, scene.vibrationInfo.ctr2VibStr)
                
                Vibration.updateVibration(uint32 assignedInputs.controller1Id, dt.TotalSeconds)
                Vibration.updateVibration(uint32 assignedInputs.controller2Id, dt.TotalSeconds)

                { scene with
                    objects = newObjects
                    trackingToWorld = newTrackingToWorld
                    physicsInfo = { scene.physicsInfo with deltaTime = dt.TotalSeconds }
                    gameInfo = { scene.gameInfo with timeSinceStart = newTimeSinceStart }
                    vibrationInfo = { scene.vibrationInfo with 
                                        ctr1VibStrLastFrame = scene.vibrationInfo.ctr1VibStr
                                        ctr2VibStrLastFrame = scene.vibrationInfo.ctr2VibStr
                                    }
                }
            
            | Collision (ghostId, colliderId) ->
                let mutable newScore = scene.gameInfo.score
                let mutable newCtr2VibStrength = scene.vibrationInfo.ctr2VibStr
                let newObjects = 
                    scene.objects 
                        // hit upper hoop trigger
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithUpperHoop = ghostId = scene.specialObjectIds.upperHoopTriggerId && o.id = colliderId
                                let hitUpperTrigger = collidingWithUpperHoop && not o.hitUpperTrigger && not o.hitLowerTrigger && not o.isGrabbed && o.linearVelocity.Y < 0.0
                                if hitUpperTrigger then
                                    //printfn "hit upper trigger at %A" scene.timeSinceStart
                                    { o with hitUpperTrigger = true } 
                                else 
                                    o
                            )
                        // hit lower hoop trigger
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithLowerHoop = ghostId = scene.specialObjectIds.lowerHoopTriggerId && o.id = colliderId
                                let hitLowerTrigger = collidingWithLowerHoop && not o.hitLowerTrigger && not o.isGrabbed
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
                        // check grabbable
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithController = ghostId = scene.specialObjectIds.controller2ObjectId && o.id = colliderId
                                if collidingWithController then
                                    let ghostLinearVelocity = getObjectWithId(ghostId, scene.objects).linearVelocity
                                    let colliderLinearVelocity = o.linearVelocity
                                    let relativeVel = (ghostLinearVelocity - colliderLinearVelocity).Length

                                    let velToStrength(vel : float) = 
                                        let maxTrackingNoiseLevel = 0.1
                                        let velWithoutTrackingNoise = (max (vel - maxTrackingNoiseLevel) 0.0) * 1.0 / (1.0 - maxTrackingNoiseLevel) // -a, clamp, to 0..1

                                        let velocityToStrength = 0.6
                                        let constVibrationOffset = 0.1
                                        let linearStrength = constVibrationOffset + velWithoutTrackingNoise * velocityToStrength

                                        let clampedStrength = clamp 0.0 1.0 linearStrength
                                        clampedStrength

                                    let strength = velToStrength(relativeVel)
//                                     printfn "ghostLinearVelocity: %A, colliderLinearVelocity: %A, strength %A" (ghostLinearVelocity.Length) (colliderLinearVelocity.Length) strength
                                    if strength > newCtr2VibStrength then newCtr2VibStrength <- strength

                                    { o with isGrabbable = true } 
                                else 
                                    o
                            )
                        // check reset on ground
                        |> PersistentHashSet.map (fun o -> 
                                let collidingWithGround = ghostId = scene.specialObjectIds.groundObjectId && o.id = colliderId && o.isManipulable && not o.willReset && not o.isGrabbed
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
                    gameInfo = 
                        { scene.gameInfo with
                            score = newScore
                        }
                    vibrationInfo = 
                        { scene.vibrationInfo with
                            ctr2VibStr = newCtr2VibStrength
                        }
                }
            | RayCastResult (hasHit, hitPoint, hitNormal) ->
                { scene with 
                    raycastInfo = 
                        { scene.raycastInfo with
                            rayCastHasHit = hasHit
                            rayCastHitPoint = hitPoint
                            rayCastHitNormal = hitNormal
                            wantsRayCast = false
                        }
                }
            | _ -> scene
