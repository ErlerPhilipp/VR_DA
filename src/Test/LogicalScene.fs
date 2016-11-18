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
        | Static  // static collider, never moving
        | Dynamic // moved by physics
        | Ghost   // tracks its collisions
        // TODO: | Kinematic // not moved by physics but by game logic

    type pset<'a> = PersistentHashSet<'a>
    type Object =
        {
            id                : int
            
            objectType        : ObjectTypes
            isColliding       : bool
            isManipulable     : bool
            isGrabbable       : bool
            isGrabbed         : bool
            wasGrabbed        : bool

            trafo             : Trafo3d
            model             : ISg

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
            objectType    = ObjectTypes.Static
            isColliding   = true
            isManipulable = false
            isGrabbable   = false
            isGrabbed     = false
            wasGrabbed    = false
            trafo = Trafo3d.Identity
            model = Sg.group []
            mass = 0.0f
            restitution = 0.0f
            friction = 1.0f
            ccdSpeedThreshold = 0.0f // ccd disabled
            ccdSphereRadius = 0.0f
            rollingFriction = 0.0f
            collisionShape = None
        }

    type Scene =
        {
            objects             : pset<Object>
            viewTrafo           : Trafo3d
            lastContr2Trafo     : Trafo3d
            deviceOffset        : Trafo3d

            deltaTime           : float
            enablePhysics       : bool
            
            cam1ObjectId        : int
            cam2ObjectId        : int
            controller1ObjectId : int
            controller2ObjectId : int

            interactionType    : VrInteractions.VrInteractionTechnique
            armExtensionFactor : float

            physicsDebugDraw   : bool
            gravity            : V3d
            numSubSteps        : int
            subStepTime        : float

            moveDirection      : V3d
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
        | Collision of Object * Object
        
    let updateObjectsTrafoWithId(id : int, objects : pset<Object>, t : Trafo3d) = 
        let newObjects = objects |> PersistentHashSet.map (fun o -> 
            if o.id = id then
                {o with trafo = t}
            else
                o
            ) 
        newObjects

    let getObjectWithId(id : int, objects : pset<Object>) =
        let objectWithId = 
            objects |> PersistentHashSet.fold ( fun found current -> if current.id = id then Some current else found) None
        
        match objectWithId with
            | Some foundObject -> foundObject
            | None -> failwith(sprintf "Object with id %A not found!" id) 

    let update (scene : Scene) (message : Message) : Scene =

        match message with
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.hmdId ->
                { scene with viewTrafo = t.Inverse }
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                let newObjects = updateObjectsTrafoWithId(scene.controller1ObjectId, scene.objects, t)
                let direction = t.Forward.TransformDir(V3d.OOI)
                { scene with 
                    objects = newObjects
                    moveDirection = direction
                }
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller2Id ->
                let virtualHandTrafo, extension = VrInteractions.getVirtualHandTrafoAndExtensionFactor(t, scene.viewTrafo, scene.interactionType)
                let deltaTrafo = scene.lastContr2Trafo.Inverse * virtualHandTrafo
                let newObjects = updateObjectsTrafoWithId(scene.controller2ObjectId, scene.objects, virtualHandTrafo)
                                 |> PersistentHashSet.map (fun a ->
                                    if a.isGrabbed then { a with trafo = a.trafo * deltaTrafo } else a
                                 )
                { scene with 
                    objects = newObjects
                    armExtensionFactor = extension
                    lastContr2Trafo = virtualHandTrafo
                }
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam1Id ->
                let newObjects = updateObjectsTrafoWithId(scene.cam1ObjectId, scene.objects, t)
                { scene with objects = newObjects }
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam2Id ->
                let newObjects = updateObjectsTrafoWithId(scene.cam2ObjectId, scene.objects, t)
                { scene with objects = newObjects }

            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 0 ->
                let newInteractionTechnique = VrInteractions.nextInteractionTechnique scene.interactionType
                transact ( fun _ -> Mod.change virtualHandColor (VrInteractions.colorForInteractionTechnique newInteractionTechnique) )
                { scene with
                    interactionType = newInteractionTechnique
                }
            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller1Id && a = 0 ->
                { scene with
                    enablePhysics = not scene.enablePhysics
                }
            | DevicePress(deviceId, a, t) when deviceId = assignedInputs.controller2Id && a = 1 ->
                
                let newObjects = scene.objects |> PersistentHashSet.map (fun o ->
                        if o.isManipulable && o.isGrabbable then
                            { o with isGrabbed = true; }
                        else
                            o
                    ) 

                { scene with objects = newObjects }
                    
            | DeviceRelease(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 1 ->
                let newObjects = scene.objects |> PersistentHashSet.map (fun a ->
                        if not a.isGrabbed then a else { a with isGrabbed = false }
                    ) 

                { scene with 
                    objects = newObjects 
                    lastContr2Trafo = Trafo3d.Identity
                }

            | StartFrame ->
                let newObjects = scene.objects |> PersistentHashSet.map (fun o -> 
                        { o with 
                            isGrabbable = false
                        }
                    )

                { scene with objects = newObjects}

            | TimeElapsed(dt) ->
                let maxSpeed = 10.0
                    
                let mutable state = VRControllerState_t()
                let axisPosition =
                    if system.GetControllerState(uint32 assignedInputs.controller1Id, &state) then
                        Some (V2d(state.[1].x, state.[1].y))
                    else None

                let axisValue = if axisPosition.IsSome then axisPosition.Value.X else 0.0

                let axisWithDeathZone = VrInteractions.getAxisValueWithDeathZone(axisValue)

                let dp = Trafo3d.Translation(scene.moveDirection * dt.TotalSeconds * maxSpeed * axisWithDeathZone)
                let newObjects = scene.objects |> PersistentHashSet.map (fun o -> 
                        { o with 
                            wasGrabbed = o.isGrabbed
                        }
                    )
                let newSceneTrafo = scene.deviceOffset * dp

                { scene with
                    objects = newObjects
                    deviceOffset = newSceneTrafo
                    deltaTime = dt.TotalSeconds
                }
            
            | Collision (ghost, collider) ->
                let newObjects = scene.objects |> PersistentHashSet.map (fun o -> 
                        let collidingWithController = ghost.id = scene.controller2ObjectId && o.id = collider.id 
                        //if collidingWithController then printfn "Ghost %A collides with %A" ghost.id collider.id
                        if collidingWithController then { o with isGrabbable = true } else o
                    )
                { scene with objects = newObjects }

            | _ -> scene
