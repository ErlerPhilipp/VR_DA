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

    type pset<'a> = PersistentHashSet<'a>
    type Object =
        {
            id                : int

            isManipulable     : bool
            isGrabbed         : bool
            wasGrabbed        : bool

            boundingBox       : Box3d

            trafo             : Trafo3d
            model             : ISg

            mass              : Mass
            restitution       : float
            collisionShape    : Option<BulletHelper.Shape> 
        }

    type Scene =
        {
            objects           : pset<Object>
            viewTrafo         : Trafo3d
            lastViewTrafo     : Trafo3d
            
            cam1Object        : Object
            cam2Object        : Object
            controller1Object : Object
            controller2Object : Object

            interactionType   : VrInteractions.VrInteractionTechnique

            physicsDebugDraw  : bool
            gravity           : V3d

            moveDirection     : V3d
        }

        

    let update (scene : Scene) (message : Message) : Scene =

        let scene =
            match message with
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                    { scene with 
                        controller1Object = {scene.controller1Object with trafo = t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller2Id ->
                    { scene with 
                        controller2Object = {scene.controller2Object with trafo = VrInteractions.getVirtualHandTrafo(t, scene.viewTrafo, scene.interactionType)}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam1Id ->
                    { scene with 
                        cam1Object = {scene.cam1Object with trafo = t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam2Id ->
                    { scene with 
                        cam2Object = {scene.cam2Object with trafo = t}
                    }
                | _ -> 
                    scene
        
        match message with
            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 0 ->
                let newInteractionTechnique = VrInteractions.nextInteractionTechnique scene.interactionType
                transact ( fun _ -> Mod.change virtualHandColor (VrInteractions.colorForInteractionTechnique newInteractionTechnique) )
                { scene with
                    interactionType = newInteractionTechnique
                }
            | DevicePress(deviceId, a, t) when deviceId = assignedInputs.controller2Id && a = 1 ->
                
                let trafo = 
                    if deviceId = assignedInputs.controller2Id then
                        scene.controller2Object.trafo
                    else
                        t

                let newObjects = scene.objects |> PersistentHashSet.map (fun o ->
                        if o.isManipulable then
                            let modelLocation = o.trafo.Backward.TransformPos trafo.Forward.C3.XYZ
                            if o.boundingBox.Contains modelLocation then
                                { o with isGrabbed = true}
                            else
                                o
                        else
                            o
                    ) 

                { scene with 
                    lastViewTrafo   = trafo
                    objects         = newObjects
                }
                    
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                let direction = t.Forward.TransformDir(V3d.OOI)
                { scene with moveDirection = direction }
            | DeviceMove(deviceId, t) ->
                let trafo = 
                    if deviceId = assignedInputs.controller2Id then
                        scene.controller2Object.trafo
                    else
                        t

                if PersistentHashSet.isEmpty scene.objects then
                    scene
                else    
                    let deltaTrafo = scene.lastViewTrafo.Inverse * trafo
                    { scene with 
                        objects =
                            scene.objects |> PersistentHashSet.map (fun a ->
                                if a.isGrabbed then { a with trafo = a.trafo * deltaTrafo } else a
                            ) 
                        lastViewTrafo = trafo
                    }
                    
            | DeviceRelease(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 1 ->
                let newObjects = scene.objects |> PersistentHashSet.map (fun a ->
                        if not a.isGrabbed then a else { a with isGrabbed = false }
                    ) 

                { scene with 
                    objects = newObjects 
                    lastViewTrafo = Trafo3d.Identity
                }

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
                { scene with
                    // only move static objects, keep active objects like controllers
                    objects = scene.objects |> PersistentHashSet.map (fun o -> 
                        let newTrafo = if o.isGrabbed then o.trafo else o.trafo * dp
                        { o with 
                            trafo = newTrafo; 
                            wasGrabbed = o.isGrabbed 
                        }
                    )
                }

            | UpdateViewTrafo trafo -> 
                { scene with viewTrafo = trafo }

            | _ -> scene
