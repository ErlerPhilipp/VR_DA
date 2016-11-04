namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

module LogicalScene =
    open VrTypes
    open VrDriver
        
    let virtualHandColor = Mod.init (C4f.White)

    type pset<'a> = PersistentHashSet<'a>
    type Object =
        {
            id              : int
            isManipulable   : bool
            isGrabbed       : bool
            wasGrabbed      : bool
            boundingBox     : Box3d
            trafo           : Trafo3d
            model           : ISg
            mass            : Mass
            collisionShape  : Option<Shape> 
        }

    type Scene =
        {
            things            : pset<Object>
            viewTrafo         : Trafo3d
            lastTrafo         : Trafo3d
            
            cam1Object        : Object
            cam2Object        : Object
            controller1Object : Object
            controller2Object : Object

            interactionType   : VrTypes.VrInteractionTechnique

            moveDirection     : V3d
        }

        
    type Message =
        | Add of Object
        | Remove of Object
        | DevicePress of int * int * Trafo3d
        | DeviceRelease of int * int * Trafo3d
        | DeviceTouch of int * int * Trafo3d
        | DeviceUntouch of int * int * Trafo3d
        | DeviceMove of int * Trafo3d
        | TimeElapsed of System.TimeSpan
        | UpdateViewTrafo of Trafo3d

    let change (i : int) (f : 'a -> 'a) (l : list<'a>) =
        l |> List.mapi (fun ii v -> if ii = i then f v else v)

    let filterIndices (set : Set<int>) (l : list<'a>) =
        let rec filterIndices (i : int) (set : Set<int>) (l : list<'a>) =
            match l with
                | [] -> []
                | h :: rest ->
                    if Set.contains i set then h :: filterIndices (i+1) set rest
                    else filterIndices (i+1) set rest
        filterIndices 0 set l

        
    let update (scene : Scene) (message : Message) : Scene =
        match message with
            | TimeElapsed _ | UpdateViewTrafo _ | DeviceMove _ | DeviceTouch _ | DevicePress _ | DeviceUntouch _ | DeviceRelease _ -> ()
            | _ -> printfn "%A" message

        let getVirtualHandTrafo (t : Trafo3d) = 
            if scene.interactionType = VrTypes.VrInteractionTechnique.VirtualHand then
                t
            else
                let handPos = t.Forward.TransformPos V3d.Zero
                let headPos = scene.viewTrafo.Backward.TransformPos V3d.Zero
                let chestPos = headPos |> Trafo3d.Translation(0.0, -0.2, 0.0).Forward.TransformPos
                let chestToHand = handPos - chestPos
                //printfn "hand: %A, chest: %A" (chestPos) (handPos)
                let headToHandDist = chestToHand.Length
                    
                let linearExtensionLimit = 0.5

                if headToHandDist < linearExtensionLimit then
                    t
                else
                    let quadraticTermFactor = 200.0
                    let quadraticExtension = headToHandDist - linearExtensionLimit
                    let gogoAdditionalExtension = max 0.0 quadraticTermFactor * quadraticExtension * quadraticExtension // R_r + k(R_r - D)^2
                    let gogoHandPosOffset = chestToHand.Normalized * gogoAdditionalExtension
                    let gogoHandTrafo = t * Trafo3d.Translation(gogoHandPosOffset)
                    //printfn "arm length: %A gogo arm length: %A, pos: %A" headToHandDist (1.0+gogoAdditionalExtension) (gogoHandTrafo.GetViewPosition())
                    gogoHandTrafo

        let scene =
            match message with
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                    { scene with 
                        controller1Object = {scene.controller1Object with trafo = t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller2Id ->
                    { scene with 
                        controller2Object = {scene.controller2Object with trafo = getVirtualHandTrafo t}
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
                let newInteractionTechnique = VrTypes.nextInteractionTechnique scene.interactionType
                transact ( fun _ -> Mod.change virtualHandColor (VrTypes.colorForInteractionTechnique newInteractionTechnique) )
                { scene with
                    interactionType = newInteractionTechnique
                }
            | DevicePress(deviceId, a, t) when deviceId = assignedInputs.controller2Id && a = 1 ->
                
                let trafo = 
                    if deviceId = assignedInputs.controller2Id then
                        scene.controller2Object.trafo
                    else
                        t
                
                let worldLocation = trafo.Forward.C3.XYZ

                let newThings = scene.things |> PersistentHashSet.map (fun o ->
                
                        if o.isManipulable then
                            let modelLocation = o.trafo.Backward.TransformPos worldLocation
                            if o.boundingBox.Contains modelLocation then
                                { o with isGrabbed = true}
                            else
                                o
                        else
                            o

                    ) 

                { scene with 
                    lastTrafo       = trafo
                    things          = newThings
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

                if PersistentHashSet.isEmpty scene.things then
                    scene
                else    
                    let deltaTrafo = scene.lastTrafo.Inverse * trafo
                    { scene with 
                        things =
                            scene.things |> PersistentHashSet.map (fun a ->
                                if a.isGrabbed then { a with trafo = a.trafo * deltaTrafo } else a
                            ) 
                        lastTrafo = trafo
                    }
                    
            | DeviceRelease(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 1 ->
                let newThings = scene.things |> PersistentHashSet.map (fun a ->
                        if not a.isGrabbed then a else { a with isGrabbed = false }
                    ) 

                { scene with 
                    things = newThings 
                    lastTrafo = Trafo3d.Identity
                }

            | TimeElapsed(dt) ->
                let maxSpeed = 10.0
                    
                let mutable state = VRControllerState_t()
                let axisPosition =
                    if system.GetControllerState(uint32 assignedInputs.controller1Id, &state) then
                        Some (V2d(state.[1].x, state.[1].y))
                    else None

                let axisValue = if axisPosition.IsSome then axisPosition.Value.X else 0.0

                let deathZone = 0.1
                let axisWithDeathZone = clamp 0.0 1.0 (axisValue * (1.0 + deathZone) - deathZone)
                //printfn "axisWithDeathZone: %A" axisWithDeathZone

                let dp = Trafo3d.Translation(scene.moveDirection * dt.TotalSeconds * maxSpeed * axisWithDeathZone)
                { scene with
                    // only move static things, keep active things like controllers
                    things = scene.things |> PersistentHashSet.map (fun o -> 
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
