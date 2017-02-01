﻿namespace Aardvark.VR

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

    let makeSceneWithInteractionInfo(firstController : bool, newInteractionInfo : InteractionInfo, scene : Scene) =
        if firstController then
            { scene with interactionInfo1 = newInteractionInfo}
        else
            { scene with interactionInfo2 = newInteractionInfo}

    let addTrafoToSelectionVolumePath(interactionInfo : InteractionInfo, t : Trafo3d, scene : Scene) =
        if interactionInfo.trackpadPressed then
            let newPos = t.Forward.TransformPos(V3d())
            let hasPosNearNewPos = interactionInfo.selectionVolumePath |> Array.exists (fun t -> (t.Forward.TransformPos(V3d()) - newPos).Length < 0.001)
            if hasPosNearNewPos then
//                printfn "has sel vol pos, count = %A" (Array.length interactionInfo.selectionVolumePath)
                interactionInfo.selectionVolumePath
            else
//                printfn "add sel vol pos, count = %A" (Array.length interactionInfo.selectionVolumePath + 1)
                let centroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
                let worldToPointcloud = (scene.pointCloudTrafo * centroidTrafo).Inverse
                let pos = t.Forward.TransformPos(V3d())
                Array.append interactionInfo.selectionVolumePath [| (Trafo3d.Scale(SelectionVolume.selectionVolumeRadius) * Trafo3d.Translation(pos)) * worldToPointcloud |]
        else
            interactionInfo.selectionVolumePath 
            
    let update (scene : Scene) (message : Message) : Scene =

        match message with
            // move HMD
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.hmdId ->
                let hmdPos = t.Forward.TransformPos(V3d())
                let hmdLookAt = t.Forward.TransformDir(-V3d.OOI)
                let hmdUp = t.Forward.TransformDir(V3d.OIO)
                Audio.setListenerPosition(hmdPos)
                Audio.setListenerOrientation(hmdLookAt, hmdUp)

                { scene with viewTrafo = t.Inverse }
            // move cam 1
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam1Id ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.cam1ObjectId, t, scene.objects)
                { scene with objects = newObjects }
            // move cam 2
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam2Id ->
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.cam2ObjectId, t, scene.objects)
                { scene with objects = newObjects }

            // move controller
            | DeviceMove(deviceId, t) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let (controllerObjectId, interactionInfo) = 
                    if firstController then
                        (scene.specialObjectIds.controller1ObjectId, scene.interactionInfo1)
                    else
                        (scene.specialObjectIds.controller2ObjectId, scene.interactionInfo2)
                        
                        

                let newObjects = setTrafoOfObjectsWithId(controllerObjectId, t, scene.objects)
                let newScene = {scene with objects = newObjects}

                let newObjects =    if scene.interactionInfo1.triggerPressed && scene.interactionInfo2.triggerPressed then 
                                        transformPointCloudWithPinch(newScene)
                                    elif interactionInfo.triggerPressed then 
                                        let deltaTrafo = interactionInfo.lastContrTrafo.Inverse * t
                                        transformTrafoOfObjectsWithId(scene.specialObjectIds.centroidId, deltaTrafo, newObjects)
                                    else
                                        newObjects
                let newScene = {scene with objects = newObjects}

                let newSelectionPath =  addTrafoToSelectionVolumePath(interactionInfo, t, scene)
                let newInteractionInfo = { interactionInfo with lastContrTrafo = t; selectionVolumePath = newSelectionPath }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, newScene)
                newScene
                 
            // press trigger
            | DevicePress(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trigger) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                        
                let newInteractionInfo = { interactionInfo with triggerPressed = true}
                makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                    
            // release trigger
            | DeviceRelease(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trigger) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                
                let newInteractionInfo = {interactionInfo with triggerPressed = false }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                newScene

            // press touchpad
            | DevicePress(deviceId, a, t) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trackpad) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                
                let newSelectionPath =  addTrafoToSelectionVolumePath(interactionInfo, t, scene)
                let newInteractionInfo = {interactionInfo with trackpadPressed = true; selectionVolumePath = newSelectionPath }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                newScene

            // release touchpad
            | DeviceRelease(deviceId, a, t) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trackpad) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                
                let finalSelectionPath =  addTrafoToSelectionVolumePath(interactionInfo, t, scene)
                
                
//                let rec traverse (node : OctreeNode) (cell: GridCell) (level : int) : PickCandidateNode[] =
//                    let bb = cell.BoundingBox              
//                    
//                    match node with
//                    | Empty             ->  [||]
//                    | Leaf _            -> 
//                                    let px = node.Points.Value := pointarray
//                                    let lodDataNode = { 
//                                            id = (node :> obj); level = level; bounds = bb; 
//                                            inner = false; granularity = Fun.Cbrt(bb.Volume / 5000.0); 
//                                            render = true}
//               
//                                    if (isPickCandidateNode cell lodDataNode hull p.frustum p.view p.wantedNearPlaneDistance) then                                                                                                                                                         
//                                            [|{node = node ; cell = cell ; color = C4b.Green}|]   
//                                    else
//                                        [||]  
//                                                                               
//                    | Node (_,children) ->                                      
//                                    let lodDataNode = { 
//                                            id = (node :> obj); level = level; bounds = bb; 
//                                                inner = true; granularity = Fun.Cbrt(bb.Volume / 5000.0); 
//                                                render = true}
//                                    
//                                    if (isPickCandidateNode cell lodDataNode hull p.frustum p.view p.wantedNearPlaneDistance) then  
//                                                                    
//                                        let childCandidateNodes = children  |> Array.mapi (fun i child -> traverse (child.Value) (cell.GetChild i) (level + 1)) 
//                                                                            |> Array.concat
//                                        if(childCandidateNodes |> Array.length > 0 ) then 
//                                            childCandidateNodes
//                                        else
//                                            [|{node = node ; cell = cell ; color = C4b.Green}|]     
//                                    else 
//                                        [||] 



                let newSelectionPath = [| |]

                let newInteractionInfo = {interactionInfo with trackpadPressed = false; selectionVolumePath = newSelectionPath }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                newScene
                    
            | StartFrame ->
                { scene with 
                    interactionInfo1 = { scene.interactionInfo1 with vibrationStrength = 0.0 }
                    interactionInfo2 = { scene.interactionInfo2 with vibrationStrength = 0.0 }
                }
                
            | TimeElapsed(dt) ->
//                let newObjects = scalePointCloudWithTrackpad(scene, dt.TotalSeconds)
                let newObjects =    
//                    if scene.interactionInfo1.triggerPressed && scene.interactionInfo2.triggerPressed then 
//                        transformPointCloudWithPinch(scene)
//                    else
                        scene.objects

                                        
//                let currController1Trafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.controller1ObjectId, newObjects)
//                let currController2Trafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.controller1ObjectId, newObjects)
                { scene with 
                    deltaTime = dt.TotalSeconds
                    objects = newObjects
//                    interactionInfo1 = {scene.interactionInfo1 with lastContrTrafo = currController1Trafo}
//                    interactionInfo2 = {scene.interactionInfo1 with lastContrTrafo = currController2Trafo}
                }
            
            | EndFrame ->
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
                
                Vibration.updateVibration(uint32 assignedInputs.controller1Id, scene.deltaTime)
                Vibration.updateVibration(uint32 assignedInputs.controller2Id, scene.deltaTime)

                { scene with
                    interactionInfo1 = { scene.interactionInfo1 with vibStrLastFrame = scene.interactionInfo1.vibrationStrength }
                    interactionInfo2 = { scene.interactionInfo2 with vibStrLastFrame = scene.interactionInfo2.vibrationStrength }
                }

            | _ -> scene
