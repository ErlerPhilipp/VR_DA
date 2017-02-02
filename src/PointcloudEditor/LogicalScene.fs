﻿namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

open InteractiveSegmentation
open InteractiveSegmentation.OctreeHelper
open Aardvark.Database

open System

[<AutoOpen>]
module UnsafeOperators =

    let (:=) (r : thunk<'a>) (v : 'a) =
        (unbox<dbref<'a>> r).Value <- v

    let (!) (r : thunk<'a>) = r.Value

    type thunk<'a> with
        member x.Delete() = 
            match x with
                | :? dbref<'a> as r -> r.Delete()
                | _ -> ()

module LogicalScene =
    open LogicalSceneTypes
    open VrTypes
    open VrInteractions
    open VrDriver

    type PickCandidateNode = 
        {
            node    : OctreeNode 
            cell    : GridCell
            color   : C4b
        }

    let myRand = Random()

    let makeSceneWithInteractionInfo(firstController : bool, newInteractionInfo : InteractionInfo, scene : Scene) =
        if firstController then
            { scene with interactionInfo1 = newInteractionInfo}
        else
            { scene with interactionInfo2 = newInteractionInfo}

    let addTrafoToSelectionVolumePath(interactionInfo : InteractionInfo, t : Trafo3d, scene : Scene) =
        let minDistToNextPos = 0.005
        if interactionInfo.trackpadPressed then
            let newPos = t.Forward.TransformPos(V3d())
            let hasPosNearNewPos = interactionInfo.selectionVolumePath |> Array.exists (fun t -> (t.Forward.TransformPos(V3d()) - newPos).Length < minDistToNextPos)
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

    let deleteSelection(scene : Scene) =
        let newColor = C4b(C4f(myRand.NextDouble(), myRand.NextDouble(), myRand.NextDouble(), 1.0))
        let centroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
        let pointcloudToWorld = scene.pointCloudTrafo * centroidTrafo
        let selectionVolumeRadius = SelectionVolume.selectionVolumeRadius
        let selectionVolumeRadiusSquared = selectionVolumeRadius * selectionVolumeRadius
//        let sphere(t : Trafo3d) = Sphere3d((t * pointcloudToWorld.Inverse).Forward.TransformPos(V3d()), selectionVolumeRadius)
        let sphere(t : Trafo3d) = Sphere3d(t.Forward.TransformPos(V3d()), selectionVolumeRadius)
        let selectionVolumeTrafos = Array.append scene.interactionInfo1.selectionVolumePath scene.interactionInfo2.selectionVolumePath
        
        let cellToBeTraversed (cell : GridCell) : bool = 
            selectionVolumeTrafos |> Array.exists (fun t -> cell.BoundingBox.Intersects(sphere(t)))

        let cellToBeDeleted (cell : GridCell) : bool = 
            selectionVolumeTrafos |> Array.exists (fun t -> cell.BoundingBox.Contains(sphere(t)))

        let pointToBeDeleted (point : Point) : bool = 
            selectionVolumeTrafos |> Array.exists (fun t -> (t.Forward.TransformPos(V3d()) - point.Position).LengthSquared < selectionVolumeRadiusSquared)

        let rec traverse (node : thunk<OctreeNode>) (cell: GridCell) (level : int) =
            let n = !node           
                    
            match n with
            | Empty             ->  
                ()
            | Leaf points       -> 
                let dethunkedPoints = points.Value
//                if cellToBeTraversed(cell) then
//                    if cellToBeDeleted(cell) then
                let newPoints = dethunkedPoints |> Array.map (fun p -> Point(p.Position, p.Normal, newColor))
                n.Points := newPoints
//                    else
//                        let newPoints = dethunkedPoints |> Array.map (fun p -> if pointToBeDeleted(p) then Point(p.Position, p.Normal, newColor) else p)
//                        n.Points := newPoints
                ()
                    
            | Node (points,children) ->
                let dethunkedPoints = points.Value
//                if cellToBeTraversed(cell) then
                children |> Array.mapi (fun i child -> traverse (child) (cell.GetChild i) (level + 1)) |> ignore
//                    if cellToBeDeleted(cell) then
                let newPoints = dethunkedPoints |> Array.map (fun p -> Point(p.Position, p.Normal, newColor))
                n.Points := newPoints
//                    else
//                        let newPoints = dethunkedPoints |> Array.map (fun p -> if pointToBeDeleted(p) then Point(p.Position, p.Normal, newColor) else p)
//                        n.Points := newPoints
                ()
            
        traverse scene.pointCloudOctree.root scene.pointCloudOctree.cell 0 |> ignore
        ()
            
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
                
                deleteSelection(scene)
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
