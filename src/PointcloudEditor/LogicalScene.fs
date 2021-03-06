﻿namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

open InteractiveSegmentation
open InteractiveSegmentation.OctreeHelper
open Aardvark.Database

open MBrace.FsPickler

open System

open LogicalSceneTypes
open VrTypes
open VrInteractions
open VrDriver
open OperationsComp

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

    let makeSceneWithInteractionInfo(firstController : bool, newInteractionInfo : InteractionInfo, scene : Scene) =
        if firstController then
            { scene with interactionInfo1 = newInteractionInfo}
        else
            { scene with interactionInfo2 = newInteractionInfo}

    let addTrafoToSelectionVolumePath(interactionInfo : InteractionInfo, t : Trafo3d, scene : Scene) =
        if interactionInfo.trackpadPressed then
            let centroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
            let worldToPointcloud = (scene.pointCloudTrafo * centroidTrafo).Inverse
            
            let minDistToNextPosWS = 0.01
            let minDistToNextPosPC = worldToPointcloud.Forward.TransformDir(V3d(minDistToNextPosWS, 0.0, 0.0)).Length
            let newPosPC = (t * worldToPointcloud).Forward.TransformPos(SelectionVolume.controllerToRingCenter) - scene.initialOctree.offset

            let hasPosNearNewPos = interactionInfo.selectionVolumePath |> Array.exists (fun oldPos -> (oldPos - newPosPC).Length < minDistToNextPosPC)

            if hasPosNearNewPos then
//                printfn "has sel vol pos, count = %A" (Array.length interactionInfo.selectionVolumePath)
                interactionInfo.selectionVolumePath
            else
//                printfn "add sel vol pos, count = %A" (Array.length interactionInfo.selectionVolumePath + 1)
                Array.append interactionInfo.selectionVolumePath [| newPosPC |]
        else
            interactionInfo.selectionVolumePath


    type memoryThunk<'a>(v: Lazy<'a>) =  
        inherit thunk<'a>()

        override x.GetValue() = v.Value
        override x.IsEvaluated = true
        override x.IsSerializable = true

        static member CreatePickler (_ : IPicklerResolver) : Pickler<memoryThunk<'a>> =
            Pickler.Null()
    
        member x.Value = v.Value
        

    let marked (pointCloudOctree : Octree) (operation : Operation) =

        let selectionVolumeRadiusSquared(op : Operation) = op.selectionVolumeRadiusPC * op.selectionVolumeRadiusPC
        let sphere(p : V3d, op : Operation) = 
            Sphere3d(p, op.selectionVolumeRadiusPC)

        let opIntersectsCell(op : Operation, cell : GridCell) =
            op.selectionVolumePath |> Array.exists (fun p -> cell.BoundingBox.Intersects(sphere(p + pointCloudOctree.offset, op)))

        let cellToBeTraversed (cell : GridCell) = 
            opIntersectsCell(operation, cell)
            
        let opContainsCell(op : Operation, cell : GridCell) =
            op.selectionVolumePath |> Array.exists (fun p -> cell.BoundingBox.Contains(sphere(p + pointCloudOctree.offset, op)))

        let cellToBeMarked (cell : GridCell) = 
            opContainsCell(operation, cell)

        let pointToBeMarked (point : V3d) = 
            operation.selectionVolumePath |> Array.exists (
                fun p -> 
                    (p + pointCloudOctree.offset - point).LengthSquared < selectionVolumeRadiusSquared(operation)
                )
            
        let updatePointState(p : Point, op : Operation) =
            if op.opType = OperationType.Select then 
                Point(p.Position, p.Normal, p.Color, PointState.Selected)
            elif op.opType = OperationType.Deselect then 
                Point(p.Position, p.Normal, p.Color, PointState.Normal)
            else 
                p

        let checkPointsToBeMarked(dethunkedPoints : Point[]) = 
            lazy (dethunkedPoints |> Array.map (fun p -> if pointToBeMarked(p.Position) then updatePointState(p, operation) else p))

        let markEntireLeaf(dethunkedPoints : Point[], op : Operation) =
            let newPoints = lazy (dethunkedPoints |> Array.map (fun p -> updatePointState(p, op)))
            Leaf(newPoints.Value.Length, memoryThunk newPoints)

        let rec traverse (node : thunk<OctreeNode>) (cell: GridCell) (level : int) (cellContainedInOperation : bool) =
            let n = !node
            
            let markEntireCell(dethunkedPoints : Point[], children : thunk<OctreeNode>[], op : Operation) =
                let newChildren = children |> Array.mapi (fun i child -> memoryThunk(traverse (child) (cell.GetChild i) (level + 1) (true)) :> thunk<_>)
                let newPoints = lazy (dethunkedPoints |> Array.map (fun p -> updatePointState(p, op)))
                Node(newPoints.Value.Length, memoryThunk newPoints, newChildren)

            match n with
            | Empty             ->  
                lazy n
            | Leaf points       -> 
                lazy 
                    if cellContainedInOperation then 
                        let dethunkedPoints = points.Value
                        markEntireLeaf(dethunkedPoints, operation)
                    else
                        if cellToBeTraversed(cell) then
                            if cellToBeMarked(cell) then
                                let dethunkedPoints = points.Value
                                markEntireLeaf(dethunkedPoints, operation)
                            else 
                                let dethunkedPoints = points.Value
                                let newPoints = checkPointsToBeMarked(dethunkedPoints)
                                Leaf(newPoints.Value.Length, memoryThunk newPoints)
                        else n
            | Node (points,children) ->
                lazy
                    if cellContainedInOperation then 
                        let dethunkedPoints = points.Value
                        markEntireCell(dethunkedPoints, children, operation)
                    else
                        if cellToBeTraversed(cell) then
                            if cellToBeMarked(cell) then
                                let dethunkedPoints = points.Value
                                markEntireCell(dethunkedPoints, children, operation)
                            else
                                let dethunkedPoints = points.Value
                                let newChildren = children |> Array.mapi (fun i child -> memoryThunk(traverse (child) (cell.GetChild i) (level + 1) (false)) :> thunk<_>)
                                let newPoints = checkPointsToBeMarked(dethunkedPoints)
                                Node(newPoints.Value.Length, memoryThunk newPoints, newChildren)
                        else n

        let newRoot = traverse pointCloudOctree.root pointCloudOctree.cell 0 false
        { pointCloudOctree with root = memoryThunk newRoot }

    let pointsInSelectionVolume (controllerTrafoWS : Trafo3d, worldToPointcloud : Trafo3d, pointCloudOctree : Octree, traverseUntilXPointsFound : int, interactionInfo : InteractionInfo) =
        
        let selVolPosPC = (controllerTrafoWS * worldToPointcloud).Forward.TransformPos(SelectionVolume.controllerToRingCenter)
        let selectionVolumeRadiusWS = SelectionVolume.selectionVolumeRadius * interactionInfo.currSelVolScale
        let selectionVolumeRadiusPC = worldToPointcloud.Forward.TransformDir(V3d(selectionVolumeRadiusWS, 0.0, 0.0)).Length
        let selectionVolumeRadiusSquared = selectionVolumeRadiusPC * selectionVolumeRadiusPC
        let sphere = Sphere3d(selVolPosPC, selectionVolumeRadiusPC)

        let cellIntersectsSelVol (cell : GridCell) : bool = cell.BoundingBox.Intersects(sphere)
        let cellContainsSelVol (cell : GridCell) : bool = cell.BoundingBox.Contains(sphere)
        let pointInSelVol (point : V3d) : bool = (selVolPosPC - point).LengthSquared < selectionVolumeRadiusSquared

        let rec traverse (node : thunk<OctreeNode>) (cell: GridCell) (level : int) (cellContained : bool) (pointsFoundSoFar : int) : int =
            let n = !node
            
            let numPointsInSelectionVolume(dethunkedPoints : Point[]) = 
                dethunkedPoints |> Array.filter (fun p -> pointInSelVol(p.Position)) |> Array.length

            let numPointsInLeaf(dethunkedPoints : Point[]) =
                dethunkedPoints |> Array.length

            let numPointsInCell(dethunkedPoints : Point[], children : thunk<OctreeNode>[]) =
                let pointsInCell = dethunkedPoints |> Array.length
                let pointsFoundSoFar = pointsFoundSoFar + pointsInCell
                let pointsInChildren =  if pointsFoundSoFar > traverseUntilXPointsFound then 
                                            [||]
                                        else
                                            children |> Array.mapi (fun i child -> (traverse (child) (cell.GetChild i) (level + 1) (true) (pointsFoundSoFar)))
                let overallPoints = pointsInCell + (Array.sum pointsInChildren)
                overallPoints

            match n with
            | Empty             ->  
                0
            | Leaf points       -> 
                let dethunkedPoints = points.Value

                if cellContained then 
                    numPointsInLeaf(dethunkedPoints)
                else
                    if cellIntersectsSelVol(cell) then 
                        if cellContainsSelVol(cell) then
                            numPointsInLeaf(dethunkedPoints)
                        else
                            numPointsInSelectionVolume(dethunkedPoints)
                    else 0
            | Node (points,children) ->
                let dethunkedPoints = points.Value

                if cellContained then 
                    numPointsInCell(dethunkedPoints, children)
                else
                    if cellIntersectsSelVol(cell) then 
                        if cellContainsSelVol(cell) then
                            numPointsInCell(dethunkedPoints, children)
                        else
                            let pointsInSelVol = numPointsInSelectionVolume(dethunkedPoints)
                            let pointsFoundSoFar = pointsFoundSoFar + pointsInSelVol
                            let pointsInChildren =  if pointsFoundSoFar > traverseUntilXPointsFound then
                                                        [||]
                                                    else
                                                        children |> Array.mapi (fun i child -> (traverse (child) (cell.GetChild i) (level + 1) (false)(pointsFoundSoFar)))
                            let overallPoints = pointsInSelVol + (Array.sum pointsInChildren)
                            overallPoints
                            
                    else 0

        traverse pointCloudOctree.root pointCloudOctree.cell 0 false 0
            
    let updateReferenceOperations(compare : bool, refOps : Operation[], myOps : Operation[], octree : Octree, refOperationsFile : string) = 
        let worker =
            async {
                do! Async.SwitchToNewThread()
                if compare then
                    printfn "%A: start comparison" DateTime.Now
                    OperationsComp.performComparison(refOps, myOps, octree)
                    printfn "%A: finish comparison" DateTime.Now
                else
                    OperationsComp.saveOperationsToFile(myOps, refOperationsFile)
            }

        Async.Start(worker)
        
    let deleteMarked (pointCloudOctree : Octree) =

        let rec traverse (node : thunk<OctreeNode>) (cell: GridCell) =
            let n = !node

            match n with
            | Empty             ->  
                lazy n
            | Leaf points       -> 
                lazy 
                    let dethunkedPoints = points.Value
                    let newPoints = lazy (dethunkedPoints |> Array.map (fun p -> if p.State = PointState.Selected then Point(p.Position, p.Normal, p.Color, PointState.Deleted) else p))
                    Leaf(newPoints.Value.Length, memoryThunk newPoints)
            | Node (points,children) ->
                lazy
                    let dethunkedPoints = points.Value
                    let newChildren = children |> Array.mapi (fun i child -> memoryThunk(traverse (child) (cell.GetChild i)) :> thunk<_>)
                    let newPoints = lazy (dethunkedPoints |> Array.map (fun p -> if p.State = PointState.Selected then Point(p.Position, p.Normal, p.Color, PointState.Deleted) else p))
                    Node(newPoints.Value.Length, memoryThunk newPoints, newChildren)

        let newRoot = traverse pointCloudOctree.root pointCloudOctree.cell
        { pointCloudOctree with root = memoryThunk newRoot }

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
                
                let newSelectionPath =  if interactionInfo.currActionType = TrackpadActionType.Select || interactionInfo.currActionType = TrackpadActionType.Deselect then 
                                            addTrafoToSelectionVolumePath(interactionInfo, t, scene)
                                        else
                                            interactionInfo.selectionVolumePath
                
                let centroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
                let worldToPointcloud = (scene.pointCloudTrafo * centroidTrafo).Inverse

                let searchUntilXPoints = 100
                let numPointsInSelVol = pointsInSelectionVolume(t, worldToPointcloud, scene.initialOctree, searchUntilXPoints, interactionInfo)
//                printfn "numPointsInSelVol = %A" numPointsInSelVol
                let minVibStrength = 0.0
//                let linStrength = minVibStrength + ((1.0 - minVibStrength) * (clamp 0.0 1.0 (float numPointsInSelVol / float searchUntilXPoints)))
                let limitedGrowthStrength = clamp 0.0 1.0 (1.0 - (1.0 - minVibStrength) * Math.Exp(-0.05 * float numPointsInSelVol))
                let limitedGrowthStrength = limitedGrowthStrength / 4.0 // full strength gets annoying

//                printfn "numPointsInSelVol = %A; limitedGrowthStrength = %A" numPointsInSelVol limitedGrowthStrength; 
                let newVibrationStrength = if numPointsInSelVol = 0 then 0.0 else limitedGrowthStrength

                let newInteractionInfo = { interactionInfo with 
                                            lastContrTrafo = t
                                            selectionVolumePath = newSelectionPath
                                            numPointsInSelVol = numPointsInSelVol
                                            vibrationStrength = newVibrationStrength
                                         }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, newScene)
                newScene
                 
            // press trigger
            | DevicePress(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trigger) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                        
                let newInteractionInfo = { interactionInfo with triggerPressed = true}
                makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                    
            // press app menu button
            | DevicePress(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.ApplicationMenu) ->
                let newOctree = 
                    if scene.allOperations.Length > 0 then
                        deleteMarked scene.currentOctree
                    else
                        scene.currentOctree

                updateReferenceOperations(scene.loadGroundTruth, scene.referenceOperations, scene.allOperations, scene.initialOctree, scene.refOperationsFile)
                { scene with currentOctree = newOctree }
                    
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
                
                let axisValue = if firstController then getAxisValue(uint32 assignedInputs.controller1Id, 0) else getAxisValue(uint32 assignedInputs.controller2Id, 0)
                let newOpType = if abs(axisValue.Y) < 0.5 then 
                                    if axisValue.X < -0.5 then TrackpadActionType.Select elif axisValue.X > 0.5 then TrackpadActionType.Deselect else TrackpadActionType.Nop
                                elif abs(axisValue.X) < 0.5 then 
                                    if axisValue.Y < -0.5 then TrackpadActionType.Downscale elif axisValue.Y > 0.5 then TrackpadActionType.Upscale else TrackpadActionType.Nop
                                else
                                    TrackpadActionType.Nop
                                        
                let newSelectionPath =  if newOpType = TrackpadActionType.Select || newOpType = TrackpadActionType.Deselect then 
                                            addTrafoToSelectionVolumePath(interactionInfo, t, scene)
                                        else
                                            interactionInfo.selectionVolumePath

                let newInteractionInfo = {interactionInfo with trackpadPressed = true; selectionVolumePath = newSelectionPath; currActionType = newOpType }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                newScene

            // release touchpad
            | DeviceRelease(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trackpad) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2
                
                let newOctree, newOperations = 
                    if interactionInfo.currActionType = TrackpadActionType.Select || interactionInfo.currActionType = TrackpadActionType.Deselect then
                        let centroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
                        let worldToPointcloud = (scene.pointCloudTrafo * centroidTrafo).Inverse
                        let selectionVolumePath = Array.append scene.interactionInfo1.selectionVolumePath scene.interactionInfo2.selectionVolumePath
                        
                        let selectionVolumeRadiusWS = SelectionVolume.selectionVolumeRadius * interactionInfo.currSelVolScale
                        let selectionVolumeRadiusPC = worldToPointcloud.Forward.TransformDir(V3d(selectionVolumeRadiusWS, 0.0, 0.0)).Length
                        
                        let opType = if interactionInfo.currActionType = TrackpadActionType.Select then OperationType.Select else OperationType.Deselect

                        let newOperation = 
                            {
                                opType = opType
                                selectionVolumePath = selectionVolumePath
                                worldToPointcloud = worldToPointcloud
                                selectionVolumeRadiusPC = selectionVolumeRadiusPC
                            }
                        let newOctree = marked scene.currentOctree newOperation
                        let newOperations = Array.append (scene.allOperations) [|newOperation|]

                        newOctree, newOperations
                    else
                        scene.currentOctree, scene.allOperations
                        
                let newInteractionInfo = {interactionInfo with trackpadPressed = false; selectionVolumePath = [||]; currActionType = TrackpadActionType.Nop }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                { newScene with currentOctree = newOctree; allOperations = newOperations }
                    
            | StartFrame ->
                { scene with 
                    interactionInfo1 = { scene.interactionInfo1 with vibrationStrength = 0.0 }
                    interactionInfo2 = { scene.interactionInfo2 with vibrationStrength = 0.0 }
                }
                
            | TimeElapsed(dt) ->
                let newObjects = scene.objects
                
                let axisValue1 = getAxisValue(uint32 assignedInputs.controller1Id, 0) 
                let axisValue2 = getAxisValue(uint32 assignedInputs.controller2Id, 0)
                let controller1Trafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.controller1ObjectId, scene.objects)
                let controller2Trafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.controller2ObjectId, scene.objects)
                let thumbPosTrafo1 = Trafo3d.Scale(0.33) * Trafo3d.Translation(axisValue1.X, axisValue1.Y, 0.0) * scene.contrToTrackpad * controller1Trafo
                let thumbPosTrafo2 = Trafo3d.Scale(0.33) * Trafo3d.Translation(axisValue2.X, axisValue2.Y, 0.0) * scene.contrToTrackpad * controller2Trafo
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.thumbPos1Id, thumbPosTrafo1, newObjects)
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.thumbPos2Id, thumbPosTrafo2, newObjects)

                let updateSelectionVolumeScale(interactionInfo : InteractionInfo) =
                    if interactionInfo.currActionType = TrackpadActionType.Upscale || interactionInfo.currActionType = TrackpadActionType.Downscale then
                        
                        let scalingFactor = if interactionInfo.currActionType = TrackpadActionType.Upscale then 1.0 else -1.0
                        let scalingSpeed = 1.1
                        let scalingFactorForFrame = 1.0 + scalingFactor * scalingSpeed * (dt.TotalSeconds)
                        let newSelVolScale = clamp 0.1 10.0 interactionInfo.currSelVolScale * scalingFactorForFrame
                        { interactionInfo with currSelVolScale = newSelVolScale }
                    else
                        interactionInfo

                let updateSelectionVolumeTrafo(controllerId : int, selVolId : int, objects : PersistentHashSet<Object>, newScale : float) =
                        let controllerTrafo = getTrafoOfFirstObjectWithId(controllerId, objects)
                        let controllerToRingCenter = Trafo3d.Translation(SelectionVolume.controllerToRingCenter)
                        let newSelVolTrafo = Trafo3d.Scale(newScale) * controllerToRingCenter * controllerTrafo
                        setTrafoOfObjectsWithId(selVolId, newSelVolTrafo, objects)
                        
                let newInteractionInfo1 = updateSelectionVolumeScale(scene.interactionInfo1)
                let newInteractionInfo2 = updateSelectionVolumeScale(scene.interactionInfo2)
                let newObjects = updateSelectionVolumeTrafo(scene.specialObjectIds.controller1ObjectId, scene.specialObjectIds.selectionVolume1Id, newObjects, newInteractionInfo1.currSelVolScale)
                let newObjects = updateSelectionVolumeTrafo(scene.specialObjectIds.controller2ObjectId, scene.specialObjectIds.selectionVolume2Id, newObjects, newInteractionInfo2.currSelVolScale)

                let newTimeSinceLastComp = 
                    if scene.autoCompareInSec > 0.0 then
                        let timeBetweenComparisons = 60.0
                        if scene.timeSinceLastComp > timeBetweenComparisons then
                            updateReferenceOperations(scene.loadGroundTruth, scene.referenceOperations, scene.allOperations, scene.initialOctree, scene.refOperationsFile)
                            0.0
                        else
                            scene.timeSinceLastComp + dt.TotalSeconds
                    else
                        0.0

                { scene with 
                    deltaTime = dt.TotalSeconds
                    objects = newObjects
                    interactionInfo1 = newInteractionInfo1
                    interactionInfo2 = newInteractionInfo2
                    timeSinceLastComp = newTimeSinceLastComp
                }
            
            | EndFrame ->
                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller1Id)
                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller2Id)

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
