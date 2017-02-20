namespace Aardvark.VR

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
            let newPosWS = t.Forward.TransformPos(SelectionVolume.controllerRingCenter)
            let newPosPC = (t * worldToPointcloud).Forward.TransformPos(SelectionVolume.controllerRingCenter)

            let hasPosNearNewPos = interactionInfo.selectionVolumePath |> Array.exists (fun oldTrafo -> (oldTrafo.Forward.TransformPos(V3d()) - newPosPC).Length < minDistToNextPosPC)

            if hasPosNearNewPos then
//                printfn "has sel vol pos, count = %A" (Array.length interactionInfo.selectionVolumePath)
                interactionInfo.selectionVolumePath
            else
//                printfn "add sel vol pos, count = %A" (Array.length interactionInfo.selectionVolumePath + 1)
                Array.append interactionInfo.selectionVolumePath [| (Trafo3d.Scale(SelectionVolume.selectionVolumeRadius) * Trafo3d.Translation(newPosWS) * worldToPointcloud) |]
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
        

    let marked (pointCloudOctree : Octree) (operations : Operation[]) =

        let selectionVolumeRadiusSquared(op : Operation) = op.selectionVolumeRadiusPC * op.selectionVolumeRadiusPC
        let sphere(t : Trafo3d, op : Operation) = 
            Sphere3d(t.Forward.TransformPos(V3d()), op.selectionVolumeRadiusPC)

        let opIntersectsCell(op : Operation, cell : GridCell) =
            op.selectionVolumeTrafos |> Array.exists (fun t -> cell.BoundingBox.Intersects(sphere(t, op)))

        let cellToBeTraversed (cell : GridCell) = 
            operations |> Array.tryFindBack (fun op -> opIntersectsCell(op, cell))
            
        let opContainsCell(op : Operation, cell : GridCell) =
            op.selectionVolumeTrafos |> Array.exists (fun t -> cell.BoundingBox.Contains(sphere(t, op)))

        let cellToBeMarked (cell : GridCell) = 
            operations |> Array.tryFindBack (fun op -> opContainsCell(op, cell))

        let pointToBeMarked (point : Point) = 
            operations |> Array.tryFindBack (
                fun op -> op.selectionVolumeTrafos |> Array.exists (
                            fun t -> 
                                (t.Forward.TransformPos(V3d()) - point.Position).LengthSquared < selectionVolumeRadiusSquared(op)
                            ))
            
        let updatePointState(p : Point, op : Operation) =
            if op.opType = OperationType.Select then 
                Point(p.Position, p.Normal, p.Color, 0x1uy) 
            elif op.opType = OperationType.Deselect then 
                Point(p.Position, p.Normal, p.Color, 0x0uy) 
            else 
                p

        let checkPointsToBeMarked(dethunkedPoints : Point[]) = 
            lazy (dethunkedPoints |> Array.map (fun p -> match pointToBeMarked(p) with
                                                            | Some op -> updatePointState(p, op)
                                                            | None -> p
                                               ))

        let markEntireLeaf(dethunkedPoints : Point[], op : Operation) =
            let newPoints = lazy (dethunkedPoints |> Array.map (fun p -> updatePointState(p, op)))
            Leaf(newPoints.Value.Length, memoryThunk newPoints)

        let rec traverse (node : thunk<OctreeNode>) (cell: GridCell) (level : int) (cellContainedInOperation : Operation option) =
            let n = !node
            
            let markEntireCell(dethunkedPoints : Point[], children : thunk<OctreeNode>[], op : Operation) =
                let newChildren = children |> Array.mapi (fun i child -> memoryThunk(traverse (child) (cell.GetChild i) (level + 1) (Some op)) :> thunk<_>)
                let newPoints = lazy (dethunkedPoints |> Array.map (fun p -> updatePointState(p, op)))
                Node(newPoints.Value.Length, memoryThunk newPoints, newChildren)

            match n with
            | Empty             ->  
                lazy n
            | Leaf points       -> 
                lazy 
                    match cellContainedInOperation with 
                        | Some op -> //when op.opType = OperationType.Select -> 
                            let dethunkedPoints = points.Value
                            markEntireLeaf(dethunkedPoints, op)
                        | _ ->  match cellToBeTraversed(cell) with
                                    | Some op -> //when op.opType = OperationType.Select ->  
                                        match cellToBeMarked(cell) with
                                            | Some op -> //when op.opType = OperationType.Select -> 
                                                let dethunkedPoints = points.Value
                                                markEntireLeaf(dethunkedPoints, op)
                                            | _ ->  //if op.opType = OperationType.Select then 
                                                        let dethunkedPoints = points.Value
                                                        let newPoints = checkPointsToBeMarked(dethunkedPoints)
                                                        Leaf(newPoints.Value.Length, memoryThunk newPoints)
                                                    //else n
                                    | _ -> n
            | Node (points,children) ->
                lazy
                    match cellContainedInOperation with 
                        | Some op -> //when op.opType = OperationType.Select -> 
                            let dethunkedPoints = points.Value
                            markEntireCell(dethunkedPoints, children, op)
                        | _ ->  match cellToBeTraversed(cell) with
                                    | Some op -> //when op.opType = OperationType.Select ->  
                                        match cellToBeMarked(cell) with
                                            | Some op -> //when op.opType = OperationType.Select -> 
                                                let dethunkedPoints = points.Value
                                                markEntireCell(dethunkedPoints, children, op)
                                            | _ ->  //if op.opType = OperationType.Select then 
                                                        let dethunkedPoints = points.Value
                                                        let newChildren = children |> Array.mapi (fun i child -> memoryThunk(traverse (child) (cell.GetChild i) (level + 1) (None)) :> thunk<_>)
                                                        let newPoints = checkPointsToBeMarked(dethunkedPoints)
                                                        Node(newPoints.Value.Length, memoryThunk newPoints, newChildren)
                                                    //else n
                                    | _ -> n

        let newRoot = traverse pointCloudOctree.root pointCloudOctree.cell 0 None
        { pointCloudOctree with root = memoryThunk newRoot }

    let operateSelection(operations : Operation[], worldToPointcloud : Trafo3d, selectionVolumeTrafos : Trafo3d[], opType : OperationType) =
        let selectionVolumeRadiusWS = SelectionVolume.selectionVolumeRadius
        let selectionVolumeRadiusPC = worldToPointcloud.Forward.TransformDir(V3d(selectionVolumeRadiusWS, 0.0, 0.0)).Length

        let newOperation = 
            {
                opType = opType
                selectionVolumeTrafos = selectionVolumeTrafos
                worldToPointcloud = worldToPointcloud
                selectionVolumeRadiusPC = selectionVolumeRadiusPC
            }
            
//        Array.append operations [| newOperation |]
        [| newOperation |]
//        transact (fun () -> Operations.Value <- Array.append Operations.Value [| newOperation |])
        
    let pointsInSelectionVolume (controllerTrafoWS : Trafo3d, worldToPointcloud : Trafo3d, pointCloudOctree : Octree, traverseUntilXPointsFound : int) =
        
        let selVolPosPC = (controllerTrafoWS * worldToPointcloud).Forward.TransformPos(SelectionVolume.controllerRingCenter)
        let selectionVolumeRadiusWS = SelectionVolume.selectionVolumeRadius
        let selectionVolumeRadiusPC = worldToPointcloud.Forward.TransformDir(V3d(selectionVolumeRadiusWS, 0.0, 0.0)).Length
        let selectionVolumeRadiusSquared = selectionVolumeRadiusPC * selectionVolumeRadiusPC
        let sphere = Sphere3d(selVolPosPC, selectionVolumeRadiusPC)

        let cellIntersectsSelVol (cell : GridCell) : bool = cell.BoundingBox.Intersects(sphere)
        let cellContainsSelVol (cell : GridCell) : bool = cell.BoundingBox.Contains(sphere)
        let pointInSelVol (point : Point) : bool = (selVolPosPC - point.Position).LengthSquared < selectionVolumeRadiusSquared

        let rec traverse (node : thunk<OctreeNode>) (cell: GridCell) (level : int) (cellContained : bool) (pointsFoundSoFar : int) : int =
            let n = !node
            
            let numPointsInSelectionVolume(dethunkedPoints : Point[]) = 
                dethunkedPoints |> Array.filter (fun p -> pointInSelVol(p)) |> Array.length

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
                
                let centroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
                let worldToPointcloud = (scene.pointCloudTrafo * centroidTrafo).Inverse

                let searchUntilXPoints = 100
                let numPointsInSelVol = pointsInSelectionVolume(t, worldToPointcloud, scene.initialOctree, searchUntilXPoints)
//                printfn "numPointsInSelVol = %A" numPointsInSelVol
                let minVibStrength = 0.0
//                let linStrength = minVibStrength + ((1.0 - minVibStrength) * (clamp 0.0 1.0 (float numPointsInSelVol / float searchUntilXPoints)))
                let limitedGrowthStrength = clamp 0.0 1.0 (1.0 - (1.0 - minVibStrength) * Math.Exp(-0.05 * float numPointsInSelVol))

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
                                        
                let newSelectionPath =  addTrafoToSelectionVolumePath(interactionInfo, t, scene)
                let newInteractionInfo = {interactionInfo with trackpadPressed = true; selectionVolumePath = newSelectionPath; currActionType = newOpType }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                newScene

            // release touchpad
            | DeviceRelease(deviceId, a, _) when (deviceId = assignedInputs.controller1Id || deviceId = assignedInputs.controller2Id) && a = int (VrAxis.VrControllerAxis.Trackpad) ->
                let firstController = deviceId = assignedInputs.controller1Id
                let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2

                let newOperations =  
                    if interactionInfo.currActionType = TrackpadActionType.Select || interactionInfo.currActionType = TrackpadActionType.Deselect then
                        let centroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
                        let worldToPointcloud = (scene.pointCloudTrafo * centroidTrafo).Inverse
                        let selectionVolumeTrafos = Array.append scene.interactionInfo1.selectionVolumePath scene.interactionInfo2.selectionVolumePath
                        if interactionInfo.currActionType = TrackpadActionType.Select then
                            operateSelection(scene.operations, worldToPointcloud, selectionVolumeTrafos, OperationType.Select)
                        elif interactionInfo.currActionType = TrackpadActionType.Deselect then
                            operateSelection(scene.operations, worldToPointcloud, selectionVolumeTrafos, OperationType.Deselect)
                        else
                            scene.operations
                    else
                        scene.operations
                        
                let newOctree = marked scene.currentOctree newOperations
                let newInteractionInfo = {interactionInfo with trackpadPressed = false; selectionVolumePath = [||]; currActionType = TrackpadActionType.Nop }
                let newScene = makeSceneWithInteractionInfo(firstController, newInteractionInfo, scene)
                { newScene with operations = newOperations; currentOctree = newOctree }
                    
            | StartFrame ->
                { scene with 
                    interactionInfo1 = { scene.interactionInfo1 with vibrationStrength = 0.0 }
                    interactionInfo2 = { scene.interactionInfo2 with vibrationStrength = 0.0 }
                }
                
            | TimeElapsed(dt) ->
//                let newObjects = scalePointCloudWithTrackpad(scene, dt.TotalSeconds)
                let newObjects = scene.objects
                
                let axisValue1 = getAxisValue(uint32 assignedInputs.controller1Id, 0) 
                let axisValue2 = getAxisValue(uint32 assignedInputs.controller2Id, 0)
                let controller1Trafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.controller1ObjectId, scene.objects)
                let controller2Trafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.controller2ObjectId, scene.objects)
                let thumbPosTrafo1 = Trafo3d.Scale(0.33) * Trafo3d.Translation(axisValue1.X, axisValue1.Y, 0.0) * scene.contrToTrackpad * controller1Trafo
                let thumbPosTrafo2 = Trafo3d.Scale(0.33) * Trafo3d.Translation(axisValue2.X, axisValue2.Y, 0.0) * scene.contrToTrackpad * controller2Trafo
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.thumbPos1, thumbPosTrafo1, newObjects)
                let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.thumbPos2, thumbPosTrafo2, newObjects)

//                printfn "ctrl = %A |thumb = %A" (controller1Trafo.Forward.TransformPos(V3d())) (thumbPosTrafo1.Forward.TransformPos(V3d()))

                //TODO:
//                if scene.interactionInfo1.currActionType = OperationType.Upscale then
//                let scalingSpeed = 1.1
//                let scalingFactorForFrame = 1.0 + scalingFactor * scalingSpeed * (dt.TotalSeconds)
//                let deltaTrafo = Trafo3d.Scale(scalingFactorForFrame)
//                printfn "axisValue = %A, scalingFactorForFrame %A" axisValue.Y scalingFactorForFrame
//
//                let translation = Trafo3d.Translation(currTrafo.Forward.TransformPos(V3d()))
//                translation.Inverse * deltaTrafo * translation


                { scene with 
                    deltaTime = dt.TotalSeconds
                    objects = newObjects
                }
            
            | EndFrame ->
                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller1Id)
                Vibration.stopVibration(Vibration.OverlappingObject, uint32 assignedInputs.controller2Id)

//                // hit object
//                if scene.interactionInfo1.vibStrLastFrame = 0.0 && scene.interactionInfo1.vibrationStrength <> 0.0 then
//                    Vibration.vibrate(Vibration.HitObject, uint32 assignedInputs.controller1Id, 0.1, 0.5)
//                if scene.interactionInfo2.vibStrLastFrame = 0.0 && scene.interactionInfo2.vibrationStrength <> 0.0 then
//                    Vibration.vibrate(Vibration.HitObject, uint32 assignedInputs.controller2Id, 0.1, 0.5)
                
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
