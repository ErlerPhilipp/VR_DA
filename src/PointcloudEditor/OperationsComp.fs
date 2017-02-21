namespace Aardvark.VR

open Aardvark.Base

open InteractiveSegmentation
open InteractiveSegmentation.OctreeHelper
open Aardvark.Database

open MBrace.FsPickler

open System

open LogicalSceneTypes
open VrTypes

module OperationsComp =

    [<StructuredFormatDisplay("{AsString}")>]
    type SelectionQualityMeasure(correctlySelectedPoints: int, wronglySelectedPoints : int, 
                                 correctlyNonSelectedPoints : int, wronglyNonSelectedPoints : int) =

        member this.correctlySelectedPoints = correctlySelectedPoints
        member this.wronglySelectedPoints = wronglySelectedPoints
        member this.correctlyNonSelectedPoints = correctlyNonSelectedPoints
        member this.wronglyNonSelectedPoints = wronglyNonSelectedPoints

        static member (+) (a : SelectionQualityMeasure, b : SelectionQualityMeasure) =
            SelectionQualityMeasure(a.correctlySelectedPoints + b.correctlySelectedPoints, 
                                    a.wronglySelectedPoints + b.wronglySelectedPoints, 
                                    a.correctlyNonSelectedPoints + b.correctlyNonSelectedPoints, 
                                    a.wronglyNonSelectedPoints + b.wronglyNonSelectedPoints)

        static member Zero = SelectionQualityMeasure(0, 0, 0, 0)

        override m.ToString() = "correctlySelectedPoints =    " + correctlySelectedPoints.ToString() + System.Environment.NewLine +
                                "wronglySelectedPoints =      " + wronglySelectedPoints.ToString() + System.Environment.NewLine +
                                "correctlyNonSelectedPoints = " + correctlyNonSelectedPoints.ToString() + System.Environment.NewLine +
                                "wronglyNonSelectedPoints =   " + wronglyNonSelectedPoints.ToString()
        member m.AsString = m.ToString()
        
    [<StructuredFormatDisplay("selectionQualityMeasure =    {selectionQualityMeasure}" + 
                              "numSelectOperationsA =       {numSelectOperationsA}" + 
                              "numSelectOperationsB =       {numSelectOperationsB}" + 
                              "numDeSelectOperationsA =     {numDeSelectOperationsA}" + 
                              "numDeSelectOperationsB =     {numDeSelectOperationsB}" + 
                              "selectOperationsA =          {selectOperationsA}" + 
                              "selectOperationsB =          {selectOperationsB}" + 
                              "deSelectOperationsA =        {deSelectOperationsA}" + 
                              "deSelectOperationsB =        {deSelectOperationsB}" + 
                              "numSelectionVolumesA =       {numSelectionVolumesA}" + 
                              "numSelectionVolumesB =       {numSelectionVolumesB}")>]
    type ComparisonResults =
        {
            selectionQualityMeasure : SelectionQualityMeasure
            numSelectOperationsA : int
            numSelectOperationsB : int
            numDeSelectOperationsA : int
            numDeSelectOperationsB : int
            operationTypesA : int[]
            operationTypesB : int[]
            numSelectionVolumesA : int[]
            numSelectionVolumesB : int[]
        }

    let saveOperationsToFile(operations : Operation[], filePath : string) =
        let serializer = FsPickler.CreateXmlSerializer(indent=true)
        serializer.PickleToString operations |> File.writeAllText filePath
        ()

    let loadOperationsFromFile(filePath : string) =
        let serializer = FsPickler.CreateXmlSerializer(indent=true)
        let operations = File.readAllText filePath |> serializer.UnPickleOfString
        operations

    let compareOperations(operationsA : Operation[], operationsB : Operation[], octree : Octree) =

        let selectionVolumeRadiusSquared(op : Operation) = op.selectionVolumeRadiusPC * op.selectionVolumeRadiusPC

        let pointToBeMarked (point : Point, operations : Operation[]) = 
            operations |> Array.tryFindBack (fun op -> 
                            op.selectionVolumeTrafos |> Array.exists (fun t -> 
                                                        (t.Forward.TransformPos(V3d()) - point.Position).LengthSquared < selectionVolumeRadiusSquared(op)))
            
        let getSelected(dethunkedPoints : Point[], operations : Operation[]) = 
            dethunkedPoints |> Array.map (fun p -> match pointToBeMarked(p, operations) with
                                                    | Some op -> op.opType = OperationType.Select
                                                    | None -> false
                                          )

        let rec traverse (node : thunk<OctreeNode>) (cell: GridCell) =
            let n = node.GetValue()

            match n with
            | Empty             ->  
                SelectionQualityMeasure.Zero
            | Leaf points       -> 
                let dethunkedPoints = points.Value
                let selectedA = getSelected(dethunkedPoints, operationsA)
                let selectedB = getSelected(dethunkedPoints, operationsB)
                let correctlySelected =             selectedA |> Array.mapi (fun i selected -> if     selected &&     selectedB.[i] then 1 else 0) |> Array.sum
                let wronglySelected =               selectedA |> Array.mapi (fun i selected -> if not selected &&     selectedB.[i] then 1 else 0) |> Array.sum
                let correctlyNonSelectedPoints =    selectedA |> Array.mapi (fun i selected -> if not selected && not selectedB.[i] then 1 else 0) |> Array.sum
                let wronglyNonSelectedPoints =      selectedA |> Array.mapi (fun i selected -> if     selected && not selectedB.[i] then 1 else 0) |> Array.sum
                SelectionQualityMeasure(correctlySelected, wronglySelected, correctlyNonSelectedPoints, wronglyNonSelectedPoints)

            | Node (_,children) ->
                let selectionQualityMeasure = 
                    children 
                        |> Array.mapi (fun i child -> traverse (child) (cell.GetChild i))
                        |> Array.sum
                selectionQualityMeasure

        let selectionQualityMeasures = traverse octree.root octree.cell
        
        let selectOperationsA = operationsA |> Array.map(fun op -> if op.opType = OperationType.Select then 1 else 0)
        let selectOperationsB = operationsB |> Array.map(fun op -> if op.opType = OperationType.Select then 1 else 0)
        let deSelectOperationsA = operationsA |> Array.map(fun op -> if op.opType = OperationType.Deselect then 0 else 1)
        let deSelectOperationsB = operationsB |> Array.map(fun op -> if op.opType = OperationType.Deselect then 0 else 1)
        let numSelectionVolumesA = operationsA |> Array.map(fun op -> op.selectionVolumeTrafos.Length)
        let numSelectionVolumesB = operationsB |> Array.map(fun op -> op.selectionVolumeTrafos.Length)

        let comparisonResults = 
            {
                selectionQualityMeasure = selectionQualityMeasures
                numSelectOperationsA    = selectOperationsA |> Array.sum
                numSelectOperationsB    = selectOperationsB |> Array.sum
                numDeSelectOperationsA  = deSelectOperationsA |> Array.sum
                numDeSelectOperationsB  = deSelectOperationsB |> Array.sum
                operationTypesA         = selectOperationsA
                operationTypesB         = selectOperationsB
                numSelectionVolumesA    = numSelectionVolumesA
                numSelectionVolumesB    = numSelectionVolumesB
            }
        comparisonResults
