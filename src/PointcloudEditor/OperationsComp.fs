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
            numSelectOperations : int
            numDeSelectOperations : int
            operationTypes : int[]
            numSelectionVolumes : int[]
        }

    let saveOperationsToFile(operations : Operation[], filePath : string) =
        let serializer = FsPickler.CreateXmlSerializer(indent=true)
        serializer.PickleToString operations |> File.writeAllText filePath
        ()

    let loadOperationsFromFile(filePath : string) =
        let serializer = FsPickler.CreateXmlSerializer(indent=true)
        let operations = File.readAllText filePath |> serializer.UnPickleOfString
        operations

    let compareOperations(refOps : Operation[], myOps : Operation[], octree : Octree) =
        
        let selectionVolumeRadiusSquared(op : Operation) = op.selectionVolumeRadiusPC * op.selectionVolumeRadiusPC

        let pointToBeMarked (point : V3d, operations : Operation[]) = 
            operations |> Array.tryFindBack (fun op -> 
                            op.selectionVolumePath |> Array.exists (fun p -> 
                                                        (p - point).LengthSquared < selectionVolumeRadiusSquared(op)))
            
        let getSelected(dethunkedPoints : Point[], operations : Operation[]) = 
            dethunkedPoints |> Array.map (fun p -> match pointToBeMarked(p.Position - octree.offset, operations) with
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
                let selectedRef = getSelected(dethunkedPoints, refOps)
                let selectedCur = getSelected(dethunkedPoints, myOps)
                let correctlySelected =             selectedRef |> Array.mapi (fun i selected -> if     selected &&     selectedCur.[i] then 1 else 0) |> Array.sum
                let wronglySelected =               selectedRef |> Array.mapi (fun i selected -> if not selected &&     selectedCur.[i] then 1 else 0) |> Array.sum
                let correctlyNonSelectedPoints =    selectedRef |> Array.mapi (fun i selected -> if not selected && not selectedCur.[i] then 1 else 0) |> Array.sum
                let wronglyNonSelectedPoints =      selectedRef |> Array.mapi (fun i selected -> if     selected && not selectedCur.[i] then 1 else 0) |> Array.sum
                SelectionQualityMeasure(correctlySelected, wronglySelected, correctlyNonSelectedPoints, wronglyNonSelectedPoints)

            | Node (_,children) ->
                let selectionQualityMeasure = 
                    children 
                        |> Array.mapi (fun i child -> traverse (child) (cell.GetChild i))
                        |> Array.sum
                selectionQualityMeasure

        let selectionQualityMeasures = traverse octree.root octree.cell
        
        let selectOperations = myOps |> Array.map(fun op -> if op.opType = OperationType.Select then 1 else 0)
        let deSelectOperations = myOps |> Array.map(fun op -> if op.opType = OperationType.Deselect then 0 else 1)
        let numSelectionVolumes = myOps |> Array.map(fun op -> op.selectionVolumePath.Length)

        let comparisonResults = 
            {
                selectionQualityMeasure = selectionQualityMeasures
                numSelectOperations    = selectOperations |> Array.sum
                numDeSelectOperations  = deSelectOperations |> Array.sum
                operationTypes         = selectOperations
                numSelectionVolumes    = numSelectionVolumes
            }
        comparisonResults
        
    let performComparison (refOps : Operation[], myOps : Operation[], octree : Octree) = 
        let currentTimeString = System.DateTime.UtcNow.ToLocalTime().ToString("yyyy-MM-dd_HH-mm-ss")
        
        System.IO.Directory.CreateDirectory("output") |> ignore

        saveOperationsToFile(myOps, @"output\operations_" + currentTimeString + ".xml")

        let screenshot = new System.Drawing.Bitmap(System.Windows.Forms.SystemInformation.VirtualScreen.Width, 
                                                    System.Windows.Forms.SystemInformation.VirtualScreen.Height, 
                                                    System.Drawing.Imaging.PixelFormat.Format32bppArgb);
        let screenGraph = System.Drawing.Graphics.FromImage(screenshot);
        screenGraph.CopyFromScreen(System.Windows.Forms.SystemInformation.VirtualScreen.X, 
                                    System.Windows.Forms.SystemInformation.VirtualScreen.Y, 
                                    0, 
                                    0, 
                                    System.Windows.Forms.SystemInformation.VirtualScreen.Size, 
                                    System.Drawing.CopyPixelOperation.SourceCopy);

        screenshot.Save(@"output\" + currentTimeString + "_screenshot.png", System.Drawing.Imaging.ImageFormat.Png);


        let compRes = compareOperations(refOps, myOps, octree)
        let qualMeas = compRes.selectionQualityMeasure
        let correctlySelectedOfAllSelected = float qualMeas.correctlySelectedPoints / float (qualMeas.correctlySelectedPoints + qualMeas.wronglySelectedPoints)
        let correctlyNonSelectedOfAllNonSelected = float qualMeas.correctlyNonSelectedPoints / float (qualMeas.correctlyNonSelectedPoints + qualMeas.wronglyNonSelectedPoints)
        let correctOfAll = float (qualMeas.correctlySelectedPoints + qualMeas.correctlyNonSelectedPoints) / 
                            float (qualMeas.correctlySelectedPoints + qualMeas.wronglySelectedPoints + qualMeas.correctlyNonSelectedPoints + qualMeas.wronglyNonSelectedPoints)

        let arrayToString(arr : array<_>) =
            "[|" + (arr |> Array.map(fun i -> i.ToString()) |> String.concat ";") + "|]"

        let operationTypes = compRes.operationTypes |> arrayToString
        let numSelectionVolumes = compRes.numSelectionVolumes |> arrayToString
                
        let compResString = 
            compRes.selectionQualityMeasure.ToString()   + System.Environment.NewLine +
            "correctlySelected =        " + correctlySelectedOfAllSelected.ToString()       + System.Environment.NewLine +
            "correctlyNonSelected =     " + correctlyNonSelectedOfAllNonSelected.ToString() + System.Environment.NewLine +
            "correctOfAll =             " + correctOfAll.ToString()                         + System.Environment.NewLine +
            "numSelectOperations =      " + compRes.numSelectOperations.ToString()          + System.Environment.NewLine +
            "numDeSelectOperations =    " + compRes.numDeSelectOperations.ToString()        + System.Environment.NewLine +
            "operationTypes =           " + operationTypes                                  + System.Environment.NewLine +
            "numSelectionVolumes =      " + numSelectionVolumes                             + System.Environment.NewLine
        Logging.log (currentTimeString + " ### Comparison Result ###" + System.Environment.NewLine + compResString)