namespace InteractiveSegmentation

open System.Diagnostics
open Aardvark.Database
open Aardvark.Git
open Aardvark.Git.Operators
open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Native

open System.IO

type Point =
    struct
        val mutable public Position : V3d   
        val mutable public Normal   : V3d     
        val mutable public Color    : C4b      
        //new(p,c) = { Position = p; Color = c }
        new(p,n, c) = { Position = p; Normal = n; Color = c }
    end

module PSeq =
    open System
    open System.Diagnostics
    open System.Threading
    open System.Collections.Concurrent   

    type pset<'a> = { values : BlockingCollection<'a> }

    type BlockingCollection<'a> with
        member x.MyTake() =
            try x.Take()
            with :? InvalidOperationException -> raise <| OperationCanceledException()

    let ofSeq (s : seq<'a>) =
        let values = new BlockingCollection<'a>(5)

        let run =
            async { 
                for e in s do 
                    values.Add e

                values.CompleteAdding()
            }

        Async.Start run

        { values = values }

    let map (f : 'a -> 'b) (input : pset<'a>) =
        let values = new BlockingCollection<'b>()
        let mutable running = 0

        let run =
            async {
                try
                    do! Async.SwitchToNewThread()
                    Interlocked.Increment(&running) |> ignore

                    while true  do
                        let v = input.values.MyTake()
                        let res = f v
                        values.Add(res)

                with :? OperationCanceledException ->
                    if Interlocked.Decrement(&running) = 0 then
                        values.CompleteAdding()


            }

        for i in 1..Environment.ProcessorCount do Async.Start run

        { values = values }

    let reduce (f : 'a -> 'a -> 'a) (input : pset<'a>) =
        
        let local = new BlockingCollection<'a>()

        let mutable count = 0
        let mutable migrated = false
        let migrate =
            async {
                do! Async.SwitchToNewThread()
                try
                    while true do
                        let v = input.values.MyTake()
                        local.Add v
                        Interlocked.Increment(&count) |> ignore

                with :? OperationCanceledException -> 
                    migrated <- true
            }

        Async.Start migrate

        let mutable running = 0
        let result = System.Threading.Tasks.TaskCompletionSource<'a>()
        let semLock = obj()
        let reduce =
            async {
                do! Async.SwitchToNewThread()
                Interlocked.Increment(&running) |> ignore

                while not migrated || local.Count > 1  do
                    let two = 
                        lock semLock (fun () ->
                            let a = local.Take()
                            match local.TryTake() with
                                | (true, b) -> Some (a,b)
                                | _ ->
                                    local.Add a
                                    Thread.Sleep(10)
                                    None
                        )

                    match two with
                        | Some (l,r) ->
                            let v = f l r
                            local.Add v
                        | None -> 
                            ()


                if Interlocked.Decrement(&running) = 0 then
                    let final = local.TryTake() |> snd
                    result.SetResult(final)
                
            }


        for i in 1..Environment.ProcessorCount do Async.Start reduce

        result.Task




[<AllowNullLiteral>]
type OctreeNode =
    class
        val mutable public Count : int
        val mutable public Points : thunk<Point[]>
        val mutable public Children : thunk<OctreeNode>[]        

        new(cnt, p,c) = { Count = cnt; Points = p; Children = c }  

        override x.GetHashCode() =
            HashCode.Combine(x.Count, x.Points.GetHashCode())

        override x.Equals(o) =
            match o with
                | :? OctreeNode as n -> 
                    n.Count = x.Count && n.Points = x.Points// && n.Children = x.Children
                | _ -> false
    end


type Octree = 
    { 
        offset : V3d
        splitCount : int
        pointCount : int
        store : Database
        root : thunk<OctreeNode>
        bounds : Box3d
        cell : GridCell 
    }

[<AutoOpen>]
module ``Octree Node Extensions`` =
    let (|Node|Leaf|Empty|) (n : OctreeNode) =
        if isNull n then Empty
        elif isNull n.Children then Leaf n.Points
        else Node (n.Points, n.Children)

    let Node(cnt,points,children)   : OctreeNode    = OctreeNode(cnt, points, children)
    let Leaf(cnt,points)            : OctreeNode    = OctreeNode(cnt, points, null)
    let Empty                       : OctreeNode    = null

//
//type DeleteHullOperation(h : Hull3d) =
//    member x.Perform(t : thunk<OctreeNode>) =
//        t |> Value.map (fun n ->
//            match n with
//                | Empty -> Empty
//                | Leaf pts -> 
//                    Leaf (n.Count, pts |> Value.map (Array.map id))
//                | Node children ->
//                    Node(n.Count, )
//
//        )


module private MutableTree = 

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

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module OctreeNode =
    

        let private partition (cell : GridCell) (points : Point[]) =
            let c = cell.Center

            let lists = Array.init 8 (fun _ -> System.Collections.Generic.List())

            for p in points do
                let x = if p.Position.X > c.X then 4 else 0
                let y = if p.Position.Y > c.Y then 2 else 0
                let z = if p.Position.Z > c.Z then 1 else 0
                let index = x + y + z
       
                lists.[index].Add(p)

            lists |> Array.map CSharpList.toArray

        let count (n : OctreeNode) =
            match n with
                | Empty -> 0
                | n -> n.Count

        let rec build (parent : Octree) (cell : GridCell) (data : Point[]) =
            if data.Length = 0 then
                Empty

            elif data.Length <= parent.splitCount then
                Leaf(data.Length, parent.store.Ref data)

            else
                let childPoints = data |> partition cell

                let subtrees = 
                    childPoints 
                        |> Array.mapi (fun i pts -> 
                            let inner = build parent (cell.GetChild i) pts
                            parent.store.Ref inner :> thunk<_>
                        ) 

                Node(data.Length, parent.store.Ref [||], subtrees)

        let rec addContained (parent : Octree) (cell : GridCell) (node : thunk<OctreeNode>) (data : Point[]) =
            if data.Length = 0 then
                ()
            else
                let n = !node
                let cnt = count n

                if data.Length + cnt <= parent.splitCount then
                    match n with
                        | Empty -> 
                            node := Leaf(data.Length, parent.store.Ref data)

                        | Leaf(pts) -> 
                            pts := Array.append !pts data
                            node := Leaf(cnt + data.Length, pts)

                        | Node _ ->
                            failwith "[Octree] small inner node"

                else
                    match n with
                        | Empty -> 
                            node := build parent cell data

                        | Leaf content ->
                            let data = Array.append !content data
                            content.Delete()
                            node := build parent cell data

                        | Node(pts, children) ->
                            partition cell data |> Array.iteri (fun i data ->
                                addContained parent (cell.GetChild i) children.[i] data
                            )
                            node := Node(cnt + data.Length, pts, children)

        let rec merge (parent : Octree) (cell : GridCell) (l : thunk<OctreeNode>) (r : thunk<OctreeNode>) =
            let lt = !l
            let rt = !r
            
            match lt, rt with
                | Empty, _ -> 
                    l.Delete()
                    r

                | _, Empty -> 
                    r.Delete()
                    l

                | Node(lp,lc), Node(rp,rc) ->
                    let newChildren = Array.mapi2 (fun i l r -> merge parent (cell.GetChild i) l r) lc rc
                    l := Node(lt.Count + rt.Count, lp, newChildren)
                    r.Delete()
                    rp.Delete()
                    l

                | Leaf(lp), _ ->
                    addContained parent cell r !lp
                    lp.Delete()
                    l.Delete()
                    r

                | _, Leaf(rp) ->
                    addContained parent cell l !rp
                    rp.Delete()
                    r.Delete()
                    l



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Octree =
    open System
    open System.Threading
    open System.Collections.Generic
    open System.Collections.Concurrent
    open System.Diagnostics


    let isRendering (node : LodDataNode)(hull: FastHull3d)(viewTrafo: Trafo3d)(frustum: Frustum)(wantedNearPlaneDistance: float)= 
                    if hull.Intersects(node.bounds) && node.inner then

                        let bounds = node.bounds

                        let depthRange =
                            bounds.ComputeCorners()
                                |> Array.map viewTrafo.Forward.TransformPos
                                |> Array.map (fun v -> -v.Z)
                                |> Range1d

                        let depthRange = Range1d(clamp frustum.near frustum.far depthRange.Min, clamp frustum.near frustum.far depthRange.Max)
                        let projAvgDistance =
                            abs (node.granularity / depthRange.Min)


                        // add all inner nodes to the result too
                        if projAvgDistance > wantedNearPlaneDistance then
                            true
                        else
                            false
                    else
                        false


    module private Internal = 
        open MutableTree
        
        let build (splitCount : int) (store : Database) (points : Point[]) =
            let bb = Box3d(points |> Seq.map (fun p -> p.Position))
            let cell = GridCell.ofBox bb

            let emptyTree =
                {
                    offset = V3d.Zero
                    splitCount = splitCount
                    pointCount = points.Length
                    store = store
                    root = store.Ref Empty
                    bounds = bb
                    cell = cell
                }

            OctreeNode.addContained emptyTree cell emptyTree.root points
            emptyTree

        let private wrap (db : Database) (l : Octree) =
            let index = l.cell.IndexInParent

            let mutable found = false
            let children = Array.init 8 (fun i -> if i = index then found <- true; l.root else db.Ref Empty :> thunk<_>)

            if not found then
                Log.warn "bad index in parent: %A" index

            { l with 
                cell = l.cell.Parent
                root = db.Ref (Node(l.pointCount, db.Ref [||], children))
            }

        let merge (l : Octree) (r : Octree) =
            try
                let db = l.store
                let mutable smaller, greater =
                    if l.cell.Exp < r.cell.Exp then l, r
                    else r, l

                // bring both cells to the same exponent
                while smaller.cell.Exp <> greater.cell.Exp do
                    smaller <- wrap db smaller

                // wrap both until their cells are identical
                while smaller.cell <> greater.cell do
                    smaller <- wrap db smaller
                    greater <- wrap db greater


                { 
                    offset = V3d.Zero
                    splitCount = l.splitCount
                    pointCount = l.pointCount + r.pointCount
                    store = l.store
                    root = OctreeNode.merge smaller smaller.cell smaller.root greater.root
                    bounds = Box3d.Union(l.bounds, r.bounds)
                    cell = smaller.cell
                }
            with e ->
                Log.warn "merge faulted"
                Log.error "%A" e
                raise e
 
        let buildTree (db : Database) (splitThreshold : int) (offset : V3d) (approximateChunks : int) (points : seq<Point[]>) =

            let mutable loaded = 0
            let mutable built = 0
            let mutable merged = 0

            let sw = Stopwatch()
            sw.Start()

//            let map s chunk =
//                Interlocked.Increment(&loaded) |> ignore
//                let res = build splitThreshold db chunk
//                Interlocked.Increment(&built) |> ignore
//                Some res
//
//            let combine l r =
//                match l,r with
//                    | None, r -> r
//                    | l, None -> l
//                    | Some l, Some r -> 
//                        let res = merge l r
//                        Interlocked.Increment(&merged) |> ignore
//                        Some res
//
//            let create () =
//                None
//
//            let task = 
//                async {
//                    do! Async.SwitchToNewThread()
//                    let r = 
//                        points 
//                            |> Nessos.Streams.ParStream.ofSeq
//                            |> Nessos.Streams.ParStream.withDegreeOfParallelism 1
//                            |> Nessos.Streams.ParStream.map id
//                            |> Nessos.Streams.ParStream.withDegreeOfParallelism 4
//                            |> Nessos.Streams.ParStream.fold map combine create
//
//                    return r.Value
//                } |> Async.StartAsTask

            let task = 
                points 
                    |> PSeq.ofSeq
                    |> PSeq.map (fun chunk -> 
                        Interlocked.Increment(&loaded) |> ignore
                        let res = build splitThreshold db chunk
                        Interlocked.Increment(&built) |> ignore
                        res
                        )
                    |> PSeq.reduce (fun l r -> 
                        let res = merge l r
                        Interlocked.Increment(&merged) |> ignore
                        res
                        )


            Report.Progress(0.0)
            let reportWatch = Stopwatch()
            reportWatch.Start()
            let total = approximateChunks
            while not task.IsCompleted do
                if reportWatch.Elapsed.TotalMilliseconds > 500.0 then
                    let time = sw.Elapsed.TotalSeconds

                    let overall = float merged / float (total - 1)
                    //Report.Progress(float merged / float (total - 1) |> clamp 0.0 1.0)
                    Log.line "memory:    %.3fMB" (float db.BlobStore.Memory.Bytes / float (1024 * 1024))
                    Log.line "parser:    %.2f%%" (100.0 * float loaded / float total) //(100.0 * float parsedPoints / float estimatedPointCount)
                    Log.line "building:  %.2f%%" (100.0 * float built / float total) //(100.0 * float processedChunks / estimatedChunks)
                    Log.line "merge:     %.2f%%" (100.0 * float merged / float (total - 1)) //(100.0 * float merged / esitmatedMerges)
                    if overall > 0.0 then
                        Log.line "time:      %A" (TimeSpan.FromSeconds(time))
                        Log.line "remaining: %A" (TimeSpan.FromSeconds(time / overall - time))

                    reportWatch.Restart()
                Thread.Sleep(10)
            reportWatch.Stop()
            
            let res = task.Result
            sw.Stop()
            res


        // Computes the Normals for an octree
        let buildNormals(tree : Octree) = 
            
            // Compute one Normal for the point in the given neighborhood
//            let computeNormal( point : V3d)(neighbors : V3d[]) =
//                if (neighbors |> Array.length) < 2 then V3d.Zero
//                else 
//                    let plane = Helper.fittingPlane(neighbors)
//                    plane.Normal
//
//            // Compute Normals for one node
//            let computeNormals (node : OctreeNode) : Point[] = 
//
//                // Retrieve points from cell
//                let points = !node.Points |> Array.map (fun p -> p.Position)           
//            
//                // Create a kdTree
//                let kdTree = points.CreateRkdTreeDist2 (1e-6)  
//
//                let points = !node.Points;
//                let pointsWithNormals = points |> Array.map  (fun p -> 
//                                                    let point       = p.Position
//                                                    let indices     = kdTree.GetClosest(point,1.0, 10) |> Seq.toList
//                                                    let neighbors   = indices |> Seq.map (fun i -> points.[(int i.Index)].Position) |> Seq.toArray                                           
//                                                    let normal      =  computeNormal point neighbors
//                                                    Point(p.Position, normal, p.Color)
//                                                    //Point(p.Position, p.Color)
//                                                    )
//                pointsWithNormals
//                   
//            // Traverse the octree
//            let rec traverse (node : OctreeNode) =                         
//                match node with
//                | Empty             -> ()
//                | Leaf _            -> node.Points := computeNormals node                                                                                                             
//                | Node (_,children) -> children |> Array.iter(fun child -> traverse !child )
//
//            // Traverse the octree
//            traverse tree.root.Value

            tree 

        // Build Level of Detail
        let buildLod (tree : Octree) =

            let mutable visited = 0
            let mutable lastPrint = 0
            let step = tree.pointCount / 100

            let newCell = GridCell(0,0,0,tree.cell.Exp)
            let offset = newCell.Center - tree.cell.Center

            let empty = [||]
            let rec traverse (n : OctreeNode) =
                match n with 
                    | Empty -> 
                        empty

                    | Leaf pts -> 
                        let all = !pts |> Array.map (fun p -> Point(p.Position + offset, p.Normal, p.Color))
                        //let all = !pts |> Array.map (fun p -> Point(p.Position + offset, p.Color))
                        pts := all
                        visited <- visited + all.Length

                        if visited - lastPrint > step then
                            Report.Progress(float visited / float tree.pointCount)
                            lastPrint <- visited

                        all

                    | Node(pts, children) ->
                        let all = children |> Array.collect (fun c -> traverse c.Value)
                        let portion = float tree.splitCount / float all.Length |> clamp 0.0 1.0
                        let lod = all.TakeRandomly portion |> Seq.toArray
                        pts := lod
                        lod
            
            traverse tree.root.Value |> ignore

            { tree with
                cell = newCell
                bounds = tree.bounds.Translated(offset)
                offset = offset
            }


        // Computes the Normals for an octree
        let buildRkDTrees(tree : Octree) = 
            
//            let buildRkDTree(node: OctreeNode) = 
//                let positions = node.Points.Value |> Array.map(fun p -> p.Position)
//                positions.CreateKdTreeDist1(1e-6)    
//            
//                   
//            // Traverse the octree
//            let rec traverse (node : OctreeNode) =                         
//                match node with
//                | Empty             -> ()
//                | Leaf _            -> ()                                                                                                            
//                | Node (_,children) -> children |> Array.iter(fun child -> traverse !child )
//
//            // Traverse the octree
//            traverse tree.root.Value
            tree 


    let private rand = Random()


    let build (db : Database) (splitThreshold : int) (offset : V3d) (approximateChunks : int) (points : seq<Point[]>) : Octree =
        Log.startTimed "building Octree"

        Log.startTimed "building tree"
        let res = Internal.buildTree db splitThreshold offset approximateChunks points
        Log.stop()


        Log.startTimed "post-processing"
        Log.startTimed "post-processing: Compute Normals"
        let res = Internal.buildNormals res
        Log.stop()

        Log.startTimed "post-processing: Build LoD"
        let res = Internal.buildLod res
        Log.stop()

        Log.startTimed "post-processing: Build RkDTrees"
        let res = Internal.buildRkDTrees res
        Log.stop()

        
        res

    

module Pts =
    
    [<AutoOpen>]
    module private Chars =
        [<Literal>]
        let cr = 13uy

        [<Literal>]
        let lf = 10uy

        [<Literal>]
        let space = 32uy

        [<Literal>]
        let zero = 48uy

        [<Literal>]
        let plus = 43uy

        [<Literal>]
        let minus = 45uy


    type ByteString(stream : Stream) =
        
        static let exp =
            Array.init 64 (fun i -> pown 10.0 -i)
        
        let buffer = Array.zeroCreate (1 <<< 24)
        let mutable bufferSize = 0
        let mutable offset = 0
        let mutable streamPos = 0L
        let mutable streamLength = stream.Length

        let tryFillBuffer() =
            if offset < bufferSize then
                true

            elif stream.Position < stream.Length then
                streamPos <- stream.Position
                bufferSize <- stream.Read(buffer, 0, buffer.Length)
                offset <- 0
                offset < bufferSize

            else
                streamPos <- stream.Length
                offset <- 0
                bufferSize <- 0
                false

        member x.Peek(res : byref<byte>) =
            if tryFillBuffer() then
                res <- buffer.[offset]
                true
            else
                false

        member x.Read(res : byref<byte>) =
            if tryFillBuffer() then
                res <- buffer.[offset]
                offset <- offset + 1
                true
            else
                false

        member x.Consume(c : char) =
            if tryFillBuffer() then
                if buffer.[offset] = byte c then
                    offset <- offset + 1
                    true
                else
                    false
            else
                false

        member x.IgnoreSpaces() =
            let mutable cnt = 0
            let mutable c = 0uy
            while x.Peek(&c) && c = space do
                offset <- offset + 1
                cnt <- cnt + 1

            cnt
  
        member x.ReadToNewLine() =
            let mutable cnt = 0
            let mutable c = 0uy
            while x.Peek(&c) && c <> cr && c <> lf do
                offset <- offset + 1
                cnt <- cnt + 1

            if c = cr || c = lf then
                offset <- offset + 1
                cnt <- cnt + 1

                if x.Peek(&c) && c = lf then
                    offset <- offset + 1
                    cnt <- cnt + 1

                cnt > 0
            else
                false
                
                
        member x.ReadInt(res : byref<int>) =
            if tryFillBuffer() then
                let off = stream.Position + int64 offset
                let rec read (pos : bool) (value : int) =
                    if tryFillBuffer() then
                        let c = int buffer.[offset]  - int zero
                        match c with
                            | -5 -> offset <- offset + 1; read true 0
                            | -3 -> offset <- offset + 1; read false 0
                            | v ->
                                if v >= 0 && v < 10 then
                                    offset <- offset + 1;
                                    read pos (value * 10 + v)
                                else
                                    if pos then value 
                                    else -value
                    else
                        if pos then value
                        else -value

                let v = read true 0
                if stream.Position + int64 offset > off then
                    res <- v
                    true
                else
                    false

            else
                false

        member x.ReadIntLength(isPositive : byref<bool>, res : byref<int>) =
            if tryFillBuffer() then
                let mutable cnt = 0
                let mutable pos = isPositive
                let rec read (value : int) =
                    if tryFillBuffer() then
                        let c = int buffer.[offset]  - int zero
                        match c with
                            | -5 -> offset <- offset + 1; pos <- true; read 0
                            | -3 -> offset <- offset + 1; pos <- false; read 0
                            | v ->
                                if v >= 0 && v < 10 then
                                    cnt <- cnt + 1
                                    offset <- offset + 1;
                                    
                                    read (value * 10 + v)
                                else
                                    value
                    else
                        value

                let v = read 0
                isPositive <- pos
                res <- v
                cnt

            else
                0

        member x.ReadDouble(res : byref<float>) =
            if tryFillBuffer() then
                
                let mutable lpos = true
                let mutable hpos = true
                let mutable h = 0
                let mutable l = 0

                let off = streamPos + int64 offset
                let check() = streamPos + int64 offset > off


                if x.ReadIntLength(&hpos, &h) >= 0 then
                    if x.Consume '.' then
                        let len = x.ReadIntLength(&lpos, &l)

                        let value = 
                            if hpos then float h + float l * exp.[len]
                            else float -h - float l * exp.[len]

                        res <- value
                        check()
                    else
                        let value = 
                            if hpos then float h
                            else float -h
                        res <- value
                        check()

                else
                    check()

            else
                false

        member x.End = offset >= bufferSize && streamPos >= streamLength

    let private readPoint (str : ByteString, p : byref<Point>) =

        let mutable x = 0.0
        let mutable y = 0.0
        let mutable z = 0.0

        let mutable r = 0
        let mutable g = 0
        let mutable b = 0

        let mutable foo = 0

        if str.ReadDouble(&x) && str.IgnoreSpaces() > 0 && 
           str.ReadDouble(&y) && str.IgnoreSpaces() > 0 && 
           str.ReadDouble(&z) && str.IgnoreSpaces() > 0 &&
           
           str.ReadInt(&foo) && str.IgnoreSpaces() > 0 &&

           str.ReadInt(&r) && str.IgnoreSpaces() > 0 &&
           str.ReadInt(&g) && str.IgnoreSpaces() > 0 &&
           str.ReadInt(&b) then

           str.ReadToNewLine() |> ignore
           p <- Point(V3d(x,y,z), V3d.Zero, C4b(byte r, byte g, byte b,255uy))
           //p <- Point(V3d(x,y,z), C4b(byte r, byte g, byte b,255uy))

           true

        else
            str.ReadToNewLine() |> ignore
            false

    let readChunked (chunkSize : int) (file : string) =
        seq {
            let stream = File.OpenRead file
            let str = ByteString(stream)

            try
                while not str.End do
                    let mutable cnt = 0
                    let chunk = Array.zeroCreate chunkSize

                    do  let mutable pt = Point()
                        while cnt < chunkSize && not str.End do
                            if readPoint(str, &pt) then
                                chunk.[cnt] <- pt
                                cnt <- cnt + 1

                    if cnt = chunkSize then yield chunk
                    else yield Array.take cnt chunk
            finally
                stream.Dispose()
        } 
                
    let approximatePointCount (file : string) =
        FileInfo(file).Length / 57L |> int


    let test(file : string) =
        use ms = File.OpenRead file
        let str = ByteString(ms)

        let mutable average = V3d.Zero
        let mutable count = 0

        Log.startTimed "reading"
        let mutable pt = Point()
        let sw = Stopwatch()
        sw.Start()
        while not str.End do
            if readPoint(str, &pt) then
                average <- average + pt.Position
                count <- count + 1

                if count % 1000000 = 0 then
                    Log.line "read %dm points" (count / 1000000)

        sw.Stop()
        Log.line "rate:    %.3fMB/s" (float ms.Length / (1024.0 * 1024.0 * sw.Elapsed.TotalSeconds))
        Log.line "average: %A" (average / float count)
        Log.line "count:   %d" count
        Log.stop()






