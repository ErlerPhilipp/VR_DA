namespace Aardvark.VR

open FShade

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.Semantics.RenderObjectSemantics

open System
open System.Collections.Generic

open System.Runtime.InteropServices
#nowarn "9"

module private NewSpatialDict =
    
    type private OctNodeInfo =
        {
            pointsPerLeaf : int
            minLeafVolume : float
        }

    [<AllowNullLiteral>]
    type private OctNode<'a> =
        class
            val mutable public Center : V3d
            val mutable public Content : List<V3d * 'a>
            val mutable public Children : OctNode<'a>[]
        
            new(center, p,c) = { Center = center; Content = p; Children = c }
        end

    [<AutoOpen>]
    module private NodePatterns =
        let inline (|Empty|Leaf|Node|) (n : OctNode<'a>) =
            if isNull n then Empty
            elif isNull n.Children then Leaf n.Content
            else Node n.Children
        let Empty<'a> : OctNode<'a> = null
        let Leaf(center, v) = OctNode(center, v, null)
        let Node(center, c) = OctNode(center, null, c)    

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module private OctNode =

        let cluster (cell : GridCell) (values : array<V3d * int>) =
            let c = cell.Center

            let lists = Array.init 8 (fun i -> List<V3d * int>(values.Length))

            for (p,i) in values do
                let index =
                    (if p.X > c.X then 4 else 0) +
                    (if p.Y > c.Y then 2 else 0) +
                    (if p.Z > c.Z then 1 else 0)

                lists.[index].Add(p,i)

            lists
                |> Array.indexed
                |> Array.choose (fun (i,l) -> 
                    if l.Count > 0 then
                        Some (i, CSharpList.toArray l)
                    else
                        None
                )
 
        let rec build (info : OctNodeInfo) (cell : GridCell) (r : float) (f : V3d -> 'a) (output : array<'a>) (points : array<V3d * int>) =
            if points.Length = 0 then
                Empty

            elif points.Length < info.pointsPerLeaf || cell.ChildVolume < info.minLeafVolume then
                let values = List<V3d * 'a>()
                let rec findOrAdd (offset : int) (p : V3d) =
                    if offset >= values.Count then
                        let v = f p
                        values.Add(p, v)
                        v
                    else
                        let (k, v) = values.[offset]

                        let found = (k - p).Abs.AllSmallerOrEqual r 
                        if found then
                            v
                        else
                            findOrAdd (offset + 1) p

                for (p,i) in points do
                    output.[i] <- findOrAdd 0 p


                Leaf (cell.Center, values)

            else
                let children = Array.zeroCreate 8
                for (child, points) in cluster cell points do
                    children.[child] <- build info (cell.GetChild child) r f output points

                Node(cell.Center, children)

        let rec query (point : V3d) (r : float) (cell : GridCell) (n : OctNode<'a>) (res : List<'a>) =
            match n with
                | Empty -> 
                    ()

                | Leaf values ->
                    for (p,v) in values do
                        if (p - point).Abs.AllSmallerOrEqual r then
                            res.Add v

                | Node children ->
                    let center = n.Center

                    let indices = HashSet.ofList [0..7]

                    if point.X > center.X + r then
                        indices.IntersectWith [4; 5; 6; 7]
                    elif point.X < center.X - r then
                        indices.IntersectWith [0; 1; 2; 3]
                        
                    if point.Y > center.Y + r then
                        indices.IntersectWith [2; 3; 6; 7]
                    elif point.Y < center.Y - r then
                        indices.IntersectWith [0; 1; 4; 5]

                    if point.Z > center.Z + r then
                        indices.IntersectWith [1; 3; 5; 7]
                    elif point.Z < center.Z - r then
                        indices.IntersectWith [0; 2; 4; 6]

                    for i in indices do
                        query point r (cell.GetChild i) children.[i] res

    type SpatialDict<'a>(r : float, data : V3d[], f : V3d -> 'a) =

        let info = { pointsPerLeaf = 10; minLeafVolume = pown 0.00001 3 }
        let mutable bounds = Box3d data
        let mutable cell = GridCell.Containing bounds
        let res = Array.zeroCreate data.Length
        let mutable root = OctNode.build info cell r f res (data |> Array.mapi (fun i v -> v, i))

        member x.Values = res

        member x.Query(point : V3d, maxDistance : float) =
            let res = List<'a>()
            OctNode.query point maxDistance cell root res
            res :> seq<_>

[<StructLayout(LayoutKind.Sequential)>]
type TriangleAdjacency =
    struct
        val mutable public I0 : int
        val mutable public N01 : int
        val mutable public I1 : int
        val mutable public N12 : int
        val mutable public I2 : int
        val mutable public N20 : int

        static member Size = 6 * sizeof<int>
        
        member x.CopyTo(arr : int[], start : int) =
            arr.[start + 0] <- x.I0
            arr.[start + 1] <- x.N01
            arr.[start + 2] <- x.I1
            arr.[start + 3] <- x.N12
            arr.[start + 4] <- x.I2
            arr.[start + 5] <- x.N20

        new(i0, i1, i2) = { I0 = i0; I1 = i1; I2 = i2; N01 = -1; N12 = -1; N20 = -1 }
        new(i0, i1, i2, n01, n12, n20) = { I0 = i0; I1 = i1; I2 = i2; N01 = n01; N12 = n12; N20 = n20 }

    end

type TriangleMesh(triangles : Triangle3d[]) =
    let indices, positions, pointTriangles =
        let points = triangles |> Array.collect (fun t -> [|t.P0; t.P1; t.P2|])

        let indexedPoints = List<V3d>()
        let add (p : V3d) =
            let i = indexedPoints.Count
            indexedPoints.Add p
            i

        let store = NewSpatialDict.SpatialDict<int>(Constant.PositiveTinyValue, points, add)

        let indices = store.Values
        let positions = indexedPoints |> CSharpList.toArray

        let pointTriangles = Array.create positions.Length Set.empty 

        for ti in 0 .. triangles.Length - 1 do
            let i0 = indices.[3 * ti + 0]
            let i1 = indices.[3 * ti + 1]
            let i2 = indices.[3 * ti + 2]

            pointTriangles.[i0] <- Set.add ti pointTriangles.[i0]
            pointTriangles.[i1] <- Set.add ti pointTriangles.[i1]
            pointTriangles.[i2] <- Set.add ti pointTriangles.[i2]

        indices, positions, pointTriangles

    let mutable adjacency : TriangleAdjacency[] = null

    member x.Indices = indices
    member x.Positions = positions
    member x.PointTriangles = pointTriangles

    member x.Adjacency =
        if not (isNull adjacency) then 
            adjacency
        else
            adjacency <- 
                Array.init (indices.Length / 3) (fun ti ->
                    let i0 = indices.[3 * ti + 0]
                    let i1 = indices.[3 * ti + 1]
                    let i2 = indices.[3 * ti + 2]
                    let points = Set.ofList [i0; i1; i2]

                    let n0 = pointTriangles.[i0] |> Set.remove ti
                    let n1 = pointTriangles.[i1] |> Set.remove ti
                    let n2 = pointTriangles.[i2] |> Set.remove ti

                    let n01 = Set.intersect n0 n1
                    let n12 = Set.intersect n1 n2
                    let n20 = Set.intersect n2 n0

                    let get (other : int) (s : Set<int>) =
                        match Set.count s with
                            | 1 -> 
                                let oi = Seq.head s

                                let otherPoints = 
                                    Set.ofList [
                                        indices.[3 * oi + 0]
                                        indices.[3 * oi + 1]
                                        indices.[3 * oi + 2]
                                    ]

                                let notMine = Set.difference otherPoints points
                                if Set.count notMine = 1 then Seq.head notMine
                                else other


                            | _ -> 
                                other
                            
                    TriangleAdjacency(i0, i1, i2, get i2 n01, get i0 n12, get i1 n20)
                ) 
            adjacency

[<ReflectedDefinition>]
module VolumeShader =
    open FShade

    type Vertex = 
        {
            [<Position>]                pos     : V4d
            [<Semantic "Inf">]          inf     : V4d
            [<WorldPosition>]           wp      : V4d
            [<Semantic "Light">]        l       : V3d
        }

    type SimpleVertex = 
        {
            [<Position>]                p     : V4d
        }

    type UniformScope with
        member x.LightPos : V3d = uniform?LightPos

    let isOutlineEdge (light : V3d) (p0 : V3d) (p1 : V3d) (inside : V3d) (test : V3d) =
        let line = Vec.normalize (p1 - p0)
        let dir = Vec.normalize (p0 - light)
        let n = Vec.cross dir line
        let n = Vec.normalize n
        let d = Vec.dot light n
        let hi = Vec.dot n inside
        let ht = Vec.dot n test

        if (hi >= d && ht < d) || (hi <= d && ht > d) then
            false
        else
            true 

    let thick (width : float) (p0n : V4d) (p1n : V4d) =
        let halfWidth = width / V2d uniform.ViewportSize
        let dir = p1n.XYZ / p1n.W - p0n.XYZ / p0n.W |> Vec.normalize


        let d0 = V4d(dir.X * halfWidth.X, dir.Y * halfWidth.Y, dir.Z, 0.0) * p0n.W
        let d1 = V4d(dir.X * halfWidth.X, dir.Y * halfWidth.Y, dir.Z, 0.0) * p1n.W
        let n0 = V4d(-dir.Y * halfWidth.X, dir.X * halfWidth.Y, 0.0, 0.0) * p0n.W
        let n1 = V4d(-dir.Y * halfWidth.X, dir.X * halfWidth.Y, 0.0, 0.0) * p1n.W

        (p0n - d0 - n0), (p1n + d1 - n1), (p0n - d0 + n0), (p1n + d1 + n1)

    let vertex (v : Vertex) =
        vertex {
            let p = uniform.ViewProjTrafo * v.pos
            let l = uniform.ViewProjTrafo * V4d(uniform.LightLocation, 1.0)

            return {
                pos = p
                inf = p - l
                wp = v.pos
                l = uniform.LightLocation
            }
        }

    let extrude (a : TriangleAdjacency<Vertex>) =
        triangle {
            let u = a.P1.inf - a.P0.inf |> Vec.xyz
            let v = a.P2.inf - a.P0.inf |> Vec.xyz
            let n = Vec.cross u v |> Vec.normalize
            let h = Vec.dot n a.P0.inf.XYZ
            let ff = h > 0.0

            let w0 = a.P0.wp.XYZ
            let w1 = a.P1.wp.XYZ
            let w2 = a.P2.wp.XYZ
            let w01 = a.N01.wp.XYZ
            let w12 = a.N12.wp.XYZ
            let w20 = a.N20.wp.XYZ

            let p0n = a.P0.pos + V4d(0.0,0.0,0.0001,0.0)
            let p1n = a.P1.pos + V4d(0.0,0.0,0.0001,0.0)
            let p2n = a.P2.pos + V4d(0.0,0.0,0.0001,0.0)
                    
            let light = a.P0.l

            if ff then
                yield { p = p0n }
                yield { p = p2n }
                yield { p = p1n }
                restartStrip()
                yield { p = a.P0.inf }
                yield { p = a.P1.inf }
                yield { p = a.P2.inf }
                restartStrip()
            else
                yield { p = p0n }
                yield { p = p1n }
                yield { p = p2n }
                restartStrip()
                yield { p = a.P0.inf }
                yield { p = a.P2.inf }
                yield { p = a.P1.inf }
                restartStrip()


            if isOutlineEdge light w0 w1 w2 w01 then
                // Line01 is an outline

                if ff then 
                    yield { p = p0n }
                    yield { p = p1n }
                    yield { p = a.P0.inf }
                    yield { p = a.P1.inf }
                    restartStrip()
                else
                    yield { p = p1n }
                    yield { p = p0n }
                    yield { p = a.P1.inf }
                    yield { p = a.P0.inf }
                    restartStrip()



                ()

            if isOutlineEdge light w1 w2 w0 w12 then
                // Line12 is an outline

                if ff then 
                    yield { p = p1n }
                    yield { p = p2n }
                    yield { p = a.P1.inf }
                    yield { p = a.P2.inf }
                    restartStrip()
                else
                    yield { p = p2n }
                    yield { p = p1n }
                    yield { p = a.P2.inf }
                    yield { p = a.P1.inf }
                    restartStrip()

            if isOutlineEdge light w2 w0 w1 w20 then
                // Line20 is an outline
                if ff then 
                    yield { p = p2n }
                    yield { p = p0n }
                    yield { p = a.P2.inf }
                    yield { p = a.P0.inf }
                    restartStrip()
                else
                    yield { p = p0n }
                    yield { p = p2n }
                    yield { p = a.P0.inf }
                    yield { p = a.P2.inf }
                    restartStrip()

        }

    let outline (a : TriangleAdjacency<Vertex>) =
        line {
            let u = a.P1.inf - a.P0.inf |> Vec.xyz
            let v = a.P2.inf - a.P0.inf |> Vec.xyz
            let n = Vec.cross u v |> Vec.normalize
            let h = Vec.dot n a.P0.inf.XYZ
            let ff = h > 0.0

            let w0 = a.P0.wp.XYZ
            let w1 = a.P1.wp.XYZ
            let w2 = a.P2.wp.XYZ
            let w01 = a.N01.wp.XYZ
            let w12 = a.N12.wp.XYZ
            let w20 = a.N20.wp.XYZ

            let p0n = a.P0.pos
            let p1n = a.P1.pos
            let p2n = a.P2.pos
                    
            let light = a.P0.l

            if isOutlineEdge light w0 w1 w2 w01 then
                // Line01 is an outline

                if ff then 
                    yield { p = p0n }
                    yield { p = p1n }
                    restartStrip()
                else
                    yield { p = p1n }
                    yield { p = p0n }
                    restartStrip()



                ()

            if isOutlineEdge light w1 w2 w0 w12 then
                // Line12 is an outline

                if ff then 
                    yield { p = p1n }
                    yield { p = p2n }
                    restartStrip()
                else
                    yield { p = p2n }
                    yield { p = p1n }
                    restartStrip()

            if isOutlineEdge light w2 w0 w1 w20 then
                // Line20 is an outline
                if ff then 
                    yield { p = p2n }
                    yield { p = p0n }
                    restartStrip()
                else
                    yield { p = p0n }
                    yield { p = p2n }
                    restartStrip()

        }

    let thickOutline (a : TriangleAdjacency<Vertex>) =
        triangle {
            let u = a.P1.inf - a.P0.inf |> Vec.xyz
            let v = a.P2.inf - a.P0.inf |> Vec.xyz
            let n = Vec.cross u v |> Vec.normalize
            let h = Vec.dot n a.P0.inf.XYZ
            let ff = h > 0.0

            let w0 = a.P0.wp.XYZ
            let w1 = a.P1.wp.XYZ
            let w2 = a.P2.wp.XYZ
            let w01 = a.N01.wp.XYZ
            let w12 = a.N12.wp.XYZ
            let w20 = a.N20.wp.XYZ

            let p0n = a.P0.pos
            let p1n = a.P1.pos
            let p2n = a.P2.pos
                    
            let light = a.P0.l

            let width = 5.0


            if isOutlineEdge light w0 w1 w2 w01 then
                // Line01 is an outline
                if ff then 
                    let (a,b,c,d) = thick width p0n p1n 
                    yield { p = a }
                    yield { p = b }
                    yield { p = c }
                    yield { p = d }
                    restartStrip()
                else
                    let (a,b,c,d) = thick width p1n p0n 
                    yield { p = a }
                    yield { p = b }
                    yield { p = c }
                    yield { p = d }
                    restartStrip()



                ()

            if isOutlineEdge light w1 w2 w0 w12 then
                // Line12 is an outline
                if ff then 
                    let (a,b,c,d) = thick width p1n p2n 
                    yield { p = a }
                    yield { p = b }
                    yield { p = c }
                    yield { p = d }
                    restartStrip()
                else
                    let (a,b,c,d) = thick width p2n p1n 
                    yield { p = a }
                    yield { p = b }
                    yield { p = c }
                    yield { p = d }
                    restartStrip()

            if isOutlineEdge light w2 w0 w1 w20 then
                // Line20 is an outline
                if ff then 
                    let (a,b,c,d) = thick width p2n p0n 
                    yield { p = a }
                    yield { p = b }
                    yield { p = c }
                    yield { p = d }
                    restartStrip()
                else
                    let (a,b,c,d) = thick width p0n p2n 
                    yield { p = a }
                    yield { p = b }
                    yield { p = c }
                    yield { p = d }
                    restartStrip()

        }

module TriangleSet =
    open System.Collections.Generic
    open Aardvark.Base.Monads.Option

    let private tryGetTrianglesAsMod (ro : IRenderObject) =
        option {
            match ro with
                | :? RenderObject as ro ->

                    let mode = ro.Mode
                    let! modelTrafo = ro.Uniforms.TryGetUniform(Ag.emptyScope, Symbol.Create "ModelTrafo")
                    let! positions = ro.VertexAttributes.TryGetAttribute(DefaultSemantic.Positions)
                    let indices = ro.Indices

                    let positionType = positions.ElementType
                    let indexType = match ro.Indices with | Some i -> i.ElementType | _ -> typeof<int> //if isNull ro.Indices then typeof<int> else indices.GetValue().GetType().GetElementType()
                    
                    let toInt : Array -> int[] = PrimitiveValueConverter.arrayConverter indexType
                    let toV3d : Array -> V3d[] = PrimitiveValueConverter.arrayConverter positionType

                    let triangles =
                        Mod.custom(fun self ->
                            let mode = mode.GetValue self
                            let modelTrafo = modelTrafo.GetValue self |> unbox<Trafo3d>
                            let positions = positions.Buffer.GetValue self
                            let indices =
                                match indices with
                                    | Some i -> 
                                        match i.Buffer.GetValue() with
                                            | :? ArrayBuffer as ab -> ab.Data |> toInt
                                            | _ -> null
                                    | _ -> null
//                                if isNull indices then null
//                                else indices.GetValue self |> toInt

                            match positions with
                                | :? ArrayBuffer as ab ->
                                    let data = toV3d ab.Data |> Array.map modelTrafo.Forward.TransformPos
                                        
                                    match mode with
                                        | IndexedGeometryMode.TriangleList ->
                                            if isNull indices then
                                                let triangles = Array.zeroCreate (data.Length / 3)
                                                for i in 0..triangles.Length-1 do
                                                    let p0 = data.[3*i + 0]
                                                    let p1 = data.[3*i + 1]
                                                    let p2 = data.[3*i + 2]
                                                    triangles.[i] <- Triangle3d(p0, p1, p2)

                                                triangles
                                            else
                                                let triangles = Array.zeroCreate (indices.Length / 3)
                                                for i in 0..triangles.Length-1 do
                                                    let p0 = data.[indices.[3*i + 0]]
                                                    let p1 = data.[indices.[3*i + 1]]
                                                    let p2 = data.[indices.[3*i + 2]]
                                                    triangles.[i] <- Triangle3d(p0, p1, p2)

                                                triangles

                                        | IndexedGeometryMode.TriangleStrip ->
                                            if isNull indices then
                                                let triangles = Array.zeroCreate (data.Length - 2)

                                                let mutable p0 = data.[0]
                                                let mutable p1 = data.[1]
                         
                                                for i in 2..triangles.Length-1 do
                                                    let p2 = data.[i]
                                                    triangles.[i - 2] <- Triangle3d(p0, p1, p2)

                                                    if i % 2 = 0 then p0 <- p2
                                                    else p1 <- p2


                                                triangles
                                            else
                                                let triangles = Array.zeroCreate (indices.Length - 2)

                                                let mutable p0 = data.[indices.[0]]
                                                let mutable p1 = data.[indices.[1]]
                         
                                                for i in 2..triangles.Length-1 do
                                                    let p2 = data.[indices.[i]]
                                                    triangles.[i - 2] <- Triangle3d(p0, p1, p2)

                                                    if i % 2 = 0 then p0 <- p2
                                                    else p1 <- p2

                                                triangles
                                        | _ ->
                                            [||]

                                | _ ->
                                    failwith "not implemented"


                        )


                    return triangles



                | _ ->
                    return! None
        }

    let private tryGetTrianglesAsASet (ro : IRenderObject) =
        option {
            match ro with
                | :? RenderObject as ro ->

                    let mode = ro.Mode
                    let! modelTrafo = ro.Uniforms.TryGetUniform(Ag.emptyScope, Symbol.Create "ModelTrafo")
                    let! positions = ro.VertexAttributes.TryGetAttribute(DefaultSemantic.Positions)
                    let indices = ro.Indices

                    let positionType = positions.ElementType
                    let indexType = match ro.Indices with | Some i -> i.ElementType | _ -> typeof<int> //if isNull ro.Indices then typeof<int> else indices.GetValue().GetType().GetElementType()
                    
                    let toInt : Array -> int[] = PrimitiveValueConverter.arrayConverter indexType
                    let toV3d : Array -> V3d[] = PrimitiveValueConverter.arrayConverter positionType

                    let triangles =
                        ASet.custom(fun self ->
                            let mode = mode.GetValue self
                            let modelTrafo = modelTrafo.GetValue self |> unbox<Trafo3d>
                            let positions = positions.Buffer.GetValue self
                            let indices =
                                match indices with
                                    | Some i -> 
                                        match i.Buffer.GetValue() with
                                            | :? ArrayBuffer as ab -> ab.Data |> toInt
                                            | _ -> null
                                    | _ -> null
                            let newTriangles =
                                match positions with
                                    | :? ArrayBuffer as ab ->
                                        let data = toV3d ab.Data |> Array.map modelTrafo.Forward.TransformPos
                                        
                                        match mode with
                                            | IndexedGeometryMode.TriangleList ->
                                                if isNull indices then
                                                    let triangles = Array.zeroCreate (data.Length / 3)
                                                    for i in 0..triangles.Length-1 do
                                                        let p0 = data.[3*i + 0]
                                                        let p1 = data.[3*i + 1]
                                                        let p2 = data.[3*i + 2]
                                                        triangles.[i] <- Triangle3d(p0, p1, p2)

                                                    triangles
                                                else
                                                    let triangles = Array.zeroCreate (indices.Length / 3)
                                                    for i in 0..triangles.Length-1 do
                                                        let p0 = data.[indices.[3*i + 0]]
                                                        let p1 = data.[indices.[3*i + 1]]
                                                        let p2 = data.[indices.[3*i + 2]]
                                                        triangles.[i] <- Triangle3d(p0, p1, p2)

                                                    triangles

                                            | IndexedGeometryMode.TriangleStrip ->
                                                if isNull indices then
                                                    let triangles = Array.zeroCreate (data.Length - 2)

                                                    let mutable p0 = data.[0]
                                                    let mutable p1 = data.[1]
                         
                                                    for i in 0..triangles.Length-1 do
                                                        let p2 = data.[i + 2]
                                                        triangles.[i] <- Triangle3d(p0, p1, p2)

                                                        if i % 2 = 0 then p0 <- p2
                                                        else p1 <- p2


                                                    triangles
                                                else
                                                    let triangles = Array.zeroCreate (indices.Length - 2)

                                                    let mutable p0 = data.[indices.[0]]
                                                    let mutable p1 = data.[indices.[1]]
                         
                                                    for i in 0..triangles.Length-1 do
                                                        let p2 = data.[indices.[i + 2]]
                                                        triangles.[i] <- Triangle3d(p0, p1, p2)

                                                        if i % 2 = 0 then p0 <- p2
                                                        else p1 <- p2

                                                    triangles
                                            | _ ->
                                                [||]

                                    | _ ->
                                        failwith "not implemented"

                            let newTriangles = 
                                newTriangles |> Seq.filter (fun t -> not t.IsDegenerated) |> HashSet
                            


                            let rem = self.Content |> Seq.filter (newTriangles.Contains >> not) |> Seq.map Rem |> Seq.toList
                            let add = newTriangles |> Seq.filter (self.Content.Contains >> not) |> Seq.map Add |> Seq.toList


                            add @ rem
                        )


                    return triangles



                | _ ->
                    return! None
        }


    let ofRenderObject (o : IRenderObject) =
        o |> tryGetTrianglesAsMod |> Option.get

    let ofRenderObjects (o : aset<IRenderObject>) =
        aset {
            for ro in o do
                match tryGetTrianglesAsASet ro with
                    | Some set -> yield! set
                    | _ -> ()
        }

    let ofSg (sg : ISg) =
        aset {
            for ro in sg.RenderObjects() do
                match tryGetTrianglesAsASet ro with
                    | Some set -> yield! set
                    | _ -> ()
        }

module ShadowVolumes = 

    //place holder
    let handBoxEdgeLength = 0.1
    let handBox = Box3d.FromCenterAndSize(V3d.OOO, handBoxEdgeLength * V3d.III)
    let handSg = Sg.box (Mod.constant C4b.Green) (Mod.constant handBox) 

    let sg =
        handSg |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.diffuseTexture |> toEffect
                DefaultSurfaces.normalMap |> toEffect
                DefaultSurfaces.lighting false |> toEffect
              ]

           //|> normalizeTo (Box3d(-V3d.III, V3d.III))
           |> Sg.trafo (Mod.constant (Trafo3d.FromBasis(V3d.IOO, V3d.OOI, V3d.OIO, V3d.Zero) ))
           |> Sg.translate 0.0 0.0 1.0
           //|> Sg.scale 20.0
//           |> Sg.andAlso (Sg.box' C4b.Red (Box3d.FromMinAndSize(V3d(-1.6, -1.6, 0.0), V3d(0.1,0.2,0.3))))
//           |> Sg.andAlso (Sg.box' C4b.Red (Box3d.FromMinAndSize(V3d(-1.56, -1.57, 0.15), V3d(0.05,0.15,0.3))))
//           |> Sg.andAlso (Sg.fullScreenQuad |> Sg.scale 0.5 |> Sg.transform (Trafo3d.RotationX Constant.PiHalf ) |> Sg.translate -2.0 -2.0 0.5)
           |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.lighting true |> toEffect
            ]

    let writeStencil =
        StencilMode(
            IsEnabled = true,
            Compare = StencilFunction(StencilCompareFunction.Always, 0, 0u),
            OperationFront = 
                StencilOperation(
                    StencilOperationFunction.Keep, 
                    StencilOperationFunction.IncrementWrap, 
                    StencilOperationFunction.Keep
                ),
            OperationBack = 
                StencilOperation(
                    StencilOperationFunction.Keep,
                    StencilOperationFunction.DecrementWrap, 
                    StencilOperationFunction.Keep
                )
        )

    let readStencil =
        StencilMode(
            IsEnabled = true,
            Compare = StencilFunction(StencilCompareFunction.NotEqual, 0, 0xFFFFFFFFu),
            Operation = 
                StencilOperation(
                    StencilOperationFunction.Keep, 
                    StencilOperationFunction.Keep, 
                    StencilOperationFunction.Keep
                )
        )

    let afterMain = RenderPass.after "bla" RenderPassOrder.Arbitrary RenderPass.main
    let afterAfterMain = RenderPass.after "blubb" RenderPassOrder.Arbitrary afterMain
    let final = RenderPass.after "blubber" RenderPassOrder.Arbitrary afterAfterMain
        
    let mesh (sceneGraph : ISg) =
        sceneGraph  |> TriangleSet.ofSg
            |> ASet.toMod
            |> Mod.map (Seq.filter (fun t -> not t.IsDegenerated) >> Seq.toArray >> TriangleMesh)

    let shadowGeometry (sceneGraph : ISg) =
        let index = mesh (sceneGraph) |> Mod.map (fun m -> m.Adjacency.UnsafeCoerce<int>())
        let positions = mesh (sceneGraph) |> Mod.map (fun m -> m.Positions |> Array.map V3f)

        let drawCall = 
            index |> Mod.map (fun i -> 
                DrawCallInfo(
                    FaceVertexCount = i.Length,
                    InstanceCount = 1
                )
            )

        Sg.RenderNode(drawCall, Mod.constant IndexedGeometryMode.TriangleAdjacencyList)
            |> Sg.index index
            |> Sg.vertexAttribute DefaultSemantic.Positions positions


    let shadowVolumes (sceneGraph : ISg) =
        shadowGeometry (sceneGraph)
            |> Sg.stencilMode (Mod.constant writeStencil)
            |> Sg.writeBuffers' (Set.ofList [ DefaultSemantic.Stencil ])
            |> Sg.pass afterMain
            |> Sg.effect [
                VolumeShader.vertex |> toEffect
                VolumeShader.extrude |> toEffect
                DefaultSurfaces.constantColor (C4f(1.0,0.0,0.0,0.0)) |> toEffect
            ]

    let shadows (sceneGraph : ISg) =
        Sg.group' [ 

            let color = C4f(0.0,0.0,0.0,0.0)

            yield shadowVolumes (sceneGraph)

            yield
                Sg.fullScreenQuad
                    |> Sg.pass final
                    |> Sg.depthTest (Mod.constant DepthTestMode.None)
                    |> Sg.writeBuffers' (Set.ofList [DefaultSemantic.Colors])
                    |> Sg.stencilMode (Mod.constant readStencil)
                    |> Sg.blendMode (Mod.constant BlendMode.Blend)
                    |> Sg.effect [
                        DefaultSurfaces.constantColor (C4f(0.0,0.0,0.0,0.8)) |> toEffect
                    ]

        ]
        
    let mode = Mod.init FillMode.Fill