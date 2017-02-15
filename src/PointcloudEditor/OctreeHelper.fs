namespace InteractiveSegmentation

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
open Aardvark.Application
open Aardvark.Application.WinForms
open System.IO
open System.Threading

open Aardvark.Git
open Aardvark.Database
open Aardvark.Base.Native

module OctreeHelper = 
    
    let currentOctree = Mod.init

    let frustum (f : IMod<CameraView>) (proj : IMod<Frustum>) =
        let invViewProj = Mod.map2 (fun v p -> (CameraView.viewTrafo v * Frustum.projTrafo p).Inverse) f proj

        let positions = 
            [|
                V3f(-1.0, -1.0, -1.0)
                V3f(1.0, -1.0, -1.0)
                V3f(1.0, 1.0, -1.0)
                V3f(-1.0, 1.0, -1.0)
                V3f(-1.0, -1.0, 1.0)
                V3f(1.0, -1.0, 1.0)
                V3f(1.0, 1.0, 1.0)
                V3f(-1.0, 1.0, 1.0)
            |]

        let indices =
            [|
                1;2; 2;6; 6;5; 5;1;
                2;3; 3;7; 7;6; 4;5; 
                7;4; 3;0; 0;4; 0;1;
            |]

        let geometry =
            IndexedGeometry(
                Mode = IndexedGeometryMode.LineList,
                IndexedAttributes =
                    SymDict.ofList [
                        DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                        DefaultSemantic.Colors, Array.create indices.Length C4b.Red :> Array
                    ]
            )

        geometry
            |> Sg.ofIndexedGeometry
            |> Sg.trafo invViewProj


    // Gets the values from memory
    let thunkForce (t : thunk<'a>) =  t.Value

    let fullscreenQuadSg =
            Sg.draw IndexedGeometryMode.TriangleStrip
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant  [|V3f(-1.0,-1.0,0.0); V3f(1.0,-1.0,0.0); V3f(-1.0,1.0,0.0);V3f(1.0,1.0,0.0) |])
                |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates (Mod.constant [|V2f.OO; V2f.IO; V2f.OI; V2f.II|])

    let ig ( box : Box3d ) : IndexedGeometry =
        let pa = [|
            V3f(box.Min.X, box.Min.Y, box.Min.Z);
            V3f(box.Max.X, box.Min.Y, box.Min.Z);
            V3f(box.Max.X, box.Max.Y, box.Min.Z);
            V3f(box.Min.X, box.Max.Y, box.Min.Z);
            V3f(box.Min.X, box.Min.Y, box.Max.Z);
            V3f(box.Max.X, box.Min.Y, box.Max.Z);
            V3f(box.Max.X, box.Max.Y, box.Max.Z);
            V3f(box.Min.X, box.Max.Y, box.Max.Z);
            |]

        let pos = [|
                pa.[0]; pa.[1]; pa.[1]; pa.[2]; pa.[2]; pa.[3]; pa.[3]; pa.[0];
                pa.[4]; pa.[5]; pa.[5]; pa.[6]; pa.[6]; pa.[7]; pa.[7]; pa.[4];
                pa.[0]; pa.[4]; pa.[1]; pa.[5]; pa.[2]; pa.[6]; pa.[3]; pa.[7];
                |]
        
        let attrs = [
                        (DefaultSemantic.Positions, pos :> Array)
                        //(DefaultSemantic.Colors, (C4b.Red |> Array.replicate (pos |> Array.length)) :> Array)
                    ] |> SymDict.ofList
            
        IndexedGeometry(
                    Mode = IndexedGeometryMode.LineList,
                    IndexedAttributes = attrs
                )

    let instancedGeometry (trafos : IMod<Trafo3d[]>)(colors : IMod<C4f[]>) (g : IndexedGeometry) =
        let vertexAttributes = 
            g.IndexedAttributes |> Seq.map (fun (KeyValue(k,v)) -> 
                let t = v.GetType().GetElementType()
                let view = BufferView(Mod.constant (ArrayBuffer(v) :> IBuffer), t)

                k, view
            ) |> Map.ofSeq

        let index, faceVertexCount =
            if g.IsIndexed then
                g.IndexArray, g.IndexArray.Length
            else
                null, g.IndexedAttributes.[DefaultSemantic.Positions].Length

        let call = trafos |> Mod.map (fun t ->
                DrawCallInfo(
                    FaceVertexCount = faceVertexCount,
                    FirstIndex = 0,
                    InstanceCount = t.Length,
                    FirstInstance = 0,
                    BaseVertex = 0
                )
            )

        let sg = Sg.VertexAttributeApplicator(vertexAttributes, Sg.RenderNode(call, Mod.constant g.Mode)) :> ISg
        
        let sg =
            if index <> null then
                Sg.VertexIndexApplicator(BufferView.ofArray  index, sg) :> ISg
            else
                sg
       
        let m44Trafos = trafos |> Mod.map (fun a -> a |> Array.map (fun (t : Trafo3d) -> (M44f.op_Explicit t.Forward)) :> Array)
        let m44View = BufferView(m44Trafos |> Mod.map (fun a -> ArrayBuffer a :> IBuffer), typeof<M44f>)

        let sg = Sg.InstanceAttributeApplicator([DefaultSemantic.InstanceTrafo, m44View] |> Map.ofList, sg) :> ISg

        //let colors = colors |> Mod.map (fun a -> a |> Array.map (fun (v : V4d) -> (V4f.op_Explicit v)) :> Array)
        let colors      = colors  |> Mod.map (fun a -> a :> Array)
        let colorView   = BufferView(colors |> Mod.map (fun a-> ArrayBuffer a :> IBuffer), typeof<C4f>)
        
        //let colorView = BufferView(colors |> Mod.map (fun a-> ArrayBuffer a :> IBuffer), typeof<V4f>)
        Sg.InstanceAttributeApplicator([Sym.ofString "InstanceColor", colorView] |> Map.ofList, sg) :> ISg

    let fittingPlane (points : V3d[]) = 

        let centroid = (points |> Array.fold ( fun x y -> x + y ) V3d.Zero) / (float)(points |> Array.length)

        // Calc full 3x3 covariance matrix, excluding symmetries:
        let mutable xx = 0.0; 
        let mutable xy = 0.0; 
        let mutable xz = 0.0;
        let mutable yy = 0.0; 
        let mutable yz = 0.0; 
        let mutable zz = 0.0;

        points |> Array.iter (fun point -> 
                                let p = point - centroid
                                xx <- xx + p.X * p.X
                                xy <- xy + p.X * p.Y
                                xz <- xz + p.X * p.Z
                                yy <- yy + p.Y * p.Y
                                yz <- yz + p.Y * p.Z
                                zz <- zz + p.Z * p.Z
                                )
        
        let det_X = yy * zz - yz * yz
        let det_Y = xx * zz - xz * xz
        let det_Z = xx * yy - xy * xy

        let det_max = max (max det_X det_Y) det_Z
//        let x = (det_max > 0.0) //"The points don't span a plane"

        let normal = 
            if (det_max = det_X) then
                let x = 1.0
                let y = (xz * yz - xy * zz) / det_X
                let z = (xy * yz - xz * yy) / det_X
                V3d(x, y, z).Normalized
            else if (det_max = det_Y) then
                let x = (yz * xz - xy * zz) / det_Y
                let y = 1.0
                let z = (xy * xz - yz * xx) / det_Y
                V3d(x , y, z).Normalized
            else
                let x = (yz * xy - xz * yy) / det_Z
                let y = (xz * xy - yz * xx) / det_Z
                let z = 1.0
                V3d(x,y,z).Normalized

        Plane3d(normal, centroid)
