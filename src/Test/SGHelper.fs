namespace Aardvark.VR

open FShade

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO

open System

open LogicalScene

module Primitives = 
    let unitBox =
        let box = Box3d.Unit
        let indices =
            [|
                1;2;6; 1;6;5
                2;3;7; 2;7;6
                4;5;6; 4;6;7
                3;0;4; 3;4;7
                0;1;5; 0;5;4
                0;3;2; 0;2;1
            |]

        let positions = 
            [|
                V3f(box.Min.X, box.Min.Y, box.Min.Z)
                V3f(box.Max.X, box.Min.Y, box.Min.Z)
                V3f(box.Max.X, box.Max.Y, box.Min.Z)
                V3f(box.Min.X, box.Max.Y, box.Min.Z)
                V3f(box.Min.X, box.Min.Y, box.Max.Z)
                V3f(box.Max.X, box.Min.Y, box.Max.Z)
                V3f(box.Max.X, box.Max.Y, box.Max.Z)
                V3f(box.Min.X, box.Max.Y, box.Max.Z)
            |]

        let normals = 
            [| 
                V3f.IOO;
                V3f.OIO;
                V3f.OOI;

                -V3f.IOO;
                -V3f.OIO;
                -V3f.OOI;
            |]

        let tangents = 
            [| 
                V3f.OIO;
                V3f.OOI;
                -V3f.IOO;

                -V3f.OIO;
                -V3f.OOI;
                V3f.IOO;
            |]

        let bitangents = 
            [| 
                V3f.OOI;
                -V3f.IOO;
                -V3f.OIO;

                -V3f.OOI;
                V3f.IOO;
                V3f.OIO;
            |]

        let texcoords =
            [|
                V2f.OO; V2f.IO; V2f.II;  V2f.OO; V2f.II; V2f.OI
            |]

        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,

            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, indices |> Array.map (fun i -> positions.[i]) :> Array
                    DefaultSemantic.Normals, indices |> Array.mapi (fun ti _ -> normals.[ti / 6]) :> Array
                    DefaultSemantic.DiffuseColorVTangents, indices |> Array.mapi (fun ti _ -> tangents.[ti / 6]) :> Array
                    DefaultSemantic.DiffuseColorUTangents, indices |> Array.mapi (fun ti _ -> bitangents.[ti / 6]) :> Array
                    DefaultSemantic.DiffuseColorCoordinates, indices |> Array.mapi (fun ti _ -> texcoords.[ti % 6]) :> Array
                ]
        )
        
    let unitBoxSG = Sg.ofIndexedGeometry Primitives.unitBox
    
    let box (color : IMod<C4b>) (bounds : IMod<Box3d>) =
        let trafo = bounds |> Mod.map (fun box -> Trafo3d.Scale(box.Size) * Trafo3d.Translation(box.Min))
        let color = color |> Mod.map (fun c -> c.ToC4f().ToV4f())
        unitBoxSG
            |> Sg.vertexBufferValue DefaultSemantic.Colors color
            |> Sg.trafo trafo

module SGHelper = 
    let rec triangles (trafo : Trafo3d) (m : IndexedGeometry) =
        let positions = m.IndexedAttributes.[DefaultSemantic.Positions] |> unbox<V3f[]>
        let index =
            if isNull m.IndexArray then Array.init positions.Length id
            else m.IndexArray |> unbox<int[]>

        match m.Mode with
            | IndexedGeometryMode.TriangleList ->
                Array.init (index.Length / 3) (fun ti ->
                    Triangle3d(
                        trafo.Forward.TransformPos (V3d positions.[index.[3*ti+0]]),
                        trafo.Forward.TransformPos (V3d positions.[index.[3*ti+1]]),
                        trafo.Forward.TransformPos (V3d positions.[index.[3*ti+2]])
                    )
                )
                
            | IndexedGeometryMode.TriangleStrip ->
                failwith ""

            | _ ->
                [||]

    let rec createShape (currentTrafo : Trafo3d) (n : Loader.Node) : Triangle3d[] =
        match n with
            | Loader.Empty -> 
                [||]
            | Loader.Trafo(t,c) -> 
                createShape (t * currentTrafo) c
            | Loader.Group(children) ->
                children |> List.toArray |> Array.collect (createShape currentTrafo)
            | Loader.Leaf n ->
                triangles currentTrafo n.geometry

            | Loader.Material(_,c) ->
                createShape currentTrafo c
            
    let replicate (objects : Object list, amount : int) = 
        [
            for (o) in objects do
                for i in 1..amount do
                    let offset = Trafo3d.Translation(0.0, float i*2.0, 0.0)
                    yield ({o with 
                                id = newId()
                                trafo = offset * o.trafo})
        ]
                    
    type UniformScope with
        member x.isHighlighted : bool = x?isHighlighted
        member x.tilingFactor : V2f = x?tilingFactor

    let highlight (v : Vertex) =
        fragment {
            if uniform.isHighlighted then
                return v.c + 0.25
            else
                return v.c
        }

module TextureTiling = 
    
    let internal textureTiling (v : Vertex) =
        vertex {
            let tilingFactor = uniform?tilingFactor
            return { v with
                        tc = v.tc * tilingFactor
                   }
        }
    
    let Effect = 
        toEffect textureTiling
        

module Lighting = 

    type UniformScope with
        member x.HasSpecularColorTexture : bool = x?HasSpecularColorTexture
        member x.SpecularExponent : int = x?SpecularExponent
        member x.AmbientFactor : float = x?AmbientFactor
        member x.LinearAttenuation : float = x?LinearAttenuation

    let internal lighting (twoSided : bool) (v : Vertex) =
        fragment {
            let n = v.n |> Vec.normalize
            let fragmentToLight = uniform.LightLocation - v.wp.XYZ
            let distToLight = fragmentToLight.Length
            let c = fragmentToLight |> Vec.normalize
            let l = c
            let h = c
            let specularExponent = uniform.SpecularExponent
            let attenuation = (1.0 - distToLight * uniform.LinearAttenuation) |> clamp 0.0 1.0

            let ambient = uniform.AmbientFactor
            let diffuse = 
                if twoSided then Vec.dot l n |> abs
                else Vec.dot l n |> max 0.0

            let s = Vec.dot h n |> abs

            let l = ambient + (1.0 - ambient) * diffuse * attenuation

            return V4d(v.c.XYZ * l + attenuation * pown s specularExponent, v.c.W)
        }

    let Effect (twoSided : bool)= 
        toEffect (lighting twoSided)