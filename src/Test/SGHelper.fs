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

module BoxSg = 
    let private calcTangentsFromVertices (indices : int[], texPos : V2f[], normals : V3f[], positions : V3f[]) =

        let indices = if isNull indices then Array.init positions.Length id else indices

        let tangents = Array.zeroCreate positions.Length
        let bitangents = Array.zeroCreate positions.Length

        for bi in 0 .. 3 .. indices.Length - 3 do
            let i0 = indices.[bi + 0]
            let i1 = indices.[bi + 1]
            let i2 = indices.[bi + 2]

            let v0 = positions.[i0]
            let v1 = positions.[i1]
            let v2 = positions.[i2]

            let uv0 = texPos.[i0]
            let uv1 = texPos.[i1]
            let uv2 = texPos.[i2]

            let deltaPos0 = v1 - v0
            let deltaPos1 = v2 - v0
                    
            let deltaUV0 = uv1 - uv0
            let deltaUV1 = uv2 - uv0

            let r = 1.0f / (deltaUV0.X * deltaUV1.Y - deltaUV0.Y * deltaUV1.X)
            let triTangent = (deltaPos0 * deltaUV1.Y - deltaPos1 * deltaUV0.Y) * r
            let triBiTangent = (deltaPos1 * deltaUV0.X - deltaPos0 * deltaUV1.X) * r

            tangents.[i0] <- tangents.[i0] + triTangent
            tangents.[i1] <- tangents.[i1] + triTangent
            tangents.[i2] <- tangents.[i2] + triTangent

            bitangents.[i0] <- bitangents.[i0] + triBiTangent
            bitangents.[i1] <- bitangents.[i1] + triBiTangent
            bitangents.[i2] <- bitangents.[i2] + triBiTangent

        for i in 0 .. tangents.Length-1 do
            let t = tangents.[i].Normalized
            let b = bitangents.[i].Normalized

            let n = normals.[i] |> Vec.normalize
            let t = t - (Vec.dot n t) * n |> Vec.normalize
            let b = b - (Vec.dot n b) * n - (Vec.dot t b) * t |> Vec.normalize

            tangents.[i] <- t
            bitangents.[i] <- b

        (tangents, bitangents)

    let private unitBox =
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

        let texcoords =
            [|
                V2f.OO; V2f.IO; V2f.II;  V2f.OO; V2f.II; V2f.OI
            |]

        let positions = indices |> Array.map (fun i -> positions.[i])
        let texcoords = indices |> Array.mapi (fun ti _ -> texcoords.[ti % 6])
        let normals = indices |> Array.mapi (fun ti _ -> normals.[ti / 6])

        let (tangents, bitangents) = calcTangentsFromVertices (null, texcoords, normals, positions)

        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,

            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, positions :> Array
                    DefaultSemantic.DiffuseColorCoordinates, texcoords :> Array
                    DefaultSemantic.Normals, normals :> Array
                    DefaultSemantic.DiffuseColorUTangents, tangents :> Array
                    DefaultSemantic.DiffuseColorVTangents, bitangents :> Array
                ]
        )
        
    let private unitBoxSG = Sg.ofIndexedGeometry unitBox
    
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
                    let offset = Trafo3d.Translation(0.0, float i*0.5, 0.0)
                    yield ({o with 
                                id = newId()
                                trafo = offset * o.trafo})
        ]

module Highlight =
    type UniformScope with
        member x.isHighlighted : bool = x?isHighlighted

    let highlight (v : Vertex) =
        fragment {
            if uniform.isHighlighted then
                return v.c * 3.0
            else
                return v.c
        }
    
    let Effect = 
        toEffect highlight

module TextureTiling = 

    type UniformScope with
        member x.tilingFactor : V2d = x?tilingFactor

    let internal textureTiling (v : Vertex) =
        vertex {
            let tilingFactor = uniform.tilingFactor
            return { v with
                        tc = v.tc * tilingFactor
                   }
        }
    
    let Effect = 
        toEffect textureTiling
       
module NormalMap =
 
    let private normalSampler =
        sampler2d {
            texture uniform?NormalMapTexture
            filter Filter.MinMagMipLinear
            addressU SamplerStateModule.WrapMode.Wrap
            addressV SamplerStateModule.WrapMode.Wrap
        }

    let internal normalMap (v : Vertex) =
        fragment {
            let texColor = normalSampler.Sample(v.tc).XYZ
            let texNormal = (2.0 * texColor - V3d.III) |> Vec.normalize

            let n = v.n.Normalized * texNormal.Z + v.b.Normalized * texNormal.X + v.t.Normalized * texNormal.Y |> Vec.normalize
            
            return { v with n = n }
        }

    let Effect = 
        toEffect normalMap

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
                
            let s = Vec.dot h n |> max 0.0

            let l = ambient + (1.0 - ambient) * diffuse
            
            return V4d((v.c.XYZ * l + pown s specularExponent) * attenuation, v.c.W)
            //return V4d(v.c.XYZ * l * attenuation, v.c.W)
            //return V4d(v.n * 0.5 + 0.5, 1.0)
        }

    let Effect (twoSided : bool)= 
        toEffect (lighting twoSided)

module SphereTexture =

    type Vertex =
        {
            [<Position>] pos : V4d
            [<TexCoord>] tc : V2d
            [<Semantic("VertexPosition")>] vertexPos : V3d
        }

    let vertex (v : Vertex) =
        vertex {
            return { v with vertexPos = v.pos.XYZ }
        }

    let fragment (v : Vertex) =
        fragment {
            let pos = v.vertexPos

            let s = (atan2 pos.Y pos.X + Constant.Pi) / Constant.PiTimesTwo
            let t = 0.5 + asin (pos.Z / pos.Length) / Constant.Pi

            return { v with tc = V2d(s,t) }
        }