namespace Aardvark.VR

open FShade

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO

open LogicalScene

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
