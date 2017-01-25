namespace InteractiveSegmentation

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph.Semantics
open System.IO
open System.Threading

open Aardvark.Git
open Aardvark.Database
open Aardvark.Base.Native

open InteractiveSegmentation.Octree

module BoundingBox = 


    open FShade
    open Aardvark.SceneGraph.Semantics  

    type InstanceColor() = inherit SemanticAttribute("InstanceColor")

    type InstanceVertex = { 
            [<Position>] pos : V4d 
            [<Color>] col : V4d
            //[<InstanceColor>] col : V4d
            [<InstanceTrafo>] trafo : M44d
        }


    let instancedTrafo (v : InstanceVertex) =
        vertex {
            return {
                pos     = uniform.ViewProjTrafo * v.trafo * v.pos
                col     = v.col
                trafo   = v.trafo
            }
        }
    let vertexColor (v:InstanceVertex) = fragment {return v.col}

    

    let effectInstancedColor  = 
        let e = FShade.SequentialComposition.compose [ instancedTrafo |> toEffect; vertexColor |> toEffect]
        //let e   = FShade.SequentialComposition.compose [ instancedTrafo |> toEffect; DefaultSurfaces.thickLineRoundCaps C4f.Green |> toEffect ]
        let s   = FShadeSurface(e) :> ISurface 
        s |> Mod.constant
    

    let effectGreen  = 
        let e   = FShade.SequentialComposition.compose [ instancedTrafo |> toEffect; DefaultSurfaces.constantColor C4f.Green |> toEffect ]
        let s   = FShadeSurface(e) :> ISurface 
        s |> Mod.constant


    let effectRed  = 
        let e   = FShade.SequentialComposition.compose [ instancedTrafo |> toEffect; DefaultSurfaces.constantColor C4f.Red |> toEffect ]
        let s   = FShadeSurface(e) :> ISurface 
        s |> Mod.constant
    
    
    let trafoForUnitBox (box: Box3d) = 
        let shift       = Trafo3d.Translation (box.Center)
        let bias        = Trafo3d.Translation(-V3d.III * 0.5)
        let scale       = Trafo3d.Scale (box.SizeX, box.SizeY, box.SizeZ)
        bias * scale * shift     




