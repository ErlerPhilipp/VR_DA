namespace Aardvark.VR

open System

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Database

open InteractiveSegmentation

module PointCloudHelper =

    let flipYZ = Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, V3d.OIO)
    
    module PointSetStorage =
    
        module Lazy =
            let force ( x : Lazy<'a> ) : 'a =
                x.Value

    module Rendering =

        open PointSetStorage

         module LodData =
            open System.Collections.Concurrent
            
            type PointSetLodData(tree : IMod<Octree>, nodeCount : IMod<float>) =
            
                let bounds = tree |> Mod.map ( fun t -> t.bounds )

                member x.Traverse (f : LodDataNode -> bool) = 
             
                    let rec traverse (level : int) (cell : GridCell) (n : OctreeNode)  =
                        let bb = cell.BoundingBox
                        match n with
                            | Empty -> ()
                            | Node (_,children)  ->
//                            | Node (points,children)  ->
                                let nn = { id = (n :> obj); level = level; bounds = bb; 
                                           inner = true; granularity = Fun.Cbrt(bb.Volume / 5000.0); 
                                           render = true}
//                                printfn "traversed node"

                                if f nn then
                                    children |> Array.iteri (fun i child -> traverse (level + 1) (cell.GetChild i) child.Value) 
                            | Leaf _ ->                             
//                            | Leaf points ->                             
                                let nn = { id = (n :> obj); level = level; bounds = bb; 
                                           inner = true; granularity = 1.0; render = true }

//                                printfn "traversed leaf"
                                f nn |> ignore
                            
               
                    let tree = Mod.force tree
                    traverse 0 tree.cell tree.root.Value

                member x.BoundingBox = bounds

                member x.GetData (n : LodDataNode) : Async<Option<IndexedGeometry>> =
                    async {
                        let node = n.id |> unbox<OctreeNode>

                        let points =
                            match node with
                                | Node (points,_) | Leaf points  -> points.Value
                                | Empty -> [||]
                    
                    
                        let p = points    |> Array.map ( fun p -> V3f p.Position)
                        let n = points    |> Array.map ( fun p -> V3f p.Normal)
                        let c = points    |> Array.map ( fun p -> p.Color)
                        let s = points    |> Array.map ( fun p -> p.State |> int)

                        let r = 
                            IndexedGeometry(
                                IndexedAttributes =
                                    SymDict.ofList [
                                        DefaultSemantic.Positions,  p :> Array
                                        DefaultSemantic.Normals,    n :> Array
                                        DefaultSemantic.Colors,     c :> Array
                                        Sym.ofString "State",       s :> Array
                                    ]
                            )

                        return Some r
                    }
            
                interface ILodData with
            
                    member x.BoundingBox = bounds |> Mod.force
                    member x.Traverse f = x.Traverse f
                    member x.GetData n = x.GetData n
                    member x.Dependencies = [ tree :> IMod ]

        module Logic =
            type LodUserSettings =
                {
                    NodeCount           : IModRef<float>
                    PointSize           : IModRef<float>
                }


        open Logic

        let lodSettings =
            {   
                NodeCount = Mod.init 150.0
                PointSize = Mod.init 0.01
            }

        open LodData
        open FShade
        open Aardvark.Base.Rendering.Effects

        type StateAttribute() = inherit SemanticAttribute("State")

        type PointCloudVertex = {
            [<Position>]        pos     : V4d
            [<WorldPosition>]   wp      : V4d
            [<Normal>]          n       : V3d
            [<BiNormal>]        b       : V3d
            [<Tangent>]         t       : V3d
            [<Color>]           c       : V4d
            [<TexCoord>]        tc      : V2d
            [<State>]           s       : int
        }

        let internal debugNormal (v : PointCloudVertex) =
            fragment {
                return V3d(abs v.n.X, abs v.n.Y, abs v.n.Z)
            }


        let viewSizedPointSprites (p : Point<PointCloudVertex>) =
            triangle {
                let s = uniform.PointSize * 0.5
                let vp = uniform.ViewTrafo * p.Value.wp
                let p00 = V3d(vp.XYZ + V3d( -s, -s, 0.0 ))
                let p01 = V3d(vp.XYZ + V3d( -s,  s, 0.0 ))
                let p10 = V3d(vp.XYZ + V3d(  s, -s, 0.0 ))
                let p11 = V3d(vp.XYZ + V3d(  s,  s, 0.0 ))

                yield { p.Value with pos = uniform.ProjTrafo * V4d(p00, vp.W); tc = V2d.OO }
                yield { p.Value with pos = uniform.ProjTrafo * V4d(p10, vp.W); tc = V2d.IO }
                yield { p.Value with pos = uniform.ProjTrafo * V4d(p01, vp.W); tc = V2d.OI }
                yield { p.Value with pos = uniform.ProjTrafo * V4d(p11, vp.W); tc = V2d.II }

            }

        type Frag =
            {
                [<Color>]
                color : V4d
            }
            
        [<ReflectedDefinition>]
        let emptyArray =
            [|
                for _ in 0..7 do
                    yield V4d.OOOO
            |]

        let sphereImposterGeometry (p : Point<PointCloudVertex>) =
            triangle {
                let sqrt2Half = 0.7071067812
                let tcPiQuarter = 0.8535533906
                let oneMinusTcPiQuarter = 0.1464466094

                let s   = uniform.PointSize * 0.5
                let wp  = p.Value.wp
                let vp  = uniform.ViewTrafo * wp
            
                // prevent culling mess at near plane
                let ndcp = uniform.ProjTrafo * vp
                let ndcp = ndcp / ndcp.W
                let ndcs = uniform.ProjTrafo * uniform.ViewTrafo * V4d(0.0, 0.0, s, 0.0)
                let newZ = if (ndcp + ndcs).Z < -1.0 then vp.Z else vp.Z + s

                let vpCenter = V4d(vp.X, vp.Y, newZ, vp.W)
                let wpCenter = uniform.ViewTrafoInv * vpCenter
                let posCenter = uniform.ProjTrafo * vpCenter

                let vp = [|vp + V4d(  s,              0.0,           0.0, 0.0 )
                           vp + V4d(  s * sqrt2Half,  s * sqrt2Half, 0.0, 0.0 )
                           vp + V4d(  0.0,            s,             0.0, 0.0 )
                           vp + V4d( -s * sqrt2Half,  s * sqrt2Half, 0.0, 0.0 )
                           vp + V4d( -s,              0.0,           0.0, 0.0 )
                           vp + V4d( -s * sqrt2Half, -s * sqrt2Half, 0.0, 0.0 )
                           vp + V4d(  0.0,           -s,             0.0, 0.0 )
                           vp + V4d(  s * sqrt2Half, -s * sqrt2Half, 0.0, 0.0 )|]

                let tc = [|V2d( 1.0,                    0.5                 )
                           V2d( tcPiQuarter,            tcPiQuarter         )
                           V2d( 0.5,                    1.0                 )
                           V2d( oneMinusTcPiQuarter,    tcPiQuarter         )
                           V2d( 0.0,                    0.5                 )
                           V2d( oneMinusTcPiQuarter,    oneMinusTcPiQuarter )
                           V2d( 0.5,                    0.0                 )
                           V2d( tcPiQuarter,            oneMinusTcPiQuarter )|]
                           
                let wp =  emptyArray 
                let pos = emptyArray 
                for i in 0..7 do
                    wp.[i] <- uniform.ViewTrafoInv * vp.[i]
                    pos.[i] <- uniform.ProjTrafo * vp.[i]
                
                yield { p.Value with c = p.Value.c; wp = wp.[7];    pos = pos.[7] / pos.[7].W;  tc = tc.[7] }
                for i in 0..7 do
                    yield { p.Value with c = p.Value.c; wp = wpCenter;      pos = posCenter / posCenter.W;      tc = V2d(0.5, 0.5) }
                    yield { p.Value with c = p.Value.c; wp = wp.[i];        pos = pos.[i] / pos.[i].W;          tc = tc.[i] }
            }       


        let sphereImposterFragment (v : PointCloudVertex) =
           fragment {
                let newColor = if v.s = 0 then v.c else V4d(1.0, 0.0, 0.0, 0.5)
                return {color = newColor}
            }
    
    
        let vertexColor (f : Frag) =
            fragment {
                return f.color
            }

        let mkSg (customView : IMod<Trafo3d>) (lodData : PointSetLodData) = 
        
            let pointCloudInfoSettings =
                {
                    targetPointDistance     = Mod.init 1500.0
                    maxReuseRatio           = 0.5
                    minReuseCount           = 1L <<< 20
                    pruneInterval           = 500
                    customView              = Some customView
                    customProjection        = None
                    attributeTypes =
                        Map.ofList [
                            DefaultSemantic.Positions, typeof<V3f>
                            DefaultSemantic.Colors, typeof<C4b>
                            DefaultSemantic.Normals, typeof<V3f>
                            Sym.ofString "State", typeof<int>
                        ]
                    boundingBoxSurface      = Some BoundingBox.effectRed
                }

            Sg.pointCloud' lodData pointCloudInfoSettings (LodProgress.Progress.empty)
            |> Sg.effect 
                [
                    DefaultSurfaces.trafo       |> toEffect
                    sphereImposterGeometry      |> toEffect
                    sphereImposterFragment      |> toEffect
                ]
            |> Sg.uniform "PointSize" lodSettings.PointSize  