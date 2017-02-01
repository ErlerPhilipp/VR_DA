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
            
                let bounds = tree.GetValue().bounds

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

                                if f nn then
                                    children |> Array.iteri (fun i child -> traverse (level + 1) (cell.GetChild i) child.Value) 
                            | Leaf _ ->                             
//                            | Leaf points ->                             
                                let nn = { id = (n :> obj); level = level; bounds = bb; 
                                           inner = true; granularity = 1.0; render = true }
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
                    

                        let (p,n,c) = points    |> Array.map ( fun p -> V3f p.Position, V3f p.Normal, p.Color)
                                                |> Array.unzip3

                        let r = 
                            IndexedGeometry(
                                IndexedAttributes =
                                    SymDict.ofList [
                                        DefaultSemantic.Positions,  p :> Array
                                        DefaultSemantic.Normals,    n :> Array
                                        DefaultSemantic.Colors,     c :> Array
                                    ]
                            )

                        return Some r
                    }
            
                interface ILodData with
            
                    member x.BoundingBox = bounds
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

        let internal debugNormal (v : Vertex) =
            fragment {
                return V3d(abs v.n.X, abs v.n.Y, abs v.n.Z)
            }


        let viewSizedPointSprites (p : Point<Effects.Vertex>) =
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
                [<Depth>]
                depth : float
            }
            
        let sphereImposterGeometry (p : Point<Vertex>) =
            triangle {
                let s   = uniform.PointSize * 0.5
                let wp  = p.Value.wp
                let vp  = uniform.ViewTrafo * wp
            
                let vp00 = vp + V4d( -s, -s, 0.0, 0.0 )
                let vp01 = vp + V4d( -s,  s, 0.0, 0.0 )
                let vp10 = vp + V4d(  s, -s, 0.0, 0.0 )
                let vp11 = vp + V4d(  s,  s, 0.0, 0.0 )

                let wp00 = uniform.ViewTrafoInv * vp00 
                let wp01 = uniform.ViewTrafoInv * vp01 
                let wp10 = uniform.ViewTrafoInv * vp10 
                let wp11 = uniform.ViewTrafoInv * vp11 


                let p00 = uniform.ProjTrafo * vp00
                let p01 = uniform.ProjTrafo * vp01
                let p10 = uniform.ProjTrafo * vp10
                let p11 = uniform.ProjTrafo * vp11


                yield { p.Value with c = p.Value.c; wp = wp00; pos = p00 / p00.W; tc = V2d.OO }
                yield { p.Value with c = p.Value.c; wp = wp01; pos = p01 / p01.W; tc = V2d.IO }
                yield { p.Value with c = p.Value.c; wp = wp10; pos = p10 / p10.W; tc = V2d.OI }
                yield { p.Value with c = p.Value.c; wp = wp11; pos = p11 / p11.W; tc = V2d.II }
            }       


        let sphereImposterFragment (v : Vertex) =
           fragment {
                let c = 2.0 * v.tc - V2d.II
                if c.Length > 1.0 then
                    discard()

                let z = sqrt (1.0 - c.LengthSquared)
                
                //let n = V3d(c.XY,z)
                let d = z * uniform.PointSize * 0.5
                
                let vp = uniform.ViewTrafo * v.wp
                let vp = vp / vp.W
                let vp = V4d(vp.X, vp.Y, vp.Z + d, 1.0)

                let sp = uniform.ProjTrafo *vp
                let sp = sp / sp.W

                return {color = v.c; depth = sp.Z * 0.5 + 0.5} 
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
                        ]
                    boundingBoxSurface      = Some BoundingBox.effectRed
                }

            Sg.pointCloud' lodData pointCloudInfoSettings (LodProgress.Progress.empty)
            |> Sg.effect 
                [
                    DefaultSurfaces.trafo       |> toEffect
                    sphereImposterGeometry      |> toEffect
                    sphereImposterFragment      |> toEffect
                    DefaultSurfaces.vertexColor |> toEffect
                ]
            |> Sg.uniform "PointSize" lodSettings.PointSize  