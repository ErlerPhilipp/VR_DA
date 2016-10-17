namespace Aardvark.VR

open Aardvark.Base
open Aardvark.Base.Rendering

[<AutoOpen>]
module Instanced =
    open FShade
    open Aardvark.SceneGraph.Semantics
    type Vertex = { 
            [<Position>] pos : V4d 
            [<Color>] col : V4d
            [<TexCoord>] tc : V2d
            [<Semantic("ZZZInstanceTrafo")>] trafo : M44d
        }

    type Fragment = { 
            [<Color>] col : V4d
            [<TexCoord>] tc : V2d
            [<Normal>] n : V3d
        }
    let trafo (v : Vertex) =
        vertex {
            return { 
                pos = uniform.ViewProjTrafo * v.trafo * v.pos
                col = v.col
                trafo = v.trafo
                tc = v.tc
            }
        }
            
    let diffuse (v : Effects.Vertex) =
        fragment {
            return V4d(v.c.XYZ * v.n.Z, v.c.W)
        }

    let pointSprite (p : Point<Effects.Vertex>) =
        triangle {
                
            let s = uniform.PointSize
            let pos = p.Value.wp
            let r = uniform.ViewTrafoInv * V4d.IOOO |> Vec.xyz
            let u = uniform.ViewTrafoInv * V4d.OIOO |> Vec.xyz
            let pxyz = pos.XYZ

            let p00 = pxyz - s * r - s * u
            let p01 = pxyz - s * r + s * u
            let p10 = pxyz + s * r - s * u
            let p11 = pxyz + s * r + s * u

            yield { p.Value with pos = uniform.ViewProjTrafo * V4d(p00, 1.0); tc = V2d.OO; }
            yield { p.Value with pos = uniform.ViewProjTrafo * V4d(p10, 1.0); tc = V2d.IO; }
            yield { p.Value with pos = uniform.ViewProjTrafo * V4d(p01, 1.0); tc = V2d.OI; }
            yield { p.Value with pos = uniform.ViewProjTrafo * V4d(p11, 1.0); tc = V2d.II; }
        }

    let pointSpriteFragment (v : Effects.Vertex) =
        fragment {
            let c = 2.0 * v.tc - V2d.II
            if c.Length > 1.0 then
                discard()

            let z = sqrt (1.0 - c.LengthSquared)
            let n = V3d(c.XY,z)

               
            return { 
                col = v.c
                tc = v.tc
                n = n
            } 
        }

