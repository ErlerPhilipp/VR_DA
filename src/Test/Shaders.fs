namespace Aardvark.VR

open FShade

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects

module OmnidirShadowShader =
    open FShade

    type UniformScope with
        member x.LightSpaceViewProjTrafo : M44d = uniform?LightSpaceViewProjTrafo
        member x.LightViewTrafo : M44d = uniform?LightViewTrafo
        
    let private shadowSampler =
        sampler2dShadow {
            texture uniform?ShadowTexture
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }

    let trafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            return {
                pos = uniform.ViewProjTrafo * wp
                wp = wp
                n = (uniform.ViewTrafo * (V4d(v.n,0.0))).XYZ
                b = uniform.NormalMatrix * v.b
                t = uniform.NormalMatrix * v.t
                c = v.c
                tc = v.tc
            }
        }

    let shadowShader (v : Vertex) =
        fragment {
            let lightSpace = uniform.LightSpaceViewProjTrafo * v.wp
            let div = lightSpace.XYZ / lightSpace.W
            let tc = V3d(0.5, 0.5,0.5) + V3d(0.5, 0.5, 0.5) * div.XYZ
            let shadowValue = shadowSampler.Sample(tc.XY, tc.Z - 0.000017)
            let d = max 0.3 shadowValue
//            return V4d(1.0, d, 0.0, v.c.W * 0.5)
            return V4d(v.c.XYZ * d, v.c.W)
        }

module Highlight =
    type UniformScope with
        member x.isHighlighted : bool = x?isHighlighted
        member x.scoredState : int = x?scoredState

    let highlight (v : Vertex) =
        fragment {
            let mutable newColor = v.c
            if uniform.isHighlighted then
                newColor <- newColor + V4d(0.3, 0.3, 0.1, 0.0)
                
            if uniform.scoredState = 0 then // nothing hit
                ()
            elif uniform.scoredState = 1 then // upper trigger
                newColor <- newColor + V4d(0.5, 0.1, 0.1, 0.0)
            elif uniform.scoredState = 2 then // lower trigger
                newColor <- newColor + V4d(0.1, 0.1, 0.5, 0.0)
            elif uniform.scoredState = 3 then // score
                newColor <- newColor + V4d(0.1, 0.5, 0.1, 0.0)

            return newColor
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
        member x.LightViewTrafo : M44d = uniform?LightViewTrafo

    let internal lighting (twoSided : bool) (v : Vertex) =
        fragment {
            let n = v.n |> Vec.normalize
            let lightPos = (uniform.LightViewTrafo * V4d(0.0, 0.0, 0.0, 1.0)).XYZ
            let fragmentToLight = lightPos - v.wp.XYZ
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