namespace Aardvark.VR

open FShade

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects

[<ReflectedDefinition>]
module OmnidirShadowShader =
    open FShade

    type UniformScope with
        member x.LightSpaceViewProjTrafoPosX : M44d = uniform?LightSpaceViewProjTrafoPosX
        member x.LightSpaceViewProjTrafoNegX : M44d = uniform?LightSpaceViewProjTrafoNegX
        member x.LightSpaceViewProjTrafoPosY : M44d = uniform?LightSpaceViewProjTrafoPosY
        member x.LightSpaceViewProjTrafoNegY : M44d = uniform?LightSpaceViewProjTrafoNegY
        member x.LightSpaceViewProjTrafoPosZ : M44d = uniform?LightSpaceViewProjTrafoPosZ
        member x.LightSpaceViewProjTrafoNegZ : M44d = uniform?LightSpaceViewProjTrafoNegZ
        member x.HasSpecularColorTexture : bool = x?HasSpecularColorTexture
        member x.SpecularExponent : int = x?SpecularExponent
        member x.AmbientFactor : float = x?AmbientFactor
        member x.LinearAttenuation : float = x?LinearAttenuation
        member x.LightPos : V3d = uniform?LightPos
        member x.ShadowMapSize : float = uniform?ShadowMapSize
        
    let shadowSamplerPosX =
        sampler2dShadow {
            texture uniform?ShadowTexturePosX
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }
    let shadowSamplerNegX =
        sampler2dShadow {
            texture uniform?ShadowTextureNegX
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }
    let shadowSamplerPosY =
        sampler2dShadow {
            texture uniform?ShadowTexturePosY
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }
    let shadowSamplerNegY =
        sampler2dShadow {
            texture uniform?ShadowTextureNegY
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }
    let shadowSamplerPosZ =
        sampler2dShadow {
            texture uniform?ShadowTexturePosZ
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }
    let shadowSamplerNegZ =
        sampler2dShadow {
            texture uniform?ShadowTextureNegZ
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }

    let samplerShadow =
        samplerCubeShadow {
            texture uniform?ShadowTexture
            filter Filter.MinMagLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }
        

    let getVectorComponent(v : V3d, compIndex : int) =
        if compIndex = 0 then v.X
        elif compIndex = 1 then v.Y
        elif compIndex = 2 then v.Z
        else 100.0 // should never happen
        
    let getMajorDim(vec : V3d) =
        if vec.X >= vec.Y then 
            if vec.X >= vec.Z then 0 else 2
        else 
            if vec.Y >= vec.Z then 1 else 2

    let sampleShadow (wp : V4d, sampleOffset : V3d, nDotl : float, distToLight : float) =       
        let samplePos = wp + V4d(sampleOffset, 0.0)
        let lightDir = samplePos.XYZ - uniform.LightPos
        let absLightDir = V3d(abs(lightDir.X), abs(lightDir.Y), abs(lightDir.Z))
            
        let majorDim = getMajorDim(absLightDir)

        let majorComponent = getVectorComponent(lightDir, majorDim)
        let positiveDir = majorComponent > 0.0

        let lightSpaceViewProjTrafo = 
            match majorDim with
                | 0 when positiveDir ->     uniform.LightSpaceViewProjTrafoPosX
                | 0 when not positiveDir -> uniform.LightSpaceViewProjTrafoNegX
                | 1 when positiveDir ->     uniform.LightSpaceViewProjTrafoPosY
                | 1 when not positiveDir -> uniform.LightSpaceViewProjTrafoNegY
                | 2 when positiveDir ->     uniform.LightSpaceViewProjTrafoPosZ
                | 2 when not positiveDir -> uniform.LightSpaceViewProjTrafoNegZ
                | _ -> uniform.LightSpaceViewProjTrafoPosX // M44d.Identity // should never happen
                
        let lightSpace = lightSpaceViewProjTrafo * samplePos
        let div = lightSpace.XYZ / lightSpace.W
        let tc = V3d(0.5, 0.5, 0.5) + V3d(0.5, 0.5, 0.5) * div.XYZ
            
        let constSlopeFactor = 0.001
        let a = sqrt (1.0 - nDotl * nDotl) // = sin(acos(n dot l))
        let slopeOffset = constSlopeFactor * a

        let constZOffsetFactor = 0.01
        let zOffset = constZOffsetFactor / uniform.ShadowMapSize
            
        let bias = (zOffset * 2.0 * distToLight + slopeOffset)
        let depthCompare = tc.Z - bias
                
        let shadowFactor = 
            match majorDim with
                | 0 when positiveDir ->    shadowSamplerPosX.Sample(tc.XY, depthCompare)
                | 0 when not positiveDir ->shadowSamplerNegX.Sample(tc.XY, depthCompare)
                | 1 when positiveDir ->    shadowSamplerPosY.Sample(tc.XY, depthCompare)
                | 1 when not positiveDir ->shadowSamplerNegY.Sample(tc.XY, depthCompare)
                | 2 when positiveDir ->    shadowSamplerPosZ.Sample(tc.XY, depthCompare)
                | 2 when not positiveDir ->shadowSamplerNegZ.Sample(tc.XY, depthCompare)
                | _ -> 100.0 // should never happen
        shadowFactor

    let internal shadowShader (twoSided : bool) (v : Vertex) =
        fragment {

//                (V3d(tc.XY, depthCompare), majorDim, positiveDir)

            
            // array of offset direction for sampling
            let gridSamplingDisk = 
                [|
                   V3d(1, 1, 1);    V3d(1, -1, 1);  V3d(-1, -1, 1);     V3d(-1, 1, 1); 
                   V3d(1, 1, -1);   V3d(1, -1, -1); V3d(-1, -1, -1);    V3d(-1, 1, -1);
                   V3d(1, 1, 0);    V3d(1, -1, 0);  V3d(-1, -1, 0);     V3d(-1, 1, 0);
                   V3d(1, 0, 1);    V3d(-1, 0, 1);  V3d(1, 0, -1);      V3d(-1, 0, -1);
                   V3d(0, 1, 1);    V3d(0, -1, 1);  V3d(0, -1, -1);     V3d(0, 1, -1);
                |]
//            let gridSamplingDisk = 
//                [|
//                   V2d(1, 1); V2d(1, -1); V2d(-1, -1); V2d(-1, 1); 
//                |]

            let n = v.n |> Vec.normalize
            let fragmentToLight = uniform.LightPos - v.wp.XYZ
            let distToLight = fragmentToLight.Length
            let c = fragmentToLight |> Vec.normalize
            let l = c
            let h = c
            let specularExponent = uniform.SpecularExponent
            let attenuation = (1.0 - distToLight * uniform.LinearAttenuation) |> clamp 0.0 1.0
            
            let ambient = uniform.AmbientFactor
            let nDotl = Vec.dot n l
            let diffuse = 
                if twoSided then nDotl |> abs
                else nDotl |> max 0.0
                
            let s = Vec.dot h n |> max 0.0

            let ambientPart =  ambient
            let diffusePart =  (1.0 - ambient) * diffuse * attenuation
            let specularPart = (pown s specularExponent) * attenuation
            
            // radius of PCF depending on distance from the light source
//            let diskRadiusDistanceFactor = 0.002
            let diskRadiusDistanceFactor = 0.0002
            let diskRadius = distToLight * diskRadiusDistanceFactor
            
            let mutable shadowSampleSum = 0.0
            // first 8 are bounding box. If all are equal, expect everything inside to be like them.
//            for i in 0..7 do
            let samplingOffset = V3d(0.0, 0.0, 0.0)
            for i in 0..3 do
                let samplingOffset = V3d(gridSamplingDisk.[i] * diskRadius)
                shadowSampleSum <- shadowSampleSum + sampleShadow(v.wp, samplingOffset, nDotl, distToLight)
//            let mutable numSamples = 8.0
            let mutable numSamples = 4.0
            
//            if shadowSampleSum = 0.0 || shadowSampleSum = 8.0 then
//                for i in 8..20 do
//                    let samplingOffset = V3d(gridSamplingDisk.[i] * diskRadius)
//                    shadowSampleSum <- shadowSampleSum + sampleShadow(samplingOffset, nDotl, distToLight)
//                numSamples <- 20.0

            let shadowFactor = shadowSampleSum / numSamples

            return V4d(v.c.XYZ * (ambientPart + (diffusePart + specularPart) * shadowFactor), v.c.W)
        }

    let Effect (twoSided : bool)= 
        toEffect (shadowShader twoSided)

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
        
module ControllerOverlayColor =
    type UniformScope with
        member x.overlayColor : V4d = x?overlayColor

    let controllerOverlayColor (v : Vertex) =
        fragment {
            return (1.0 - uniform.overlayColor.Z) * v.c + V4d(uniform.overlayColor.XYZ / uniform.overlayColor.Z, uniform.overlayColor.Z)
        }
    
    let Effect = 
        toEffect controllerOverlayColor

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
        member x.LightPos : V3d = uniform?LightPos

    let internal lighting (twoSided : bool) (v : Vertex) =
        fragment {
            let n = v.n |> Vec.normalize
            let fragmentToLight = uniform.LightPos - v.wp.XYZ
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