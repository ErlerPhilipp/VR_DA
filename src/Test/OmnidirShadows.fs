﻿namespace Aardvark.VR

open FShade

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.GL
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

//[<AutoOpen>]
//module EffectStack = 
//    open Aardvark.Base.Ag
//
//    module Sg =
//        type ComposeEffects(child : IMod<ISg>) =
//            inherit Sg.AbstractApplicator(child)
//            member x.Child = child
//
//        type AttachEffects(child : IMod<ISg>, effects : list<FShadeEffect>) =
//            inherit Sg.AbstractApplicator(child)
//            member x.Effects : list<FShadeEffect> = effects
//            member x.Child  = child
//
//        let composeEffects (s : ISg) = ComposeEffects(Mod.constant s) :> ISg
//        let attachEffects (e : list<FShadeEffect>) (s : ISg) = AttachEffects(Mod.constant s, e)
//
//    type ISg with
//        member x.EffectStack : list<FShadeEffect> = x?EffectStack
//
//    module EffectStackSemantics =
//
//        [<Semantic>]
//        type ComposeEffectsSemantics() =
//            member x.Surface(sg : Sg.ComposeEffects) =
//                let e = FShade.SequentialComposition.compose sg.EffectStack
//                let s = Mod.constant (FShadeSurface(e) :> ISurface)
//                sg.Child?Surface <- s
//
//            member x.EffectStack(s : Sg.AttachEffects) =
//                s.Child?EffectStack <- s.EffectStack @ s.Effects 
//
//            member x.EffectStack(s : Root<ISg>) = 
//                s.Child?EffectStack <- List.empty<FShadeEffect>
//     
// source: 
// Sammy the Salmon (Aardvark demo)
// http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-16-shadow-mapping/
// http://ogldev.atspace.co.uk/www/tutorial43/tutorial43.html
module OmnidirShadowShader =
    open FShade

    type Vertex = {
        [<Position>]        pos     : V4d
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<BiNormal>]        b       : V3d
        [<Tangent>]         t       : V3d
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
    }

    type UniformScope with
        member x.LightViewMatrix : M44d = uniform?LightViewMatrix
        
    let private diffuseSampler =
        sampler2dShadow {
            texture uniform?DiffuseColorTexture
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }

    let clipPlane = V4d(1.0,1.0,1.0,0.0)

    type ClipVertex = {
        [<Position>]        pos     : V4d
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<BiNormal>]        b       : V3d
        [<Tangent>]         t       : V3d
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
        [<ClipDistance>] clipDistances : float[]
    }

    let trafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            let distance = Vec.dot wp clipPlane
            //let distance = 10.0
            return {
                pos = uniform.ViewProjTrafo * wp
                wp = wp
                n = (uniform.ViewTrafo * (V4d(v.n,0.0))).XYZ
                b = uniform.NormalMatrix * v.b
                t = uniform.NormalMatrix * v.t
                c = v.c
                tc = v.tc
                clipDistances = [| distance |]
            }
        }

    let shadowShader (v : Vertex) =
        fragment {
            let lightSpace = uniform.LightViewMatrix * v.wp
            let div = lightSpace.XYZ / lightSpace.W
            let tc = V3d(0.5, 0.5,0.5) + V3d(0.5, 0.5, 0.5) * div.XYZ
            let d = max 0.3 (diffuseSampler.Sample(tc.XY, tc.Z - 0.000017))
            return V4d(v.c.XYZ * d, v.c.W)
        }


    let lighting (v : Vertex) =
        fragment {
            let n = v.n |> Vec.normalize
            let c = uniform?lightLocation - v.wp.XYZ |> Vec.normalize

            let ambient = 0.2
            let diffuse = Vec.dot (uniform.ViewTrafo * V4d(c,0.0)).XYZ n |> max 0.0

            let l = ambient + (1.0 - ambient) * diffuse

            return V4d(v.c.XYZ * diffuse, v.c.W)
        }
           
    type InstanceVertex = { 
        [<Position>]      pos   : V4d 
        [<InstanceTrafo>] trafo : M44d
    }

    let instanceTrafo (v : InstanceVertex) =
        vertex {
            return { v with pos = v.trafo * v.pos }
        }

module OmnidirShadows =
    open System

    let init (vrWin : VrWindow.VrWindow, shadowCasters : ISg, shadowReceiver : ISg) = 

        let shadowMapSize = Mod.init (V2i(4096, 4096))
    
        let shadowCam = CameraView.lookAt (V3d.III * 2.0) V3d.Zero V3d.OOI
        let shadowProj = Frustum.perspective 60.0 0.1 10.0 1.0

        //let angle = Mod.init 0.0
        let rotation =
            controller {
                let! dt = differentiate Mod.time
                return fun f -> f + dt.TotalSeconds * 0.6
            }
  
        let angle = Mod.constant 0.0 //AFun.integrate rotation 0.0
        let lightSpaceView =
            angle |> Mod.map (fun angle -> Trafo3d.RotationZ(angle) * (shadowCam |> CameraView.viewTrafo))
        let lightSpaceViewProjTrafo = lightSpaceView |> Mod.map (fun view -> view * (shadowProj |> Frustum.projTrafo))
        let lightPos = lightSpaceView |> Mod.map (fun t -> t.GetViewPosition())
    
        let signature = 
            vrWin.Runtime.CreateFramebufferSignature [
                DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
            ] 

        let quadSg (color : C4b) = 
                let index = [|0;1;2; 0;2;3|]
                let positions = [|V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |] |> Array.map ((*)3.0f)

                IndexedGeometry(IndexedGeometryMode.TriangleList, index, 
                    SymDict.ofList [
                        DefaultSemantic.Positions, positions :> Array
                        DefaultSemantic.Colors,  Array.init positions.Length (constF color  ) :> Array
                        DefaultSemantic.Normals, Array.init positions.Length (constF V3f.OOI) :> Array
                    ], SymDict.empty) |> Sg.ofIndexedGeometry

        let sceneSg (sg : ISg, fragmentShader : list<FShadeEffect>) =
            quadSg C4b.Green 
            |> Sg.effect ( (OmnidirShadowShader.trafo |> toEffect) :: fragmentShader )
            |> Sg.andAlso ( sg )
            |> Sg.uniform "LightViewMatrix" lightSpaceViewProjTrafo
            |> Sg.trafo ( Trafo3d.Translation(V3d(0.0,0.0,0.003)) |> Mod.constant )

        let shadowDepth =
            sceneSg (shadowCasters, [ DefaultSurfaces.vertexColor |> toEffect ])
                |> Sg.uniform "ViewportSize" (Mod.constant (V2i(1024,1024)))
                |> Sg.viewTrafo lightSpaceView
                |> Sg.projTrafo (shadowProj |> Frustum.projTrafo |> Mod.constant)
                |> Sg.compile vrWin.Runtime signature   
                |> RenderTask.renderToDepth shadowMapSize
               
        let sg =
             sceneSg (shadowReceiver, [ OmnidirShadowShader.shadowShader |> toEffect; OmnidirShadowShader.lighting |> toEffect ])
                |> Sg.uniform "lightLocation" lightPos
                |> Sg.uniform "ViewportSize" vrWin.Sizes
                |> Sg.texture DefaultSemantic.DiffuseColorTexture shadowDepth

                |> Sg.andAlso (
                    Sg.cone' 32 C4b.Red 0.1 0.5 |> Sg.trafo (lightPos |> Mod.map Trafo3d.Translation)
                        |> Sg.effect [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.Red |> toEffect; DefaultSurfaces.simpleLighting |> toEffect ]
                    )
        sg