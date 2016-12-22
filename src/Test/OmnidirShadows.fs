namespace Aardvark.VR

open FShade

open Aardvark.Application
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.GL
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics
 
open Types

// source: 
// Sammy the Salmon 
// Aardvark Shadows demo
// http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-16-shadow-mapping/
// http://ogldev.atspace.co.uk/www/tutorial43/tutorial43.html
module OmnidirShadows =
    open System
    open LogicalSceneTypes
    open GraphicsSceneTypes

    let init (vrWin : VrWindow.VrWindow, scene : Scene, graphicsScene : GraphicsScene) = 
        
        let toSg (t : GraphicsObject) =
            adaptive {
            let! model = t.mmodel
            return 
                match t.original.surface, model with
                    | Some surface, Some model ->
                        model
                            |> Sg.uniform "isHighlighted" t.mhasHighlight
                            |> Sg.uniform "scoredState" t.mscoredState
                            |> Sg.uniform "tilingFactor" t.mtilingFactor
                            |> Sg.trafo t.mtrafo
                            |> Sg.surface (Mod.constant surface)
                    | _ ->
                        Sg.ofList []
            } 
            |> Sg.dynamic
        
        let depthOnlySurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, ([DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.White |> toEffect])) :> ISurface
        let toShadowCasterSg (t : GraphicsObject) =
            adaptive {
                let! model = t.mmodel
                return 
                    match t.original.surface, model with
                        | Some surface, Some model when t.original.castsShadow ->
                            model
                                |> Sg.trafo t.mtrafo
                                |> Sg.surface (Mod.constant depthOnlySurface)
                        | _ ->
                            Sg.ofList []
            }
            |> Sg.dynamic
        
        let objectsInScene = 
            graphicsScene.graphicsObjects
                |> ASet.map toSg
                |> Sg.set
                |> Sg.blendMode(Mod.constant (BlendMode(false)))  

        let shadowCasterInScene = 
            graphicsScene.graphicsObjects
                |> ASet.map toShadowCasterSg
                |> Sg.set
                |> Sg.blendMode(Mod.constant (BlendMode(false)))

        let shadowMapSize = Mod.init (V2i(4096, 4096))
    
        let shadowCam = CameraView.lookAt (V3d.OIO * 4.0) V3d.Zero V3d.OOI
//        let shadowCam = CameraView.lookAt (V3d.III * 2.0) V3d.Zero V3d.OOI
        let shadowProj = Frustum.perspective 60.0 0.1 50.0 1.0

        let angle = Mod.constant 0.0
//        let lightViewTrafo = 
//            graphicsScene.lightViewTrafo
//                |> Mod.map (fun trafo -> 
//                                let lightPosTrafo = Trafo3d.Translation(trafo.Forward.TransformPos(V3d()))
////                                Trafo3d.RotationZ(90.0) * lightPosTrafo
//                                Trafo3d.RotationZ(90.0) * lightPosTrafo.Inverse
//                            )
        let lightViewTrafo =
            angle |> Mod.map (fun angle -> Trafo3d.RotationZ(angle) * (shadowCam |> CameraView.viewTrafo))
        let lightSpaceViewProjTrafo = lightViewTrafo |> Mod.map (fun view -> view * (shadowProj |> Frustum.projTrafo))
    
        let signature = 
            vrWin.Runtime.CreateFramebufferSignature [
                DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
            ] 

        let shadowDepth =
            shadowCasterInScene
                |> Sg.uniform "ViewportSize" (Mod.constant (V2i(1024,1024)))
                |> Sg.viewTrafo lightViewTrafo
                |> Sg.projTrafo (shadowProj |> Frustum.projTrafo |> Mod.constant)
                |> Sg.compile vrWin.Runtime signature   
                |> RenderTask.renderToDepth shadowMapSize
               
        let sg =
            objectsInScene
                |> Sg.uniform "SpecularExponent" (Mod.constant 32)
                |> Sg.uniform "AmbientFactor" (Mod.constant 0.03)
                |> Sg.uniform "LinearAttenuation" (Mod.constant 0.05)
                |> Sg.uniform "LightViewTrafo" graphicsScene.lightViewTrafo
                |> Sg.uniform "LightSpaceViewProjTrafo" lightSpaceViewProjTrafo
                |> Sg.texture (Symbol.Create "ShadowTexture") shadowDepth
        sg