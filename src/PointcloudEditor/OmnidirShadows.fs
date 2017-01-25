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

    type CubeFaces = 
        | FacePosX = 0
        | FaceNegX = 1
        | FacePosY = 2
        | FaceNegY = 3
        | FacePosZ = 4
        | FaceNegZ = 5
    
    let CubeFaceMatrices =
        [|
            Trafo3d.FromOrthoNormalBasis(-V3d.OOI, -V3d.OIO, -V3d.IOO)
            Trafo3d.FromOrthoNormalBasis( V3d.OOI, -V3d.OIO,  V3d.IOO)
            Trafo3d.FromOrthoNormalBasis( V3d.IOO, -V3d.OOI, -V3d.OIO)
            Trafo3d.FromOrthoNormalBasis( V3d.IOO,  V3d.OOI,  V3d.OIO)
            Trafo3d.FromOrthoNormalBasis( V3d.IOO, -V3d.OIO, -V3d.OOI)
            Trafo3d.FromOrthoNormalBasis(-V3d.IOO, -V3d.OIO,  V3d.OOI)
        |]

    let init (vrWin : VrWindow.VrWindow, graphicsScene : GraphicsScene) = 
        
        let toSg (t : GraphicsObject) =
            adaptive {
            let! model = t.model
            return 
                match t.original.surface, model with
                    | Some surface, Some model ->
                        model
                            |> Sg.uniform "tilingFactor" t.tilingFactor
                            |> Sg.trafo t.trafo
                            |> Sg.surface (Mod.constant surface)
                            |> Sg.onOff t.visible
                    | _ ->
                        Sg.ofList []
            } 
            |> Sg.dynamic
        
        let depthOnlySurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, ([DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.White |> toEffect])) :> ISurface
        let toShadowCasterSg (t : GraphicsObject) =
            adaptive {
                let! model = t.model
                return 
                    match model with
                        | Some model when t.original.castsShadow ->
                            model
                                |> Sg.trafo t.trafo
                                |> Sg.surface (Mod.constant depthOnlySurface)
                                |> Sg.onOff t.visible
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

        let shadowMapEdgeSize = 1024
        let shadowMapSize = Mod.init (V2i(shadowMapEdgeSize, shadowMapEdgeSize))
    
        let lightViewTrafo(faceTrafo : Trafo3d) = 
            graphicsScene.lightPos
                |> Mod.map (fun trafo -> 
                                let lightPosTrafo = Trafo3d.Translation(trafo)
                                (faceTrafo * lightPosTrafo).Inverse
                            )
        let shadowProj = Frustum.perspective 90.0 0.1 50.0 1.0
        let lightViewProjTrafo(faceTrafo : Trafo3d) = lightViewTrafo(faceTrafo) |> Mod.map (fun view -> view * (shadowProj |> Frustum.projTrafo))
    
        let signature = 
            vrWin.Runtime.CreateFramebufferSignature [
                DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
            ] 

//        let tex = vrWin.Runtime.CreateTextureCube()
//        let fbo = vrWin.Runtime.Create
//        let renderToDepthCube (size : IMod<V2i>) (t : IRenderTask) =
//            t.Runtime.Value

        let shadowDepth (faceTrafo : Trafo3d) =
            shadowCasterInScene
                |> Sg.uniform "ViewportSize" (Mod.constant (V2i(1024,1024)))
                |> Sg.viewTrafo (lightViewTrafo (faceTrafo))
                |> Sg.projTrafo (shadowProj |> Frustum.projTrafo |> Mod.constant)
                |> Sg.compile vrWin.Runtime signature   
                |> RenderTask.renderToDepth shadowMapSize

        let sg =
            objectsInScene
                |> Sg.uniform "SpecularExponent" (Mod.constant 32)
                |> Sg.uniform "AmbientFactor" (Mod.constant 0.2)
                |> Sg.uniform "LinearAttenuation" (Mod.constant 0.05)
                |> Sg.uniform "ShadowMapSize" (Mod.constant shadowMapEdgeSize)
                |> Sg.uniform "LightPos" graphicsScene.lightPos
                |> Sg.uniform "LightColor" graphicsScene.lightColor
                |> Sg.uniform "LightSpaceViewProjTrafoPosX" (lightViewProjTrafo(CubeFaceMatrices.[CubeFaces.FacePosX |> int]))
                |> Sg.uniform "LightSpaceViewProjTrafoNegX" (lightViewProjTrafo(CubeFaceMatrices.[CubeFaces.FaceNegX |> int]))
                |> Sg.uniform "LightSpaceViewProjTrafoPosY" (lightViewProjTrafo(CubeFaceMatrices.[CubeFaces.FacePosY |> int]))
                |> Sg.uniform "LightSpaceViewProjTrafoNegY" (lightViewProjTrafo(CubeFaceMatrices.[CubeFaces.FaceNegY |> int]))
                |> Sg.uniform "LightSpaceViewProjTrafoPosZ" (lightViewProjTrafo(CubeFaceMatrices.[CubeFaces.FacePosZ |> int]))
                |> Sg.uniform "LightSpaceViewProjTrafoNegZ" (lightViewProjTrafo(CubeFaceMatrices.[CubeFaces.FaceNegZ |> int]))
                |> Sg.texture (Symbol.Create "ShadowTexturePosX") (shadowDepth(CubeFaceMatrices.[CubeFaces.FacePosX |> int]))
                |> Sg.texture (Symbol.Create "ShadowTextureNegX") (shadowDepth(CubeFaceMatrices.[CubeFaces.FaceNegX |> int]))
                |> Sg.texture (Symbol.Create "ShadowTexturePosY") (shadowDepth(CubeFaceMatrices.[CubeFaces.FacePosY |> int]))
                |> Sg.texture (Symbol.Create "ShadowTextureNegY") (shadowDepth(CubeFaceMatrices.[CubeFaces.FaceNegY |> int]))
                |> Sg.texture (Symbol.Create "ShadowTexturePosZ") (shadowDepth(CubeFaceMatrices.[CubeFaces.FacePosZ |> int]))
                |> Sg.texture (Symbol.Create "ShadowTextureNegZ") (shadowDepth(CubeFaceMatrices.[CubeFaces.FaceNegZ |> int]))
        sg