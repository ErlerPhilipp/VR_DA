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
        
        

        let effectDict = Dict.empty
        let getOrCreateEffect(key) = 
            let value = Dict.tryFind key effectDict
            match value with
                | Some value -> value
                | None -> 
                    let newEffect = Sg.effect key
                    Dict.add(key) newEffect effectDict
                    newEffect
        
        let toSg (t : GraphicsObject) =
            if not t.effect.IsEmpty then
                let prep = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, (t.effect @ [OmnidirShadowShader.shadowShader |> toEffect]))
                t.mmodel
                    |> Sg.dynamic
                    |> Sg.uniform "isHighlighted" t.mhasHighlight
                    |> Sg.uniform "scoredState" t.mscoredState
                    |> Sg.uniform "tilingFactor" t.mtilingFactor
                    |> Sg.trafo t.mtrafo
                    |> Sg.surface (Mod.constant (prep :> ISurface))
//                    |> getOrCreateEffect t.effect
            else
                Sg.ofList []
        
        let toShadowCasterSg (t : GraphicsObject) =
            if t.original.castsShadow && not t.effect.IsEmpty then
                t.mmodel
                    |> Sg.dynamic
                    |> Sg.uniform "isHighlighted" t.mhasHighlight
                    |> Sg.uniform "scoredState" t.mscoredState
                    |> Sg.uniform "tilingFactor" t.mtilingFactor
                    |> Sg.trafo t.mtrafo
                    |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.White |> toEffect]
//                    |> getOrCreateEffect t.effect
            else
                Sg.ofList []
        
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
    
        let shadowCam = CameraView.lookAt (V3d.III * 2.0) V3d.Zero V3d.OOI
//        let shadowCam = CameraView.lookAt lightTrafo V3d.Zero V3d.OOI
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

//        let quadSg (color : C4b) = 
//                let index = [|0;1;2; 0;2;3|]
//                let positions = [|V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |] |> Array.map ((*)3.0f)
//
//                IndexedGeometry(IndexedGeometryMode.TriangleList, index, 
//                    SymDict.ofList [
//                        DefaultSemantic.Positions, positions :> Array
//                        DefaultSemantic.Colors,  Array.init positions.Length (constF color  ) :> Array
//                        DefaultSemantic.Normals, Array.init positions.Length (constF V3f.OOI) :> Array
//                    ], SymDict.empty) |> Sg.ofIndexedGeometry

//        let sceneSg (sg : ISg, fragmentShader : list<FShadeEffect>) =
//            quadSg C4b.Green 
//            |> Sg.effect ( (OmnidirShadowShader.trafo |> toEffect) :: fragmentShader )
//            |> Sg.andAlso ( sg )
//            |> Sg.uniform "LightSpaceViewProjTrafo" lightSpaceViewProjTrafo
//            |> Sg.trafo ( Trafo3d.Translation(V3d(0.0,0.0,0.003)) |> Mod.constant )

        let shadowDepth =
//            sceneSg (shadowCasters, [ DefaultSurfaces.vertexColor |> toEffect ])
            shadowCasterInScene
                |> Sg.uniform "ViewportSize" (Mod.constant (V2i(1024,1024)))
                |> Sg.uniform "LightSpaceViewProjTrafo" lightSpaceViewProjTrafo
                |> Sg.viewTrafo lightSpaceView
                |> Sg.projTrafo (shadowProj |> Frustum.projTrafo |> Mod.constant)
                |> Sg.compile vrWin.Runtime signature   
                |> RenderTask.renderToDepth shadowMapSize
               
        let sg =
//             sceneSg (shadowReceiver, [ OmnidirShadowShader.shadowShader |> toEffect; OmnidirShadowShader.lighting |> toEffect ])
            objectsInScene
                |> Sg.uniform "SpecularExponent" (Mod.constant 32)
                |> Sg.uniform "AmbientFactor" (Mod.constant 0.03)
                |> Sg.uniform "LinearAttenuation" (Mod.constant 0.05)
                |> Sg.uniform "LightTrafo" graphicsScene.lightTrafo
                |> Sg.uniform "LightSpaceViewProjTrafo" lightSpaceViewProjTrafo
                |> Sg.texture (Symbol.Create "ShadowTexture") shadowDepth
        sg