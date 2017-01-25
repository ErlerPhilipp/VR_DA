// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module VRGrabbingTest

open Valve.VR

open OpenTK

open Aardvark.Application.WinForms
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.VR

open LogicalSceneTypes

open Shapes
open Primitives
open Sphere
open SGHelper
open NormalMap
open TextureTiling
open Lighting
open Highlight
open LightRepresentation
open Audio

type CollisionGroups =
    | Static =          0b0000000000000001s
    | Ball =            0b0000000000000010s
    | HandTrigger =     0b0000000000000100s
    | HoopTrigger =     0b0000000000001000s
    | Avatar =          0b0000000000010000s
    | TeleportRaycast = 0b0000000000100000s

[<EntryPoint>]
let main _ =
    Ag.initialize()
    Aardvark.Init()
    Aardvark.SceneGraph.IO.Loader.Assimp.initialize()
    Aardvark.Rendering.GL.RuntimeConfig.SupressSparseBuffers <- true

    use app = new OpenGlApplication()
    let vrWin = VrWindow.VrWindow(app.Runtime, true)
    
    //#region Trafos / Architecture   
    let trackingAreaSize = 2.9
    let trackingAreaHeight = 5.2
    let wallThickness = 1.0

    let hoopScale = 2.0
    let hoopTrafoWithoutScale = Trafo3d.RotationYInDegrees(90.0) * Trafo3d.Translation(trackingAreaSize * 0.48, -1.8, 0.0)
    let scoreScale = 0.1
    let scoreTrafo = Trafo3d.Scale(scoreScale * hoopScale) * Trafo3d.RotationYInDegrees(180.0) * hoopTrafoWithoutScale * 
                        Trafo3d.Translation(V3d(-0.03, 1.71, -3.3 * scoreScale) * hoopScale)
    
    let wallLateralOffset = trackingAreaSize * 0.5 + wallThickness * 0.5
    let wallHorizontalOffset = trackingAreaHeight * 0.5 - wallThickness
    
    let pedestalHeight = 1.0
    let pedestalRadius = 0.005
    let pedestalHorizontalOffset = trackingAreaSize / 2.0 - 0.3
    let pedestalVerticalOffset = pedestalHeight / 2.0
    let pedestalPosition = V3d(+pedestalHorizontalOffset, pedestalVerticalOffset, +pedestalHorizontalOffset)
        
    let cushionHeight = pedestalRadius * 0.75
    let cushionSize = pedestalRadius * 1.5
    let cushionPosition = V3d(pedestalPosition.X, pedestalPosition.Y + pedestalHeight / 2.0 + cushionHeight / 2.0, pedestalPosition.Z)
        
//    let pointCloudRadius = 0.1213
//    let pointCloudResetPos = V3d(cushionPosition.X, cushionPosition.Y + pointCloudRadius, cushionPosition.Z)
    //#endregion
       
    //#region Effects and Surfaces   
    let defaultTrafoEffect = DefaultSurfaces.trafo |> toEffect
    let defaultSimpleLightingEffect = DefaultSurfaces.simpleLighting |> toEffect
    let defaultDiffuseTextureEffect = DefaultSurfaces.diffuseTexture |> toEffect

    let constColorEffect =      [
                                    defaultTrafoEffect
                                    LightRepresentation.Effect
                                    defaultSimpleLightingEffect
                                ]
    let constColorSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, constColorEffect) :> ISurface
    let diffuseEffect =         [
                                    defaultTrafoEffect
                                    defaultDiffuseTextureEffect
                                    OmnidirShadowShader.Effect false
                                ] 
    let diffuseSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, diffuseEffect) :> ISurface
    let normalDiffuseEffect =   [
                                    defaultTrafoEffect
                                    TextureTiling.Effect
                                    NormalMap.Effect
                                    defaultDiffuseTextureEffect
                                    OmnidirShadowShader.Effect false
                                ]
    let normalDiffuseSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, normalDiffuseEffect) :> ISurface
    //#endregion
    
    //#region Textures   
    let textureParam = { TextureParams.empty with wantMipMaps = true }
    let groundNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_normalmap_S.jpg", textureParam) :> ITexture))
    let ceilingNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_normalmap_S.jpg", textureParam) :> ITexture))
    let wallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_normalmap_S.jpg", textureParam) :> ITexture))
    let cushionNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Tufted Leather\TexturesCom_TuftedLeather_normalmap_S.png", textureParam) :> ITexture))
    let pedestalNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Marble Polished\TexturesCom_MarblePolishedWhite1_normalmap_S.png", textureParam) :> ITexture))

    let groundDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_albedo_S.jpg", textureParam) :> ITexture))
    let ceilingDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_albedo_S.jpg", textureParam) :> ITexture))
    let wallDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg", textureParam) :> ITexture))
    let cushionDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Tufted Leather\TexturesCom_TuftedLeather_albedo_S.png", textureParam) :> ITexture))
    let pedestalDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Marble Polished\TexturesCom_MarblePolishedWhite1_diffuse_S.png", textureParam) :> ITexture))
    //#endregion
    
    //#region SceneGraph
    let assimpFlagsSteamVR = 
        Assimp.PostProcessSteps.None
        
    let loadVR f = Loader.Assimp.Load(f,assimpFlagsSteamVR)
   
    let controllerSg = 
        let controllerBody = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\bodytri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        let controllerButton =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\buttontri.obj" |> loadVR|> Sg.AdapterNode :> ISg
        let controllerLGrip =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\lgriptri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        let controllerRGrip = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\rgriptri.obj" |> loadVR|> Sg.AdapterNode :> ISg
        let controllerSysButton = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\sysbuttontri.obj" |> loadVR |> Sg.AdapterNode :> ISg
        let controllerTrackpad =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\trackpadtri.obj" |> loadVR |> Sg.AdapterNode :> ISg
        let controllerTrigger =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\triggertri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        [ controllerBody; controllerButton; controllerLGrip; controllerRGrip; controllerSysButton; controllerTrackpad; controllerTrigger ]
            |> Sg.group :> ISg
            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\models\SteamVR\vr_controller_vive_1_5\onepointfive_texture.png", textureParam) :> ITexture))

//    let basestationSg = 
//            Loader.Assimp.Load(@"..\..\resources\models\SteamVR\lh_basestation_vive\basestationtri.obj", assimpFlagsSteamVR)
//            |> Sg.AdapterNode :> ISg
//            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\models\SteamVR\lh_basestation_vive\lh_basestation_vive.png", textureParam) :> ITexture))
            
    let lightSg = Sg.sphere 3 (Mod.constant C4b.White) (Mod.constant 0.1)

    let groundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
                            |> groundDiffuseTexture |> groundNormalMap
    let ceilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
                            |> ceilingDiffuseTexture |> ceilingNormalMap
    let wallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, trackingAreaHeight, wallThickness))))
                            |> wallDiffuseTexture |> wallNormalMap

    let pedestalSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(pedestalRadius, pedestalHeight, pedestalRadius))))
                            |> pedestalDiffuseTexture |> pedestalNormalMap
    let cushionSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(cushionSize, cushionHeight, cushionSize))))
                            |> cushionDiffuseTexture |> cushionNormalMap
    //#endregion

    //#region Objects   
    let controller1Object = 
        { defaultObject with
            id = newId()
            model = Some controllerSg
            surface = Some diffuseSurface
        }
    let controller2Object = { controller1Object with id = newId() }
//    let grabTrigger1 = 
//        { defaultObject with
//            id = newId()
//            castsShadow = false
//        }
//    let grabTrigger2 = { grabTrigger1 with id = newId() }
    let camObject1 = 
        { defaultObject with
            id = newId()
//            model = Some basestationSg
//            surface = Some diffuseSurface
        }
    let camObject2 = { camObject1 with id = newId() }
    let lightObject =
        { defaultObject with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Translation(0.0, trackingAreaHeight - wallThickness * 2.5, 0.0)
            model = Some lightSg
            surface = Some constColorSurface
        }

    let groundTilingFactor = 0.3
    let ceilingTilingFactor = 0.4
    let groundObject = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0)
            model = Some groundSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(groundTilingFactor * trackingAreaSize)
        }

    let ceilingObject = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Translation(0.0, trackingAreaHeight - 1.5 * wallThickness, 0.0)
            model = Some ceilingSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(ceilingTilingFactor * trackingAreaSize)
        }

    let wallBase = 
        { defaultObject with
            model = Some wallSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(0.25 * trackingAreaSize)
        }

    let wall1 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.Translation(0.0, wallHorizontalOffset, -wallLateralOffset)
        }
    let wall2 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (-90.0) * Trafo3d.Translation(wallLateralOffset, wallHorizontalOffset, 0.0)
        }
    let wall3 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (180.0) * Trafo3d.Translation(0.0, wallHorizontalOffset, wallLateralOffset)
        }
    let wall4 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (90.0) * Trafo3d.Translation(-wallLateralOffset, wallHorizontalOffset, 0.0)
        }
    
    let pedestalBase = 
        { defaultObject with
            model = Some pedestalSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(1.0, pedestalRadius / pedestalHeight)
        }
    let pedestal = { pedestalBase with id = newId(); trafo = Trafo3d.Translation(pedestalPosition)}

    let cushionBase =
        { defaultObject with
            model = Some cushionSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(0.25)
        }
    let cushion = { cushionBase with id = newId(); trafo = Trafo3d.Translation(cushionPosition)}

    let objects =
        [lightObject]
        @ [groundObject; ceilingObject; wall1; wall2; wall3; wall4]
        @ [pedestal; cushion]
        @ [controller1Object; controller2Object; camObject1; camObject2]
    //#endregion
        
    //#region Scene   
    let specialObjectIds =
        {
            cam1ObjectId        = camObject1.id
            cam2ObjectId        = camObject2.id
            controller1ObjectId = controller1Object.id
            controller2ObjectId = controller2Object.id
            lightId             = lightObject.id
        }

    let sceneObj =
        {
            objects             = PersistentHashSet.ofList objects
            viewTrafo           = Trafo3d.Identity
            trackingToWorld     = Trafo3d.Identity

            lightColor          = V3d(1.0, 1.0, 1.0)
            deltaTime           = 0.0
            scoreTrafo          = scoreTrafo
            scoreText           = "test"

            selectionVolume     = Shape.Sphere(0.25)
            
            specialObjectIds    = specialObjectIds
            interactionInfo1    = DefaultInteractionInfo
            interactionInfo2    = DefaultInteractionInfo
        }
    //#endregion

    let scene = GraphicsScene.createScene sceneObj vrWin

    let task = app.Runtime.CompileRender(vrWin.FramebufferSignature, scene)
    vrWin.RenderTask <- task
    
    vrWin.Run()
    
    Logging.endSession()

    OpenVR.Shutdown()

    0
