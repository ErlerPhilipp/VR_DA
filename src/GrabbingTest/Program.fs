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

open BulletHelper
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
let main argv =
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
    let hoopTrafo = Trafo3d.Scale hoopScale * hoopTrafoWithoutScale
    let scoreScale = 0.1
    let scoreTrafo = Trafo3d.Scale(scoreScale * hoopScale) * Trafo3d.RotationYInDegrees(180.0) * hoopTrafoWithoutScale * 
                        Trafo3d.Translation(V3d(-0.03, 1.71, -3.3 * scoreScale) * hoopScale)
    let hoopTriggerTrafo = hoopTrafoWithoutScale * Trafo3d.Translation(-0.23 * hoopScale, 1.44 * hoopScale, 0.0)
    let targetBallTrafo = hoopTriggerTrafo * Trafo3d.Translation(0.35, 0.0, 0.0)
    
    let wallLateralOffset = trackingAreaSize * 0.5 + wallThickness * 0.5
    let wallHorizontalOffset = trackingAreaHeight * 0.5 - wallThickness
    
    let pedestalHeight = 1.0
    let pedestalRadius = 0.005
    let pedestalHorizontalOffset = trackingAreaSize / 2.0 - 0.3
    let pedestalVerticalOffset = pedestalHeight / 2.0
    let pedestal1Position = V3d(+pedestalHorizontalOffset, pedestalVerticalOffset, +pedestalHorizontalOffset)
    let pedestal2Position = V3d(-pedestalHorizontalOffset, pedestalVerticalOffset, +pedestalHorizontalOffset)
    let pedestal3Position = V3d(+pedestalHorizontalOffset, pedestalVerticalOffset, -pedestalHorizontalOffset)
    let pedestal4Position = V3d(-pedestalHorizontalOffset, pedestalVerticalOffset, -pedestalHorizontalOffset)
        
    let cushionHeight = pedestalRadius * 0.75
    let cushionSize = pedestalRadius * 1.5
    let cushion1Position = V3d(pedestal1Position.X, pedestal1Position.Y + pedestalHeight / 2.0 + cushionHeight / 2.0, pedestal1Position.Z)
    let cushion2Position = V3d(pedestal2Position.X, pedestal2Position.Y + pedestalHeight / 2.0 + cushionHeight / 2.0, pedestal2Position.Z)
    let cushion3Position = V3d(pedestal3Position.X, pedestal3Position.Y + pedestalHeight / 2.0 + cushionHeight / 2.0, pedestal3Position.Z)
    let cushion4Position = V3d(pedestal4Position.X, pedestal4Position.Y + pedestalHeight / 2.0 + cushionHeight / 2.0, pedestal4Position.Z)
        
    let ballRadius = 0.1213
    let ball1ResetPos = V3d(cushion1Position.X, cushion1Position.Y + ballRadius, cushion1Position.Z)
    let ball2ResetPos = V3d(cushion2Position.X, cushion2Position.Y + ballRadius, cushion2Position.Z)
    let ball3ResetPos = V3d(cushion3Position.X, cushion3Position.Y + ballRadius, cushion3Position.Z)
    let ball4ResetPos = V3d(cushion4Position.X, cushion4Position.Y + ballRadius, cushion4Position.Z)
    //#endregion
        
    //#region Audio   
    let bounceBuffer = Audio.Sound.bufferFromFile(@"..\..\resources\sound\ball-bouncing.wav")
    let bounceSound = [ for i in 1 .. 10 -> Audio.Sound.sourceFromBuffer(bounceBuffer) ]
    
    let sireneBuffer = Audio.Sound.bufferFromFile(@"..\..\resources\sound\170825__santino-c__sirene-horn.wav")
    let sireneSound = Audio.Sound.sourceFromBuffer(sireneBuffer)
    sireneSound.Volume <- 2.5
    sireneSound.RolloffFactor <- 0.25
    sireneSound.Location <- hoopTriggerTrafo.Forward.TransformPos(V3d())

    let popBuffer = Audio.Sound.bufferFromFile(@"..\..\resources\sound\222373__qubodup__balloon-pop.wav")
    let popSound = Audio.Sound.sourceFromBuffer(popBuffer)
    popSound.Volume <- 0.5

    let ambient1Buffer = Audio.Sound.bufferFromFile(@"..\..\resources\sound\245187__patricklieberkind__dark-ambience-2.wav")
    let ambient1Sound = Audio.Sound.sourceFromBuffer(ambient1Buffer)
    ambient1Sound.Volume <- 0.02
    ambient1Sound.Location <- V3d(15.0, 0.0, 15.0)
    ambient1Sound.Loop <- true
    ambient1Sound.Play()

    let ambient2Buffer = Audio.Sound.bufferFromFile(@"..\..\resources\sound\371277__goulven__dark-ambient-loop.wav")
    let ambient2Sound = Audio.Sound.sourceFromBuffer(ambient1Buffer)
    ambient2Sound.Volume <- 0.02
    ambient2Sound.Location <- V3d(-25.0, 0.0, -5.0)
    ambient2Sound.Loop <- true
    ambient2Sound.Play()
    //#endregion
    
    //#region CollisionGroups   
    let staticCollidesWith = CollisionGroups.Ball ||| CollisionGroups.HandTrigger ||| CollisionGroups.Avatar ||| CollisionGroups.TeleportRaycast |> int16
    let ballCollidesWith =  CollisionGroups.Static ||| CollisionGroups.Ball ||| CollisionGroups.HandTrigger ||| CollisionGroups.HoopTrigger ||| CollisionGroups.Avatar |> int16
    let handTriggerCollidesWith =  CollisionGroups.Static ||| CollisionGroups.Ball |> int16
    let hoopTriggerCollidesWith =  CollisionGroups.Ball |> int16
    let avatarCollidesWith =  CollisionGroups.Static ||| CollisionGroups.Ball ||| CollisionGroups.Avatar |> int16
    let teleportRaycastCollidesWith =  CollisionGroups.Static |> int16
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
    let rayCastHitEffect =      [
                                    defaultTrafoEffect
                                    DefaultSurfaces.constantColor (C4f(0.3f, 0.3f, 0.9f, 0.3f)) |> toEffect
                                    defaultSimpleLightingEffect
                                ]
    let rayCastHitSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, rayCastHitEffect) :> ISurface
    let beamEffect =            [
                                    defaultTrafoEffect
                                    DefaultSurfaces.vertexColor |> toEffect
                                    DefaultSurfaces.thickLine |> toEffect
                                ]
    let beamSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, beamEffect) :> ISurface
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
    let boxEffect =             [
                                    defaultTrafoEffect
                                    TextureTiling.Effect
                                    NormalMap.Effect
                                    defaultDiffuseTextureEffect
                                    OmnidirShadowShader.Effect false
                                    Highlight.Effect
                                ]
    let boxSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, boxEffect) :> ISurface
    let ballEffect =            [
                                    SphereTexture.vertex |> toEffect
                                    defaultTrafoEffect
                                    SphereTexture.fragment |> toEffect
                                    defaultDiffuseTextureEffect
                                    OmnidirShadowShader.Effect false
                                    Highlight.Effect
                                ]
    let ballSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, ballEffect) :> ISurface
    //#endregion
    
    //#region Textures   
    let textureParam = { TextureParams.empty with wantMipMaps = true }
    let groundNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_normalmap_S.jpg", textureParam) :> ITexture))
    let ceilingNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_normalmap_S.jpg", textureParam) :> ITexture))
    let wallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_normalmap_S.jpg", textureParam) :> ITexture))
    let goalRoomWallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Brown Bricks\TexturesCom_Brown Bricks_normalmap_S.jpg", textureParam) :> ITexture))
    let cushionNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Tufted Leather\TexturesCom_TuftedLeather_normalmap_S.png", textureParam) :> ITexture))
    let pedestalNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Marble Polished\TexturesCom_MarblePolishedWhite1_normalmap_S.png", textureParam) :> ITexture))

    let groundDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_albedo_S.jpg", textureParam) :> ITexture))
    let ceilingDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_albedo_S.jpg", textureParam) :> ITexture))
    let wallDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg", textureParam) :> ITexture))
    let goalRoomWallDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Brown Bricks\TexturesCom_Brown Bricks_albedo_S.jpg", textureParam) :> ITexture))
    let cushionDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Tufted Leather\TexturesCom_TuftedLeather_albedo_S.png", textureParam) :> ITexture))
    let pedestalDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Marble Polished\TexturesCom_MarblePolishedWhite1_diffuse_S.png", textureParam) :> ITexture))
    
    let basketballDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\basketball\balldimpled.png", textureParam) :> ITexture))
    let beachballDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\balls\BeachBallColor.jpg", textureParam) :> ITexture))
    let softballDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\balls\SoftballColor.jpg", textureParam) :> ITexture))
    let tennisballDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\balls\TennisBallColorMap.jpg", textureParam) :> ITexture))
    //#endregion
    
    //#region SceneGraph   
    let staticModels =
        [
            @"..\..\resources\models\basketball\hoop.obj", hoopTrafo, 0.0f, None, 0.5f
        ]

    let assimpFlagsSteamVR = 
        Assimp.PostProcessSteps.None
        
    let assimpFlags = 
        Assimp.PostProcessSteps.CalculateTangentSpace |||
        Assimp.PostProcessSteps.GenerateSmoothNormals |||
        //Assimp.PostProcessSteps.FixInFacingNormals ||| 
        //Assimp.PostProcessSteps.JoinIdenticalVertices |||
//        Assimp.PostProcessSteps.FindDegenerates |||
        //Assimp.PostProcessSteps.FlipUVs |||
        //Assimp.PostProcessSteps.FlipWindingOrder |||
        Assimp.PostProcessSteps.MakeLeftHanded ||| 
        Assimp.PostProcessSteps.Triangulate

    let loadVR f = Loader.Assimp.Load(f,assimpFlagsSteamVR)
   
    let handBoxEdgeLength = 0.1
    let handBox = Box3d.FromCenterAndSize(V3d.OOO, handBoxEdgeLength * V3d.III)
    let handSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant handBox)
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

    let basestationSg = 
            Loader.Assimp.Load(@"..\..\resources\models\SteamVR\lh_basestation_vive\basestationtri.obj", assimpFlagsSteamVR)
            |> Sg.AdapterNode :> ISg
            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\models\SteamVR\lh_basestation_vive\lh_basestation_vive.png", textureParam) :> ITexture))
            
    let beamSg = Sg.lines (Mod.constant C4b.Red) (Mod.constant ( [| Line3d(V3d.OOO, -V3d.OOI * 100.0) |]) ) 
                    |> Sg.effect beamEffect
    let basketballSg = Sg.sphere 6 (Mod.constant C4b.DarkYellow) (Mod.constant ballRadius) |> basketballDiffuseTexture
    let beachballSg = Sg.sphere 6 (Mod.constant C4b.DarkYellow) (Mod.constant ballRadius) |> beachballDiffuseTexture
    let softballSg = Sg.sphere 6 (Mod.constant C4b.DarkYellow) (Mod.constant ballRadius) |> softballDiffuseTexture
    let tennisballSg = Sg.sphere 6 (Mod.constant C4b.DarkYellow) (Mod.constant ballRadius) |> tennisballDiffuseTexture
    let lightSg = Sg.sphere 3 (Mod.constant C4b.White) (Mod.constant 0.1)
    let rayCastAreaSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, 0.1, trackingAreaSize))))
    let rayCastPointSg = Sg.sphere 4 (Mod.constant C4b.Green) (Mod.constant 0.08)
    let rayCastCamSg = Sg.cone 4 (Mod.constant (C4b(50, 250, 50, 80))) (Mod.constant 0.1) (Mod.constant 0.4)

    let groundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
                            |> groundDiffuseTexture |> groundNormalMap
    let ceilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
                            |> ceilingDiffuseTexture |> ceilingNormalMap
    let wallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, trackingAreaHeight, wallThickness))))
                            |> wallDiffuseTexture |> wallNormalMap

//    let pedestalSg = Sg.cylinder 4 (Mod.constant C4b.Gray) (Mod.constant pedestalRadius) (Mod.constant pedestalHeight)
    let pedestalSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(pedestalRadius, pedestalHeight, pedestalRadius))))
                            |> pedestalDiffuseTexture |> pedestalNormalMap
    let cushionSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(cushionSize, cushionHeight, cushionSize))))
                            |> cushionDiffuseTexture |> cushionNormalMap

    let camBox = Box3d.FromCenterAndSize(V3d.OOO, 0.15 * V3d.III)

    let objectBoxEdgeLength = 0.25
    let objectBox = Box3d.FromCenterAndSize(V3d.OOO, objectBoxEdgeLength * V3d.III)
    let boxSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant objectBox)
                    |> wallNormalMap |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg", textureParam) :> ITexture))

    let rayCastHitPointSg = rayCastPointSg |> Sg.effect rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))
    let rayCastHitAreaSg = rayCastAreaSg |> Sg.effect rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))
    let rayCastCamSg = rayCastCamSg |> Sg.effect rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))
    //#endregion

    //#region Objects   
    let simpleControllerBodyAssimpScene = Loader.Assimp.Load(@"..\..\resources\models\SteamVR\vr_controller_vive_1_5\bodySimplified\bodytrisimple.obj", assimpFlagsSteamVR) 
    let simpleControllerBodyCollShape = createShape Trafo3d.Identity simpleControllerBodyAssimpScene.root |> BulletHelper.TriangleMesh |> toCollisionShape
    let numScales = 3
    let simpleControllerBodyCollShapeScaled = [| for i in 0..numScales -> 
                                                    let minScaling = 1.05
                                                    let maxScaling = 1.5
                                                    let reductionPerRound = (maxScaling - minScaling) / float numScales
                                                    let scale = maxScaling - (float i) * reductionPerRound
                                                    let controllerToVisualOrigin = Trafo3d.Translation(0.0, 0.01, -0.01)
                                                    let scaleTrafoWithOtherOrigin = controllerToVisualOrigin * Trafo3d.Scale(scale) * controllerToVisualOrigin.Inverse
                                                    createShape scaleTrafoWithOtherOrigin simpleControllerBodyAssimpScene.root |> BulletHelper.TriangleMesh |> toCollisionShape |]

    let controller1Object = 
        { defaultObject with
            id = newId()
            objectType = ObjectTypes.Kinematic
            collisionCallback = true
            model = Some controllerSg
            surface = Some diffuseSurface
            collisionShape = Some simpleControllerBodyCollShape
            restitution = 1.5f
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
            collisionGroup = CollisionGroups.Avatar |> int16
            collisionMask = avatarCollidesWith
        }
    let controller2Object = { controller1Object with id = newId() }
//    let controllerToVisualOrigin = Trafo3d.Translation(0.0, 0.01, -0.01) // left-right, up-down, forward-backward
    let grabTrigger1 = 
        { defaultObject with
            id = newId()
            castsShadow = false
            objectType = ObjectTypes.Ghost
            collisionShape = Some simpleControllerBodyCollShapeScaled.[0]
//            model = Some (controllerSg 
//                            |> Sg.trafo(Mod.constant(controllerToVisualOrigin * Trafo3d.Scale(1.1) * controllerToVisualOrigin.Inverse)) 
//                            |> Sg.blendMode(Mod.constant (BlendMode(true)))
//                            |> Sg.writeBuffers (Some (Set.singleton DefaultSemantic.Colors)))
//            surface = Some rayCastHitSurface
            isColliding = false
            collisionGroup = CollisionGroups.HandTrigger |> int16
            collisionMask = handTriggerCollidesWith
        }
    let grabTrigger2 = { grabTrigger1 with id = newId() }
    let camObject1 = 
        { defaultObject with
            id = newId()
//            model = Some basestationSg
//            surface = Some diffuseSurface
            isColliding = false
        }
    let camObject2 = { camObject1 with id = newId() }
    let lightObject =
        { defaultObject with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Translation(0.0, trackingAreaHeight - wallThickness * 2.5, 0.0)
            model = Some lightSg
            surface = Some constColorSurface
            isColliding = false
        }

    let defaultCollider =
        { defaultObject with
            rollingFriction = 0.01f
            restitution = 0.95f
            friction = 0.75f
            collisionCallback = true
        }

    let staticDefaultCollider =
        { defaultCollider with
            collisionGroup = CollisionGroups.Static |> int16
            collisionMask = staticCollidesWith
        }

    let groundTilingFactor = 0.3
    let ceilingTilingFactor = 0.4
    let groundObject = 
        { staticDefaultCollider with
            id = newId()
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0)
            model = Some groundSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(groundTilingFactor * trackingAreaSize)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box |> toCollisionShape )
        }

    let ceilingObject = 
        { staticDefaultCollider with
            id = newId()
            trafo = Trafo3d.Translation(0.0, trackingAreaHeight - 1.5 * wallThickness, 0.0)
            model = Some ceilingSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(ceilingTilingFactor * trackingAreaSize)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box |> toCollisionShape )
        }

    let wallBase = 
        { staticDefaultCollider with
            model = Some wallSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(0.25 * trackingAreaSize)
        }

    let wall1 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.Translation(0.0, wallHorizontalOffset, -wallLateralOffset)
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaHeight, wallThickness) |> BulletHelper.Shape.Box |> toCollisionShape )
        }
    let wall2 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (-90.0) * Trafo3d.Translation(wallLateralOffset, wallHorizontalOffset, 0.0)
            collisionShape = Some ( V3d(trackingAreaSize + 2.0 * wallThickness, trackingAreaHeight, wallThickness) |> BulletHelper.Shape.Box |> toCollisionShape )
        }
    let wall3 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (180.0) * Trafo3d.Translation(0.0, wallHorizontalOffset, wallLateralOffset)
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaHeight, wallThickness) |> BulletHelper.Shape.Box |> toCollisionShape )
        }
    let wall4 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (90.0) * Trafo3d.Translation(-wallLateralOffset, wallHorizontalOffset, 0.0)
            collisionShape = Some ( V3d(trackingAreaSize + 2.0 * wallThickness, trackingAreaHeight, wallThickness) |> BulletHelper.Shape.Box |> toCollisionShape )
        }
    
    let pedestalBase = 
        { defaultObject with
            model = Some pedestalSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(1.0, pedestalRadius / pedestalHeight)
//            collisionShape = Some (BulletHelper.Shape.CylinderY (pedestalRadius, pedestalHeight))
            collisionShape = Some ( V3d(pedestalRadius, pedestalHeight, pedestalRadius) |> BulletHelper.Shape.Box |> toCollisionShape )
            rollingFriction = 0.01f
            restitution = 0.95f
            friction = 0.75f
            collisionGroup = CollisionGroups.Static |> int16
            collisionMask = staticCollidesWith
        }
    let pedestal1 = { pedestalBase with id = newId(); trafo = Trafo3d.Translation(pedestal1Position)}
    let pedestal2 = { pedestalBase with id = newId(); trafo = Trafo3d.Translation(pedestal2Position)}
    let pedestal3 = { pedestalBase with id = newId(); trafo = Trafo3d.Translation(pedestal3Position)}
    let pedestal4 = { pedestalBase with id = newId(); trafo = Trafo3d.Translation(pedestal4Position)}

    let cushionBase =
        { defaultObject with
            model = Some cushionSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(0.25)
            collisionShape = Some ( V3d(cushionSize, cushionHeight, cushionSize) |> BulletHelper.Shape.Box |> toCollisionShape )
            rollingFriction = 0.1f
            restitution = 0.15f
            friction = 0.95f
            collisionGroup = CollisionGroups.Static |> int16
            collisionMask = staticCollidesWith
        }
    let cushion1 = { cushionBase with id = newId(); trafo = Trafo3d.Translation(cushion1Position)}
    let cushion2 = { cushionBase with id = newId(); trafo = Trafo3d.Translation(cushion2Position)}
    let cushion3 = { cushionBase with id = newId(); trafo = Trafo3d.Translation(cushion3Position)}
    let cushion4 = { cushionBase with id = newId(); trafo = Trafo3d.Translation(cushion4Position)}

    let ballBase = 
        { defaultCollider with
            objectType = ObjectTypes.Dynamic
            isManipulable = true
            collisionCallback = true
            surface = Some ballSurface
            mass = 0.625f
            collisionShape = Some (BulletHelper.Shape.Sphere ballRadius |> toCollisionShape)
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
            collisionGroup = CollisionGroups.Ball |> int16
            collisionMask = ballCollidesWith
        }
    let ball1 = { ballBase with id = newId(); trafo = Trafo3d.Translation(ball1ResetPos); model = Some basketballSg}
    let ball2 = { ballBase with id = newId(); trafo = Trafo3d.Translation(ball2ResetPos); model = Some beachballSg}
    let ball3 = { ballBase with id = newId(); trafo = Trafo3d.Translation(ball3ResetPos); model = Some softballSg}
    let ball4 = { ballBase with id = newId(); trafo = Trafo3d.Translation(ball4ResetPos); model = Some tennisballSg}
    let ballObjects = [ball1; ball2; ball3; ball4]
    
    let staticBallBase = 
        { defaultCollider with
            objectType = ObjectTypes.Static
            surface = Some ballSurface
            collisionShape = Some (BulletHelper.Shape.Sphere ballRadius |> toCollisionShape)
            collisionGroup = CollisionGroups.Static |> int16
            collisionMask = staticCollidesWith
        }
    let staticBall1 = { staticBallBase with id = newId(); trafo = targetBallTrafo; model = Some basketballSg}
    let staticBall2 = { staticBallBase with id = newId(); trafo = targetBallTrafo; model = Some beachballSg; visible = false}
    let staticBall3 = { staticBallBase with id = newId(); trafo = targetBallTrafo; model = Some softballSg; visible = false}
    let staticBall4 = { staticBallBase with id = newId(); trafo = targetBallTrafo; model = Some tennisballSg; visible = false}
    let staticBallObjects = [staticBall1; staticBall2; staticBall3; staticBall4]

    let box = 
        { defaultCollider with
            id = newId()
            objectType = ObjectTypes.Dynamic
            isManipulable = true
            collisionCallback = true
            trafo = Trafo3d.Translation(-0.5, 0.0, 0.0)
            model = Some boxSg
            surface = Some boxSurface
            mass = 0.625f
            collisionShape = Some ( V3d(objectBoxEdgeLength) |> BulletHelper.Shape.Box |> toCollisionShape )
            restitution = 0.1f
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
            collisionGroup = CollisionGroups.Ball |> int16
            collisionMask = ballCollidesWith
        }
    let headCollider = 
        { defaultCollider with
            id = newId()
            castsShadow = false
            objectType = ObjectTypes.Kinematic
            isManipulable = false
            trafo = Trafo3d.Translation(-0.1, 0.0, 0.0)
            collisionShape = Some (BulletHelper.Shape.Sphere 0.12 |> toCollisionShape)
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
            collisionGroup = CollisionGroups.Avatar |> int16
            collisionMask = avatarCollidesWith
        }
    let hoopTrigger = 
        { defaultObject with
            id = newId()
            castsShadow = false
            objectType = ObjectTypes.Ghost
            trafo = hoopTriggerTrafo
            collisionShape = Some (BulletHelper.Shape.CylinderY (0.16 * hoopScale, 0.02 * hoopScale) |> toCollisionShape)
            isColliding = false
            collisionGroup = CollisionGroups.HoopTrigger |> int16
            collisionMask = hoopTriggerCollidesWith
        }

    let toObjects (canMove : bool) (l : list<_>) =
        l |> List.mapi (fun i (file, (trafo : Trafo3d), mass, shape, restitution) ->
                let assimpScene : Loader.Scene =  Loader.Assimp.Load(file, assimpFlags) 
                let triangles = createShape trafo assimpScene.root
                let bounds = triangles |> Seq.collect (fun t -> [t.P0; t.P1; t.P2]) |> Box3d

                let sg = 
                    assimpScene 
                        |> Sg.AdapterNode :> ISg 
                        |> Sg.transform (trafo)

                let collShape = 
                    match shape with
                        | None ->
                            Some (triangles |> BulletHelper.TriangleMesh)
                        | Some s ->
                            s

                { defaultObject with
                    id = newId()
                    isManipulable = canMove
                    model = Some sg 
                    surface = Some diffuseSurface
                    mass = mass
                    objectType = ObjectTypes.Kinematic
                    collisionShape = match collShape with | Some shape -> Some (shape |> toCollisionShape) | None -> None
                    restitution = restitution
                    collisionGroup = CollisionGroups.Static |> int16
                    collisionMask = staticCollidesWith
                }
            )
        
    let staticObjects = toObjects false staticModels 
    let hoop = staticObjects.[0]

    let objects =
        ballObjects @ staticBallObjects
        @ [hoopTrigger; lightObject]
        @ [groundObject; ceilingObject; wall1; wall2; wall3; wall4]
        @ [pedestal1; pedestal2; pedestal3; pedestal4; cushion1; cushion2; cushion3; cushion4; hoop]
        @ [controller1Object; controller2Object; camObject1; camObject2; headCollider]
        @ [grabTrigger1; grabTrigger2]
    //#endregion
        
    //#region Scene   
    let specialObjectIds =
        {
            cam1ObjectId        = camObject1.id
            cam2ObjectId        = camObject2.id
            controller1ObjectId = controller1Object.id
            controller2ObjectId = controller2Object.id
            headId              = headCollider.id
            lightId             = lightObject.id
            hoopTriggerId       = hoopTrigger.id
            grabTrigger1Id      = grabTrigger1.id
            grabTrigger2Id      = grabTrigger2.id
            hoopObjectId        = hoop.id
            ballObjectIds       = [| ball1.id; ball2.id; ball3.id; ball4.id |]
            staticBallObjectIds = [| staticBall1.id; staticBall2.id; staticBall3.id; staticBall4.id |]
        }

    let firstArgAsInt = 
        if argv.Length > 0 then
            let firstArg = argv.[0]
            let mutable asInt = 3
            let success = System.Int32.TryParse(firstArg, &asInt)
            if success then asInt else 3
        else 
            printfn "No argument given. Using both feedback types"
            3

    let argToFeedbackType(a : int) =
        match a with
            | 0 -> FeedbackTypes.NoFeedback
            | 1 -> FeedbackTypes.HapticFeedback
            | 2 -> FeedbackTypes.OpticalFeedback
            | 3 -> FeedbackTypes.Both
            | _ -> printfn "Invalid argument given. Using both feedback types"; FeedbackTypes.Both
    let feedback = argToFeedbackType(firstArgAsInt)

    Logging.log("Starting Grabbing Test with feedback = " + firstArgAsInt.ToString())

    let sceneObj =
        {
            objects             = PersistentHashSet.ofList objects
            viewTrafo           = Trafo3d.Identity
            trackingToWorld     = Trafo3d.Identity
            ballResetPos        = [| ball1ResetPos; ball2ResetPos; ball3ResetPos; ball4ResetPos |]

            bounceSoundSources  = bounceSound
            sireneSoundSource   = sireneSound
            popSoundSource      = popSound
            physicsMessages     = []
            lightColor          = V3d(1.0, 1.0, 0.5)
            feedbackTypes       = feedback

            ballSgs             = [| basketballSg; beachballSg; softballSg; tennisballSg |]
            targetBallTrafo     = targetBallTrafo
            grabbingVolShape    = simpleControllerBodyCollShapeScaled
            
            specialObjectIds    = specialObjectIds
            interactionInfo1    = DefaultInteractionInfo
            interactionInfo2    = DefaultInteractionInfo
            gameInfo            = { DefaultGameInfo with 
                                        scoreTrafo = scoreTrafo
                                  }
            physicsInfo         = { DefaultPhysicsInfo with
                                        raycastCollGroup = CollisionGroups.TeleportRaycast |> int16
                                        raycastCollMask  = teleportRaycastCollidesWith
//                                        physicsDebugDraw = true
                                  }
        }
    //#endregion

    let scene = GraphicsScene.createScene sceneObj vrWin

    let task = app.Runtime.CompileRender(vrWin.FramebufferSignature, scene)
    vrWin.RenderTask <- task
    
    vrWin.Run()
    
    Logging.endSession()

    OpenVR.Shutdown()

    0
