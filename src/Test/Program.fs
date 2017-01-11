// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module VRTestApp

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

open Primitives
open Sphere
open SGHelper
open NormalMap
open TextureTiling
open Lighting
open Highlight

[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()
    Aardvark.SceneGraph.IO.Loader.Assimp.initialize()
    Aardvark.Rendering.GL.RuntimeConfig.SupressSparseBuffers <- true

    use app = new OpenGlApplication()
    let vrWin = VrWindow.VrWindow(app.Runtime, true)
    
    let defaultTrafoEffect = DefaultSurfaces.trafo |> toEffect
    let defaultSimpleLightingEffect = DefaultSurfaces.simpleLighting |> toEffect
    let defaultDiffuseTextureEffect = DefaultSurfaces.diffuseTexture |> toEffect

    let constColorEffect =      [
                                    defaultTrafoEffect
                                    DefaultSurfaces.constantColor C4f.White |> toEffect
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
    let diffuseContrEffect =    [
                                    defaultTrafoEffect
                                    defaultDiffuseTextureEffect
                                    ControllerOverlayColor.Effect
                                    OmnidirShadowShader.Effect false
                                ] 
    let diffuseContrSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, diffuseContrEffect) :> ISurface
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

    let trackingAreaSize = 2.9
    let trackingAreaHight = 5.2
    let goalAreaSize = 4.5
    let goalAreaHight = 6.5
    let wallThickness = 1.0
    let goalRoomOffset = Trafo3d.Translation(0.5 * trackingAreaSize + 0.5 * goalAreaSize, (trackingAreaSize - goalAreaSize) * 0.5, 0.0)

    let hoopScale = 2.0
    let hoopTrafoWithoutScale = Trafo3d.RotationYInDegrees(90.0) * goalRoomOffset * Trafo3d.Translation(-1.1, -1.1, 0.0)
    let hoopTrafo = Trafo3d.Scale hoopScale * hoopTrafoWithoutScale
    let scoreScale = 0.1
    let scoreTrafo = Trafo3d.Scale(scoreScale * hoopScale) * Trafo3d.RotationYInDegrees(180.0) * hoopTrafoWithoutScale * 
                        Trafo3d.Translation(V3d(-0.03, 1.71, -3.3 * scoreScale) * hoopScale)
    
    let textureParam = { TextureParams.empty with wantMipMaps = true }
    let groundNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_normalmap_S.jpg", textureParam) :> ITexture))
    let ceilingNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_normalmap_S.jpg", textureParam) :> ITexture))
    let wallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_normalmap_S.jpg", textureParam) :> ITexture))
    let goalRoomWallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Brown Bricks\TexturesCom_Brown Bricks_normalmap_S.jpg", textureParam) :> ITexture))

    let groundDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_albedo_S.jpg", textureParam) :> ITexture))
    let ceilingDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_albedo_S.jpg", textureParam) :> ITexture))
    let wallDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg", textureParam) :> ITexture))
    let goalRoomWallDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Brown Bricks\TexturesCom_Brown Bricks_albedo_S.jpg", textureParam) :> ITexture))

    let staticModels =
        [
            //@"C:\Aardwork\sponza\sponza.obj", Trafo3d.Scale 0.01, Mass.Infinite, None, 0.5f
            @"..\..\resources\models\basketball\hoop.obj", hoopTrafo, 0.0f, None, 0.5f
        ]

    let manipulableModels =
        [
            //@"C:\Aardwork\Stormtrooper\Stormtrooper.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-0.5, 0.0, 0.0), Mass 100.0f, None, 0.5f
            //@"C:\Aardwork\witcher\geralt.obj", Trafo3d.Translation(0.0, 4.0, 0.0), Mass 80.0f, None, 0.5
            //@"C:\Aardwork\ironman\ironman.obj", Trafo3d.Scale 0.5 * Trafo3d.Translation(0.0, 0.0, 0.0), Mass 100.0f, None, 0.5f
            //@"C:\Aardwork\lara\lara.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0), Mass 60.0f, None, 0.5f
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
   

    let handBoxEdgeLength = 0.1
    let handBox = Box3d.FromCenterAndSize(V3d.OOO, handBoxEdgeLength * V3d.III)
    let handSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant handBox)
    let controllerSg = 
        let controllerBody = (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\bodytri.obj") |> Loader.Assimp.load |> Sg.AdapterNode :> ISg
        let controllerButton = (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\buttontri.obj") |> Loader.Assimp.load |> Sg.AdapterNode :> ISg
        let controllerLGrip = (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\lgriptri.obj") |> Loader.Assimp.load |> Sg.AdapterNode :> ISg
        let controllerRGrip = (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\rgriptri.obj") |> Loader.Assimp.load |> Sg.AdapterNode :> ISg
        let controllerSysButton = (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\sysbuttontri.obj") |> Loader.Assimp.load |> Sg.AdapterNode :> ISg
        let controllerTrackpad = (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\trackpadtri.obj") |> Loader.Assimp.load |> Sg.AdapterNode :> ISg
        let controllerTrigger = (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\triggertri.obj") |> Loader.Assimp.load |> Sg.AdapterNode :> ISg
        [ controllerBody; controllerButton; controllerLGrip; controllerRGrip; controllerSysButton; controllerTrackpad; controllerTrigger ]
            |> Sg.group :> ISg
            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\models\SteamVR\vr_controller_vive_1_5\onepointfive_texture.png", textureParam) :> ITexture))

    let basestationSg = 
        (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\lh_basestation_vive\basestationtri.obj")
            |> Loader.Assimp.load
            |> Sg.AdapterNode :> ISg
            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\models\SteamVR\lh_basestation_vive\lh_basestation_vive.png", textureParam) :> ITexture))
            
    let beamSg = Sg.lines (Mod.constant C4b.Red) (Mod.constant ( [| Line3d(V3d.OOO, -V3d.OOI * 100.0) |]) ) 
                    |> Sg.effect beamEffect
    let ballSg = Sg.sphere 6 (Mod.constant C4b.DarkYellow) (Mod.constant 0.1213)
                    |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\basketball\balldimpled.png", textureParam) :> ITexture))
    let lightSg = Sg.sphere 3 (Mod.constant C4b.White) (Mod.constant 0.1)
    let rayCastAreaSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, 0.1, trackingAreaSize))))
    let rayCastPointSg = Sg.sphere 4 (Mod.constant C4b.Green) (Mod.constant 0.08)
    let rayCastCamSg = Sg.cone 4 (Mod.constant (C4b(50, 250, 50, 80))) (Mod.constant 0.1) (Mod.constant 0.4)

    let groundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
                            |> groundDiffuseTexture |> groundNormalMap
    let ceilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
                            |> ceilingDiffuseTexture |> ceilingNormalMap
    let wallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, trackingAreaHight, wallThickness))))
                            |> wallDiffuseTexture |> wallNormalMap
    let goalRoomGroundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, wallThickness, goalAreaSize))))
                            |> groundDiffuseTexture |> groundNormalMap
    let goalRoomCeilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, wallThickness, goalAreaSize))))
                            |> ceilingDiffuseTexture |> ceilingNormalMap
    let goalRoomWallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, goalAreaHight, wallThickness))))
                            |> goalRoomWallDiffuseTexture |> goalRoomWallNormalMap

    let camBox = Box3d.FromCenterAndSize(V3d.OOO, 0.15 * V3d.III)

    let objectBoxEdgeLength = 0.25
    let objectBox = Box3d.FromCenterAndSize(V3d.OOO, objectBoxEdgeLength * V3d.III)
    let boxSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant objectBox)
                    |> wallNormalMap |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg", textureParam) :> ITexture))

    let rayCastHitPointSg = rayCastPointSg |> Sg.effect rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))
    let rayCastHitAreaSg = rayCastAreaSg |> Sg.effect rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))
    let rayCastCamSg = rayCastCamSg |> Sg.effect rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))

    let simpleControllerBodyAssimpScene = (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\bodySimplified\bodytrisimple.obj") |> Loader.Assimp.load
    let simpleControllerBodyTriangles = createShape Trafo3d.Identity simpleControllerBodyAssimpScene.root
    let simpleControllerBodyCollShape = simpleControllerBodyTriangles |> BulletHelper.TriangleMesh
    let controller1Object = 
        { defaultObject with
            id = newId()
            objectType = ObjectTypes.Kinematic
            model = Some controllerSg
            surface = Some diffuseContrSurface
            collisionShape = Some simpleControllerBodyCollShape
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
            isColliding = true
        }
    let controller2Object = { controller1Object with id = newId() }
    let grabTrigger1 = 
        { defaultObject with
            id = newId()
            castsShadow = false
            objectType = ObjectTypes.Ghost
//            collisionShape = Some (BulletHelper.Shape.Sphere 0.15)
            collisionShape = Some (BulletHelper.Shape.CylinderZ (0.09, 0.25))
            isColliding = false
        }
    let grabTrigger2 = { grabTrigger1 with id = newId() }
    let camObject1 = 
        { defaultObject with
            id = newId()
            model = Some basestationSg
            surface = Some diffuseSurface
            isColliding = false
        }
    let camObject2 = 
        { camObject1 with
            id = newId()
        }
    let lightObject =
        { defaultObject with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Translation(-(0.5 * trackingAreaSize - wallThickness * 2.0), trackingAreaHight - wallThickness * 2.5, 0.0)
            model = Some lightSg
            surface = Some constColorSurface
        }

    let defaultCollider =
        { defaultObject with
            rollingFriction = 0.01f
            restitution = 0.95f
            friction = 0.75f
        }

    let groundTilingFactor = 0.3
    let ceilingTilingFactor = 0.4
    let groundObject = 
        { defaultCollider with
            id = newId()
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0)
            model = Some groundSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(groundTilingFactor * trackingAreaSize)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box )
        }

    let ceilingObject = 
        { defaultCollider with
            id = newId()
            trafo = Trafo3d.Translation(0.0, trackingAreaHight - 1.5 * wallThickness, 0.0)
            model = Some ceilingSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(ceilingTilingFactor * trackingAreaSize)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box )
        }

    let wallBase = 
        { defaultCollider with
            model = Some wallSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(0.25 * trackingAreaSize)
        }
    let wallLateralOffset = trackingAreaSize * 0.5 + wallThickness * 0.5
    let wallHorizontalOffset = trackingAreaHight * 0.5 - wallThickness
    let wall1 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.Translation(0.0, wallHorizontalOffset, -wallLateralOffset)
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaHight, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall2 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (-90.0) * Trafo3d.Translation(wallLateralOffset, wallHorizontalOffset, 0.0)
            collisionShape = Some ( V3d(trackingAreaSize + 2.0 * wallThickness, trackingAreaHight, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall3 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (180.0) * Trafo3d.Translation(0.0, wallHorizontalOffset, wallLateralOffset)
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaHight, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall4 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (90.0) * Trafo3d.Translation(-wallLateralOffset, wallHorizontalOffset, 0.0)
            collisionShape = Some ( V3d(trackingAreaSize + 2.0 * wallThickness, trackingAreaHight, wallThickness) |> BulletHelper.Shape.Box )
        }

    let goalRoomGroundObject = 
        { defaultCollider with
            id = newId()
            objectType = ObjectTypes.Ghost
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0) * goalRoomOffset
            model = Some goalRoomGroundSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(groundTilingFactor * goalAreaSize)
            collisionShape = Some ( V3d(goalAreaSize, wallThickness, goalAreaSize) |> BulletHelper.Shape.Box )
        }

    let goalRoomCeilingObject = 
        { defaultCollider with
            id = newId()
            trafo = Trafo3d.Translation(0.0, goalAreaHight - 1.5 * wallThickness, 0.0) * goalRoomOffset
            model = Some goalRoomCeilingSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(ceilingTilingFactor * goalAreaSize)
            collisionShape = Some ( V3d(goalAreaSize, wallThickness, goalAreaSize) |> BulletHelper.Shape.Box )
        }

    let goalRoomWallBase = 
        { defaultCollider with
            model = Some goalRoomWallSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(0.5 * goalAreaSize)
        }
    let goalWallLateralOffset = goalAreaSize * 0.5 + wallThickness * 0.5
    let goalWallHorizontalOffset = goalAreaHight * 0.5 - wallThickness
    let goalRoomWall1 = 
        { goalRoomWallBase with 
            id = newId()
            trafo = Trafo3d.Translation(0.0, goalWallHorizontalOffset, -goalWallLateralOffset) * goalRoomOffset
            collisionShape = Some ( V3d(goalAreaSize, goalAreaHight, wallThickness) |> BulletHelper.Shape.Box )
        }
    let goalRoomWall2 = 
        { goalRoomWallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (-90.0) * Trafo3d.Translation(goalWallLateralOffset, goalWallHorizontalOffset, 0.0) * goalRoomOffset
            collisionShape = Some ( V3d(goalAreaSize + 2.0 * wallThickness, goalAreaHight, wallThickness) |> BulletHelper.Shape.Box )
        }
    let goalRoomWall3 = 
        { goalRoomWallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (180.0) * Trafo3d.Translation(0.0, goalWallHorizontalOffset, goalWallLateralOffset) * goalRoomOffset
            collisionShape = Some ( V3d(goalAreaSize, goalAreaHight, wallThickness) |> BulletHelper.Shape.Box )
        }
    let goalRoomWall4 = 
        { goalRoomWallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (90.0) * Trafo3d.Translation(-goalWallLateralOffset, goalWallHorizontalOffset, 0.0) * goalRoomOffset
            collisionShape = Some ( V3d(goalAreaSize + 2.0 * wallThickness, goalAreaHight, wallThickness) |> BulletHelper.Shape.Box )
        }

    let ball = 
        { defaultCollider with
            id = newId()
            objectType = ObjectTypes.Dynamic
            isManipulable = true
            model = Some ballSg
            surface = Some ballSurface
            mass = 0.625f
            collisionShape = Some (BulletHelper.Shape.Sphere 0.1213)
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
        }
    let box = 
        { defaultCollider with
            id = newId()
            objectType = ObjectTypes.Dynamic
            isManipulable = true
            trafo = Trafo3d.Translation(-0.5, 0.0, 0.0)
            model = Some boxSg
            surface = Some boxSurface
            mass = 0.625f
            collisionShape = Some ( V3d(objectBoxEdgeLength) |> BulletHelper.Shape.Box )
            restitution = 0.1f
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
        }
    let headCollider = 
        { defaultCollider with
            id = newId()
            castsShadow = false
            objectType = ObjectTypes.Kinematic
            isManipulable = false
            trafo = Trafo3d.Translation(-0.1, 0.0, 0.0)
            collisionShape = Some (BulletHelper.Shape.Sphere 0.12)
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
        }
    let lowerHoopTrigger = 
        { defaultObject with
            id = newId()
            castsShadow = false
            objectType = ObjectTypes.Ghost
            trafo = hoopTrafoWithoutScale * Trafo3d.Translation(-0.23 * hoopScale, 1.38 * hoopScale, 0.0)
            collisionShape = Some (BulletHelper.Shape.CylinderY (0.16 * hoopScale, 0.02 * hoopScale))
            isColliding = false
        }
    let upperHoopTrigger = 
        { lowerHoopTrigger with
            id = newId()
            castsShadow = false
            objectType = ObjectTypes.Ghost
            trafo = lowerHoopTrigger.trafo * Trafo3d.Translation(0.0, 0.12 * hoopScale, 0.0)
            collisionShape = Some (BulletHelper.Shape.CylinderY (0.16 * hoopScale, 0.02 * hoopScale))
            isColliding = false
        }

//    let ObjectListToIdList(objectList : List<Object>) = objectList |> List.map (fun e -> e.id)

    let ballObjects = replicate ([ball], 1)
    let boxObjects = replicate ([box], 0)
//    let ballObjectIds = ObjectListToIdList(ballObjects)

    let objects =
        let toObjects (canMove : bool) (l : list<_>) =
            l |> List.mapi (fun i (file, (trafo : Trafo3d), mass, shape, restitution) ->
                    let assimpScene : Loader.Scene = (assimpFlags, file) |> Loader.Assimp.load 
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
                        collisionShape = collShape
                        restitution = restitution
                    }
                )
        let manipulableObjects = toObjects true manipulableModels
        
        manipulableObjects @ 
        ballObjects @ boxObjects @
        toObjects false staticModels 
        @ [lowerHoopTrigger; upperHoopTrigger; lightObject]
        @ [groundObject; ceilingObject; wall1; wall3; wall4]
        @ [goalRoomGroundObject; goalRoomCeilingObject; goalRoomWall1; goalRoomWall2; goalRoomWall3;]
        @ [controller1Object; controller2Object; camObject1; camObject2; headCollider]
        @ [grabTrigger1; grabTrigger2]
        
    let specialObjectIds =
        {
            cam1ObjectId        = camObject1.id
            cam2ObjectId        = camObject2.id
            controller1ObjectId = controller1Object.id
            controller2ObjectId = controller2Object.id
            headId              = headCollider.id
            lightId             = lightObject.id
            lowerHoopTriggerId  = lowerHoopTrigger.id
            upperHoopTriggerId  = upperHoopTrigger.id
            groundObjectId      = goalRoomGroundObject.id
            grabTrigger1Id      = grabTrigger1.id
            grabTrigger2Id      = grabTrigger2.id
        }

    let sceneObj =
        {
            objects             = PersistentHashSet.ofList objects
            viewTrafo           = Trafo3d.Identity
            trackingToWorld     = Trafo3d.Identity
            
            rayCastDirSg        = beamSg
            rayCastHitPointSg   = rayCastHitPointSg
            rayCastHitAreaSg    = rayCastHitAreaSg
            rayCastCamSg        = rayCastCamSg

            specialObjectIds    = specialObjectIds
            interactionInfo1    = DefaultInteractionInfo
            interactionInfo2    = DefaultInteractionInfo
            gameInfo            = DefaultGameInfo(scoreTrafo)
            physicsInfo         = DefaultPhysicsInfo
        }

    let scene = GraphicsScene.createScene sceneObj vrWin

    let task = app.Runtime.CompileRender(vrWin.FramebufferSignature, scene)
    vrWin.RenderTask <- task
    
    vrWin.Run()

    OpenVR.Shutdown()

    0
