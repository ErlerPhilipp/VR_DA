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

open LogicalScene

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

    use app = new OpenGlApplication()
    let vrWin = VrWindow.VrWindow(app.Runtime, true)
    
    let defaultTrafoEffect = DefaultSurfaces.trafo |> toEffect
    let defaultSimpleLightingEffect = DefaultSurfaces.simpleLighting |> toEffect
    let defaultDiffuseTextureEffect = DefaultSurfaces.diffuseTexture |> toEffect

    let virtualHandEffect = Sg.effect [
                                defaultTrafoEffect
                                DefaultSurfaces.uniformColor (LogicalScene.virtualHandColor) |> toEffect
                                defaultSimpleLightingEffect
                            ]
    let constColorEffect = Sg.effect [
                                defaultTrafoEffect
                                DefaultSurfaces.constantColor C4f.White |> toEffect
                                defaultSimpleLightingEffect
                            ]
    let rayCastHitEffect = Sg.effect [
                                defaultTrafoEffect
                                DefaultSurfaces.constantColor (C4f(0.3f, 0.3f, 0.9f, 0.3f)) |> toEffect
                                defaultSimpleLightingEffect
                            ]
    let beamEffect = Sg.effect [
                        defaultTrafoEffect
                        DefaultSurfaces.vertexColor |> toEffect
                        DefaultSurfaces.thickLine |> toEffect
                    ]
    let diffuseEffect = Sg.effect [
                                    defaultTrafoEffect
                                    defaultDiffuseTextureEffect
                                    Lighting.Effect false
                                ]
    let normalDiffuseEffect = Sg.effect [
                                    defaultTrafoEffect
                                    TextureTiling.Effect
                                    NormalMap.Effect
                                    defaultDiffuseTextureEffect
                                    Lighting.Effect false
                                ]
    let boxEffect = Sg.effect [
                        defaultTrafoEffect
                        TextureTiling.Effect
                        NormalMap.Effect
                        defaultDiffuseTextureEffect
                        Lighting.Effect false
                        Highlight.Effect
                    ]
    let ballEffect = Sg.effect [
                        SphereTexture.vertex |> toEffect
                        defaultTrafoEffect
                        SphereTexture.fragment |> toEffect
                        defaultDiffuseTextureEffect
                        Lighting.Effect false
                        Highlight.Effect
                    ]

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
    let textureParam = { TextureParams.empty with wantMipMaps = true }
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
            |> diffuseEffect

    let basestationSg = 
        (assimpFlagsSteamVR, @"..\..\resources\models\SteamVR\lh_basestation_vive\basestationtri.obj")
            |> Loader.Assimp.load
            |> Sg.AdapterNode :> ISg
            |> diffuseEffect 
            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\models\SteamVR\lh_basestation_vive\lh_basestation_vive.png", textureParam) :> ITexture))
            
    let beamSg = Sg.lines (Mod.constant C4b.Red) (Mod.constant ( [| Line3d(V3d.OOO, -V3d.OOI * 100.0) |]) ) 
    let ballSg = Sg.sphere 6 (Mod.constant C4b.DarkYellow) (Mod.constant 0.1213)
    let lightSg = Sg.sphere 3 (Mod.constant C4b.White) (Mod.constant 0.1)
    let rayCastAreaSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, 0.1, trackingAreaSize))))
    let rayCastPointSg = Sg.sphere 4 (Mod.constant C4b.Green) (Mod.constant 0.08)
    let rayCastCamSg = Sg.cone 4 (Mod.constant (C4b(50, 250, 50, 80))) (Mod.constant 0.1) (Mod.constant 0.4)

    let groundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
    let ceilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
    let wallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, trackingAreaHight, wallThickness))))

    let goalRoomGroundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, wallThickness, goalAreaSize))))
    let goalRoomCeilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, wallThickness, goalAreaSize))))
    let goalRoomWallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, goalAreaHight, wallThickness))))

    let camBox = Box3d.FromCenterAndSize(V3d.OOO, 0.15 * V3d.III)

    let objectBoxEdgeLength = 0.25
    let objectBox = Box3d.FromCenterAndSize(V3d.OOO, objectBoxEdgeLength * V3d.III)
    let boxSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant objectBox)

    let groundNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_normalmap_S.jpg", textureParam) :> ITexture))
    let ceilingNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_normalmap_S.jpg", textureParam) :> ITexture))
    let wallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_normalmap_S.jpg", textureParam) :> ITexture))
    let goalRoomWallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Brown Bricks\TexturesCom_Brown Bricks_normalmap_S.jpg", textureParam) :> ITexture))

    let groundDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_albedo_S.jpg", textureParam) :> ITexture))
    let ceilingDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_albedo_S.jpg", textureParam) :> ITexture))
    let wallDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg", textureParam) :> ITexture))
    let goalRoomWallDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Brown Bricks\TexturesCom_Brown Bricks_albedo_S.jpg", textureParam) :> ITexture))

    let rayCastHitPointSg = rayCastPointSg |> rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))
    let rayCastHitAreaSg = rayCastAreaSg |> rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))
    let rayCastCamSg = rayCastCamSg |> rayCastHitEffect |> Sg.blendMode(Mod.constant (BlendMode(true)))

    let leftHandObject = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Identity
//            model = Sg.ofList [controllerSg |> constColorEffect; beamSg |> beamEffect]
            model = Sg.ofList [controllerSg |> constColorEffect]
            isColliding = false
        }
    let rightHandObject = 
        { defaultObject with
            id = newId()
            objectType = ObjectTypes.Ghost
            trafo = Trafo3d.Identity
            model = controllerSg |> virtualHandEffect
            collisionShape = Some ( V3d(handBoxEdgeLength) |> BulletHelper.Shape.Box )
            isColliding = false
        }
    let camObject1 = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Identity
            model = basestationSg
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
            trafo = Trafo3d.Translation(-(0.5 * trackingAreaSize - wallThickness - 2.0), trackingAreaHight - wallThickness * 2.5, 0.0)
            model = lightSg |>  constColorEffect
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
            castsShadow = true
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0)
            model = groundSg 
                        |> normalDiffuseEffect 
                        |> groundDiffuseTexture
                        |> groundNormalMap
            tilingFactor = V2d(groundTilingFactor * trackingAreaSize)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box )
        }

    let ceilingObject = 
        { defaultCollider with
            id = newId()
            castsShadow = true
            trafo = Trafo3d.Translation(0.0, trackingAreaHight - 1.5 * wallThickness, 0.0)
            model = ceilingSg 
                        |> normalDiffuseEffect 
                        |> ceilingDiffuseTexture
                        |> ceilingNormalMap
            tilingFactor = V2d(ceilingTilingFactor * trackingAreaSize)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box )
        }

    let wallBase = 
        { defaultCollider with
            castsShadow = true
            model = wallSg
                        |> normalDiffuseEffect
                        |> wallDiffuseTexture
                        |> wallNormalMap
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
            castsShadow = false
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0) * goalRoomOffset
            model = goalRoomGroundSg 
                        |> normalDiffuseEffect 
                        |> groundDiffuseTexture
                        |> groundNormalMap
            tilingFactor = V2d(groundTilingFactor * goalAreaSize)
            collisionShape = Some ( V3d(goalAreaSize, wallThickness, goalAreaSize) |> BulletHelper.Shape.Box )
        }

    let goalRoomCeilingObject = 
        { defaultCollider with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Translation(0.0, goalAreaHight - 1.5 * wallThickness, 0.0) * goalRoomOffset
            model = goalRoomCeilingSg 
                        |> normalDiffuseEffect 
                        |> ceilingDiffuseTexture
                        |> ceilingNormalMap
            tilingFactor = V2d(ceilingTilingFactor * goalAreaSize)
            collisionShape = Some ( V3d(goalAreaSize, wallThickness, goalAreaSize) |> BulletHelper.Shape.Box )
        }

    let goalRoomWallBase = 
        { defaultCollider with
            castsShadow = false
            model = goalRoomWallSg
                        |> normalDiffuseEffect
                        |> goalRoomWallDiffuseTexture
                        |> goalRoomWallNormalMap
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
            trafo = Trafo3d.Identity
            model = ballSg 
                        |> ballEffect 
                        |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\basketball\balldimpled.png", textureParam) :> ITexture))
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
            model = boxSg 
                        |> boxEffect 
                        |> wallNormalMap
                        |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg", textureParam) :> ITexture))
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
            model = Sg.ofList []
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
            model = Sg.ofList []
            collisionShape = Some (BulletHelper.Shape.CylinderY (0.16 * hoopScale, 0.02 * hoopScale))
            isColliding = false
        }
    let upperHoopTrigger = 
        { lowerHoopTrigger with
            id = newId()
            castsShadow = false
            objectType = ObjectTypes.Ghost
            trafo = lowerHoopTrigger.trafo * Trafo3d.Translation(0.0, 0.12 * hoopScale, 0.0)
            model = Sg.ofList []
            collisionShape = Some (BulletHelper.Shape.CylinderY (0.16 * hoopScale, 0.02 * hoopScale))
            isColliding = false
        }

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
                        trafo = Trafo3d.Identity
                        model = sg 
                        mass = mass
                        collisionShape = collShape
                        restitution = restitution
                    }
                )

        let manipulableObjects = replicate ((toObjects true manipulableModels), 1)
        let ballObjects = replicate ([ball], 10)
        let boxObjects = replicate ([box], 10)
        
        manipulableObjects @ 
        ballObjects @ boxObjects @
        toObjects false staticModels 
        @ [lowerHoopTrigger; upperHoopTrigger; lightObject]
        @ [groundObject; ceilingObject; wall1; wall3; wall4]
        @ [goalRoomGroundObject; goalRoomCeilingObject; goalRoomWall1; goalRoomWall2; goalRoomWall3;]
        @ [leftHandObject; rightHandObject; camObject1; camObject2; headCollider]
        
    let specialObjectIds =
        {
            cam1ObjectId        = camObject1.id
            cam2ObjectId        = camObject2.id
            controller1ObjectId = leftHandObject.id
            controller2ObjectId = rightHandObject.id
            headId              = headCollider.id
            lightId             = lightObject.id
            lowerHoopTriggerId  = lowerHoopTrigger.id
            upperHoopTriggerId  = upperHoopTrigger.id
            groundObjectId      = goalRoomGroundObject.id
        }

    let sceneObj =
        {
            objects             = PersistentHashSet.ofList objects
            viewTrafo           = Trafo3d.Identity
            lastContr2Trafo     = Trafo3d.Identity
            trackingToWorld     = Trafo3d.Identity

            specialObjectIds = specialObjectIds
            interactionInfo = DefaultInteractionInfo
            gameInfo = DefaultGameInfo(scoreTrafo)
            physicsInfo = DefaultPhysicsInfo
            raycastInfo = DefaultRaycastInfo(beamSg |> beamEffect, rayCastHitPointSg, rayCastHitAreaSg, rayCastCamSg)
            vibrationInfo = DefaultVibrationInfo
        }

    let scene =
        GraphicsScene.createScene sceneObj vrWin
            |> Sg.effect [
                defaultTrafoEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                defaultDiffuseTextureEffect
                DefaultSurfaces.normalMap |> toEffect
                DefaultSurfaces.lighting false |> toEffect
            ]

    let vrSg = 
        scene
            |> Sg.projTrafo vrWin.Projection
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)

 
    let task = app.Runtime.CompileRender(vrWin.FramebufferSignature, vrSg)
    vrWin.RenderTask <- task
    
    vrWin.Run()

    OpenVR.Shutdown()

    0
