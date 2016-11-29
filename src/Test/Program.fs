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

    use app = new OpenGlApplication()
    let vrWin = VrWindow.VrWindow(app.Runtime, true)

    let staticModels =
        [
            //@"C:\Aardwork\sponza\sponza.obj", Trafo3d.Scale 0.01, Mass.Infinite, None, 0.5f
        ]

    let manipulableModels =
        [
            //@"C:\Aardwork\Stormtrooper\Stormtrooper.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-0.5, 0.0, 0.0), Mass 100.0f, None, 0.5f
            //@"C:\Aardwork\witcher\geralt.obj", Trafo3d.Translation(0.0, 4.0, 0.0), Mass 80.0f, None, 0.5
            //@"C:\Aardwork\ironman\ironman.obj", Trafo3d.Scale 0.5 * Trafo3d.Translation(0.0, 0.0, 0.0), Mass 100.0f, None, 0.5f
            //@"C:\Aardwork\lara\lara.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0), Mass 60.0f, None, 0.5f
        ]
    
    let trackingAreaSize = 4.6
    let goalAreaSize = 6.0
    let wallThickness = 1.0
    let goalRoomOffset = Trafo3d.Translation(0.5 * trackingAreaSize + 0.5 * goalAreaSize, (trackingAreaSize - goalAreaSize) * 0.5, 0.0)

    let handBoxEdgeLength = 0.1
    let handBox = Box3d.FromCenterAndSize(V3d.OOO, handBoxEdgeLength * V3d.III)
    let handSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant handBox) 
    let beamSg = Sg.lines (Mod.constant C4b.Red) (Mod.constant ( [| Line3d(V3d.OOO, -V3d.OOI * 100.0) |]) ) 
    let ballSg = Sg.sphere 4 (Mod.constant C4b.DarkYellow) (Mod.constant 0.1213)
    let lightSg = Sg.sphere 3 (Mod.constant C4b.White) (Mod.constant 0.1)

    let groundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
    let ceilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
    let wallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, trackingAreaSize, wallThickness))))

    let goalRoomGroundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, wallThickness, goalAreaSize))))
    let goalRoomCeilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, wallThickness, goalAreaSize))))
    let goalRoomWallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(goalAreaSize, goalAreaSize, wallThickness))))
    
    let camBox = Box3d.FromCenterAndSize(V3d.OOO, 0.15 * V3d.III)

    let objectBoxEdgeLength = 0.25
    let objectBox = Box3d.FromCenterAndSize(V3d.OOO, objectBoxEdgeLength * V3d.III)
    let boxSg = BoxSg.box (Mod.constant C4b.Green) (Mod.constant objectBox)

    let groundNormalSampler = (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_normalmap_S.jpg", true) :> ITexture))
    let groundNormalMap = Sg.texture DefaultSemantic.NormalMapTexture groundNormalSampler

    let ceilingNormalSampler = (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_normalmap_S.jpg", true) :> ITexture))
    let ceilingNormalMap = Sg.texture DefaultSemantic.NormalMapTexture ceilingNormalSampler

    let wallNormalSampler = (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_normalmap_S.jpg", true) :> ITexture))
    let wallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture wallNormalSampler

    let goalRoomWallNormalSampler = (Mod.constant (FileTexture(@"..\..\resources\textures\Brown Bricks\TexturesCom_Brown Bricks_normalmap_S.jpg", true) :> ITexture))
    let goalRoomWallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture goalRoomWallNormalSampler

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
    let beamEffect = Sg.effect [
                        defaultTrafoEffect
                        DefaultSurfaces.vertexColor |> toEffect
                        DefaultSurfaces.thickLine |> toEffect
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


    let leftHandObject = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Identity
            model = Sg.ofList [handSg |> constColorEffect; beamSg |> beamEffect]
            isColliding = false
        }
    let rightHandObject = 
        { defaultObject with
            id = newId()
            objectType = ObjectTypes.Ghost
            trafo = Trafo3d.Identity
            model = handSg |> virtualHandEffect
            collisionShape = Some ( V3d(handBoxEdgeLength) |> BulletHelper.Shape.Box )
            isColliding = false
        }
    let camObject1 = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Identity
            model = handSg |> constColorEffect
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
//            trafo = Trafo3d.Translation(0.5 * trackingAreaSize - wallThickness - 0.5, trackingAreaSize - wallThickness * 2.0, 0.0)
            trafo = Trafo3d.Translation(0.0, trackingAreaSize - wallThickness * 2.0, 0.0)
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
                        |> Sg.diffuseFileTexture' @"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_albedo_S.jpg" true
                        |> groundNormalMap
            tilingFactor = V2d(groundTilingFactor * trackingAreaSize)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box )
        }

    let ceilingObject = 
        { defaultCollider with
            id = newId()
            castsShadow = true
            trafo = Trafo3d.Translation(0.0, trackingAreaSize - 0.5 * wallThickness, 0.0)
            model = ceilingSg 
                        |> normalDiffuseEffect 
                        |> Sg.diffuseFileTexture' @"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_albedo_S.jpg" true
                        |> ceilingNormalMap
            tilingFactor = V2d(ceilingTilingFactor * trackingAreaSize)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box )
        }

    let wallBase = 
        { defaultCollider with
            castsShadow = true
            model = wallSg
                        |> normalDiffuseEffect
                        |> Sg.diffuseFileTexture' @"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg" true
                        |> wallNormalMap
            tilingFactor = V2d(0.25 * trackingAreaSize)
        }
    let wall1 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.Translation(0.0, trackingAreaSize * 0.5 - wallThickness, -trackingAreaSize * 0.5)
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall2 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (-90.0) * Trafo3d.Translation(trackingAreaSize * 0.5, trackingAreaSize * 0.5 - wallThickness, 0.0)
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall3 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (180.0) * Trafo3d.Translation(0.0, trackingAreaSize * 0.5 - wallThickness, trackingAreaSize * 0.5)
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall4 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (90.0) * Trafo3d.Translation(-trackingAreaSize * 0.5, trackingAreaSize * 0.5 - wallThickness, 0.0)
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }

    let goalRoomGroundObject = 
        { defaultCollider with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0) * goalRoomOffset
            model = goalRoomGroundSg 
                        |> normalDiffuseEffect 
                        |> Sg.diffuseFileTexture' @"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_albedo_S.jpg" true
                        |> groundNormalMap
            tilingFactor = V2d(groundTilingFactor * goalAreaSize)
            collisionShape = Some ( V3d(goalAreaSize, wallThickness, goalAreaSize) |> BulletHelper.Shape.Box )
        }

    let goalRoomCeilingObject = 
        { defaultCollider with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Translation(0.0, goalAreaSize - 0.5 * wallThickness, 0.0) * goalRoomOffset
            model = goalRoomCeilingSg 
                        |> normalDiffuseEffect 
                        |> Sg.diffuseFileTexture' @"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_albedo_S.jpg" true
                        |> ceilingNormalMap
            tilingFactor = V2d(ceilingTilingFactor * goalAreaSize)
            collisionShape = Some ( V3d(goalAreaSize, wallThickness, goalAreaSize) |> BulletHelper.Shape.Box )
        }

    let goalRoomWallBase = 
        { defaultCollider with
            castsShadow = false
            model = goalRoomWallSg
                        |> normalDiffuseEffect
                        |> Sg.diffuseFileTexture' @"..\..\resources\textures\Brown Bricks\TexturesCom_Brown Bricks_albedo_S.jpg" true
                        |> goalRoomWallNormalMap
            tilingFactor = V2d(0.5 * goalAreaSize)
        }
    let goalRoomWall1 = 
        { goalRoomWallBase with 
            id = newId()
            trafo = Trafo3d.Translation(0.0, goalAreaSize * 0.5 - wallThickness, -goalAreaSize * 0.5) * goalRoomOffset
            collisionShape = Some ( V3d(goalAreaSize, goalAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let goalRoomWall2 = 
        { goalRoomWallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (-90.0) * Trafo3d.Translation(goalAreaSize * 0.5, goalAreaSize * 0.5 - wallThickness, 0.0) * goalRoomOffset
            collisionShape = Some ( V3d(goalAreaSize, goalAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let goalRoomWall3 = 
        { goalRoomWallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (180.0) * Trafo3d.Translation(0.0, goalAreaSize * 0.5 - wallThickness, goalAreaSize * 0.5) * goalRoomOffset
            collisionShape = Some ( V3d(goalAreaSize, goalAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let goalRoomWall4 = 
        { goalRoomWallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (90.0) * Trafo3d.Translation(-goalAreaSize * 0.5, goalAreaSize * 0.5 - wallThickness, 0.0) * goalRoomOffset
            collisionShape = Some ( V3d(goalAreaSize, goalAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }

    let ball = 
        { defaultCollider with
            id = newId()
            objectType = ObjectTypes.Dynamic
            isManipulable = true
            trafo = Trafo3d.Identity
            model = ballSg |> ballEffect |> Sg.diffuseFileTexture' @"..\..\resources\textures\basketball\balldimpled.png" true
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
            model = boxSg |> boxEffect |> Sg.diffuseFileTexture' @"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg" true |> wallNormalMap
            mass = 0.625f
            collisionShape = Some ( V3d(objectBoxEdgeLength) |> BulletHelper.Shape.Box )
            restitution = 0.1f
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
        }
    let headCollider = 
        { defaultCollider with
            castsShadow = false
            id = newId()
            objectType = ObjectTypes.Kinematic
            isManipulable = false
            trafo = Trafo3d.Identity
            model = Sg.ofList []
            collisionShape = Some (BulletHelper.Shape.Sphere 0.20)
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
        }

    let objects =
        let toObjects (canMove : bool) (l : list<_>) =
            l |> List.mapi (fun i (file, (trafo : Trafo3d), mass, shape, restitution) ->
                    let assimpScene : Loader.Scene = file |> Loader.Assimp.load 
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
                                //Some (BulletHelper.Box (bounds.Size))
                            | Some s ->
                                s

                    { defaultObject with
                        id = newId()
                        isManipulable = canMove
                        trafo = Trafo3d.Translation(bounds.Center)
                        model = sg 
                        mass = mass
                        collisionShape = collShape
                        restitution = restitution
                    }
                )

        let manipulableObjects = replicate ((toObjects true manipulableModels), 1)
        let ballObjects = replicate ([ball], 1)
        let boxObjects = replicate ([box], 5)
        
        manipulableObjects @ 
        ballObjects @ boxObjects @
        toObjects false staticModels 
        @ [groundObject; ceilingObject; wall1; wall3; wall4; lightObject]
        @ [goalRoomGroundObject; goalRoomCeilingObject; goalRoomWall1; goalRoomWall2; goalRoomWall3;]
        @ [leftHandObject; rightHandObject; camObject1; camObject2; headCollider]
        
    let sceneObj =
        {
            cam1ObjectId = camObject1.id
            cam2ObjectId = camObject2.id
            controller1ObjectId = leftHandObject.id
            controller2ObjectId = rightHandObject.id
            headId = headCollider.id
            objects = PersistentHashSet.ofList objects
            lightId = lightObject.id
            moveDirection = V3d.Zero
            viewTrafo = Trafo3d.Identity
            lastContr2Trafo = Trafo3d.Identity
            deviceOffset = Trafo3d.Identity
            deltaTime = 0.0
            enablePhysics = true
            interactionType = VrInteractions.VrInteractionTechnique.VirtualHand
            armExtensionFactor = 1.0
            gravity = V3d(0.0, -9.81, 0.0)
            physicsDebugDraw = false
            numSubSteps = 3
            subStepTime = 1.0 / 180.0
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
