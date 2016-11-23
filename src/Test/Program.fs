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
    
    let trackingAreaSize = 5.0
    let wallThickness = 1.0

    let handBoxEdgeLength = 0.1
    let handBox = Box3d.FromCenterAndSize(V3d.OOO, handBoxEdgeLength * V3d.III)
    let handSg = Primitives.box (Mod.constant C4b.Green) (Mod.constant handBox) 
    let beamSg = Sg.lines (Mod.constant C4b.Red) (Mod.constant ( [| Line3d(V3d.OOO, -V3d.OOI * 100.0) |]) ) 
    let ballSg = Sg.sphere 6 (Mod.constant C4b.DarkYellow) (Mod.constant 0.1213)
    let groundSg = Primitives.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
    let wallSg = Primitives.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, trackingAreaSize, wallThickness))))
    let lightSg = Sg.sphere 6 (Mod.constant C4b.White) (Mod.constant 0.1) 
    
    let camBox = Box3d.FromCenterAndSize(V3d.OOO, 0.15 * V3d.III)

    let objectBoxEdgeLength = 0.25
    let objectBox = Box3d.FromCenterAndSize(V3d.OOO, objectBoxEdgeLength * V3d.III)
    let boxSg = Primitives.box (Mod.constant C4b.Green) (Mod.constant objectBox)

    let groundNormalSampler = (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_normalmap_S.jpg", true) :> ITexture))
    let groundNormalMap = Sg.texture DefaultSemantic.NormalMapTexture groundNormalSampler

    let wallNormalSampler = (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_normalmap_S.jpg", true) :> ITexture))
    let wallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture wallNormalSampler

    let virtualHandEffect = Sg.effect [
                                DefaultSurfaces.trafo |> toEffect
                                DefaultSurfaces.uniformColor (LogicalScene.virtualHandColor) |> toEffect
                                DefaultSurfaces.simpleLighting |> toEffect
                            ]
    let constColorEffect = Sg.effect [
                                DefaultSurfaces.trafo |> toEffect
                                DefaultSurfaces.constantColor C4f.White |> toEffect
                                DefaultSurfaces.simpleLighting |> toEffect
                            ]
    let beamEffect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.vertexColor |> toEffect
                        DefaultSurfaces.thickLine |> toEffect
                    ]
    let normalDiffuseEffect = Sg.effect [
                                    DefaultSurfaces.trafo |> toEffect
                                    TextureTiling.Effect
                                    NormalMap.Effect
                                    DefaultSurfaces.diffuseTexture |> toEffect
                                    Lighting.Effect false
                                ]
    let ballEffect = Sg.effect [
                        SphereTexture.vertex |> toEffect
                        DefaultSurfaces.trafo |> toEffect
                        SphereTexture.fragment |> toEffect
                        DefaultSurfaces.diffuseTexture |> toEffect
                        Lighting.Effect true
                        highlight |> toEffect
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
            trafo = Trafo3d.Translation(0.5 * trackingAreaSize - wallThickness - 0.5, trackingAreaSize - wallThickness, 0.0)
            model = lightSg |>  constColorEffect
        }

    let commonRestitution = 0.95f
    let commonRollingFriction = 0.01f

    let groundObject = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0)
            model = groundSg 
                        |> normalDiffuseEffect 
                        |> Sg.diffuseFileTexture' @"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_albedo_S.jpg" true
                        |> groundNormalMap
            tilingFactor = V2d(4.0, 4.0)
            collisionShape = Some ( V3d(trackingAreaSize, wallThickness, trackingAreaSize) |> BulletHelper.Shape.Box )
            friction = 0.75f
            rollingFriction = commonRollingFriction
            restitution = commonRestitution
        }

    let wallBase = 
        { defaultObject with
            friction = 0.75f
            rollingFriction = commonRollingFriction
            restitution = commonRestitution
            model = wallSg
                        |> normalDiffuseEffect
                        |> Sg.diffuseFileTexture' @"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg" true
                        |> wallNormalMap
            tilingFactor = V2d(3.0, 3.0)
        }
    let wall1 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.Translation(0.0, trackingAreaSize * 0.5 - wallThickness, -trackingAreaSize * 0.5)
            model = wallBase.model
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall2 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationY (float -MathHelper.PiOver2) * Trafo3d.Translation(trackingAreaSize * 0.5, trackingAreaSize * 0.5 - wallThickness, 0.0)
            model = wallBase.model
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall3 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationY (float MathHelper.Pi) * Trafo3d.Translation(0.0, trackingAreaSize * 0.5 - wallThickness, trackingAreaSize * 0.5)
            model = wallBase.model
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let wall4 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationY (float MathHelper.PiOver2) * Trafo3d.Translation(-trackingAreaSize * 0.5, trackingAreaSize * 0.5 - wallThickness, 0.0)
            model = wallBase.model
            collisionShape = Some ( V3d(trackingAreaSize, trackingAreaSize, wallThickness) |> BulletHelper.Shape.Box )
        }
    let ball = 
        { defaultObject with
            id = newId()
            objectType = ObjectTypes.Dynamic
            isManipulable = true
            trafo = Trafo3d.Identity
            model = ballSg |> ballEffect |> Sg.diffuseFileTexture' @"..\..\resources\textures\basketball\Basketball texture.jpg" true
            mass = 0.625f
            collisionShape = Some (BulletHelper.Shape.Sphere 0.1213)
            restitution = commonRestitution
            friction = 0.75f
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
            rollingFriction = commonRollingFriction
        }
    let box = 
        { defaultObject with
            id = newId()
            objectType = ObjectTypes.Dynamic
            isManipulable = true
            trafo = Trafo3d.Translation(-0.5, 0.0, 0.0)
            model = boxSg |> ballEffect |> Sg.diffuseFileTexture' @"..\..\resources\textures\basketball\Basketball texture.jpg" true
            mass = 0.625f
            collisionShape = Some ( V3d(objectBoxEdgeLength) |> BulletHelper.Shape.Box )
            restitution = 0.1f
            friction = 0.75f
            ccdSpeedThreshold = 0.1f
            ccdSphereRadius = 0.5f
            rollingFriction = commonRollingFriction
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
        let ballObjects = replicate ([ball], 25)
        let boxObjects = replicate ([box], 25)
        
        manipulableObjects @ 
        ballObjects @ boxObjects @
        toObjects false staticModels 
        @ [groundObject; wall1; wall2; wall3; wall4; lightObject]
        @ [leftHandObject; rightHandObject; camObject1; camObject2;]
        
    let sceneObj =
        {
            cam1ObjectId = camObject1.id
            cam2ObjectId = camObject2.id
            controller1ObjectId = leftHandObject.id
            controller2ObjectId = rightHandObject.id
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
//            numSubSteps = 11
//            subStepTime = 1.0 / 900.0
            numSubSteps = 3
            subStepTime = 1.0 / 180.0
//            numSubSteps = 1
//            subStepTime = 1.0 / 90.0
        }

    let scene =
        GraphicsScene.createScene sceneObj vrWin
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.diffuseTexture |> toEffect
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
