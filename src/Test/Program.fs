// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module VRTestApp

open Valve.VR

open OpenTK
open OpenTK.Graphics

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.Rendering.GL
open Aardvark.Application.WinForms
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.Semantics
open Aardvark.VR

open LogicalScene

let rec triangles (trafo : Trafo3d) (m : IndexedGeometry) =
    let positions = m.IndexedAttributes.[DefaultSemantic.Positions] |> unbox<V3f[]>
    let index =
        if isNull m.IndexArray then Array.init positions.Length id
        else m.IndexArray |> unbox<int[]>

    match m.Mode with
        | IndexedGeometryMode.TriangleList ->
            Array.init (index.Length / 3) (fun ti ->
                Triangle3d(
                    trafo.Forward.TransformPos (V3d positions.[index.[3*ti+0]]),
                    trafo.Forward.TransformPos (V3d positions.[index.[3*ti+1]]),
                    trafo.Forward.TransformPos (V3d positions.[index.[3*ti+2]])
                )
            )
                
        | IndexedGeometryMode.TriangleStrip ->
            failwith ""

        | _ ->
            [||]

let rec createShape (currentTrafo : Trafo3d) (n : Loader.Node) : Triangle3d[] =
    match n with
        | Loader.Empty -> 
            [||]
        | Loader.Trafo(t,c) -> 
            createShape (t * currentTrafo) c
        | Loader.Group(children) ->
            children |> List.toArray |> Array.collect (createShape currentTrafo)
        | Loader.Leaf n ->
            triangles currentTrafo n.geometry

        | Loader.Material(_,c) ->
            createShape currentTrafo c

[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()

    use app = new OpenGlApplication()
    let vrWin = VrWindow.VrWindow(app.Runtime, true)

    let staticModels =
        [
            //@"C:\Aardwork\sponza\sponza.obj", Trafo3d.Scale 0.01, Mass.Infinite
        ]

    let manipulableModels =
        [
            //@"C:\Aardwork\Stormtrooper\Stormtrooper.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0), Mass 100.0f, None, 0.5
            //@"C:\Aardwork\witcher\geralt.obj", Trafo3d.Translation(0.0, 4.0, 0.0), Mass 80.0f, None, 0.5
            //@"C:\Aardwork\ironman\ironman.obj", Trafo3d.Scale 0.5 * Trafo3d.Translation(2.0, 0.0, 0.0), Mass 100.0f, None, 0.5
            //@"C:\Aardwork\lara\lara.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0), Mass 60.0f, None, 0.5
        ]
        
    let handBox = Box3d.FromCenterAndSize(V3d.OOO, 0.1 * V3d.III)
    let handSg = Sg.box (Mod.constant C4b.Green) (Mod.constant handBox) 
    let beamSg = Sg.lines (Mod.constant C4b.Red) (Mod.constant ( [| Line3d(V3d.OOO, -V3d.OOI * 100.0) |]) ) 
    let ballSg = Sg.sphere 10 (Mod.constant C4b.DarkYellow) (Mod.constant 0.1213)
    let groundSg = Sg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, 10.0 * V3d.III))) 
//    let groundSg = Sg.fullScreenQuad |> Sg.trafo (Mod.constant (Trafo3d.Scale 100.0 * Trafo3d.RotationX (float MathHelper.PiOver2)))

    let virtualHandEffect = Sg.effect [
                                DefaultSurfaces.trafo |> toEffect
                                DefaultSurfaces.uniformColor (LogicalScene.virtualHandColor) |> toEffect
                                DefaultSurfaces.simpleLighting |> toEffect
                            ]
    let handEffect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.White |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]
    let beamEffect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.vertexColor |> toEffect
                        DefaultSurfaces.thickLine |> toEffect
                    ]
    let groundEffect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.Gray80 |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]
    let wall1Effect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.Red |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]
    let wall2Effect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.Green |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]
    let wall3Effect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.Blue |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]
    let wall4Effect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.VRVisGreen |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]
    let ballEffect = Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor (C4f (0.586, 0.297, 0.172)) |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]

    let leftHandObject = 
        { defaultObject with
            id = newId()
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = Sg.ofList [handSg |> handEffect; beamSg |> beamEffect]
        }
    let rightHandObject = 
        { defaultObject with
            id = newId()
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = handSg |> virtualHandEffect
        }
    let camObject = 
        { defaultObject with
            id = newId()
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = handSg |> handEffect
        }
    let groundObject = 
        let edgeLength = 10.0
        { defaultObject with
            id = newId()
//            trafo = Trafo3d.Identity
//            model = groundSg |> groundEffect
//            collisionShape = Some ( Plane3d(V3d(0,1,0), V3d(0,0,0)) |> BulletHelper.Shape.Plane )
            trafo = Trafo3d.Translation(0.0, -edgeLength * 0.5, 0.0)
            model = groundSg |> groundEffect
            collisionShape = Some ( V3d(edgeLength) |> BulletHelper.Shape.Box )
            friction = 0.75
            restitution = 0.75
        }
    let wall1 = 
        { defaultObject with
            id = newId()
            friction = 0.75
            restitution = 0.75
            model = Sg.fullScreenQuad |> Sg.trafo (Mod.constant (Trafo3d.Scale 100.0 * Trafo3d.RotationY (float MathHelper.PiOver2) * Trafo3d.RotationX (float MathHelper.PiOver2) * Trafo3d.Translation(-5.0, 0.0, 0.0))) |> wall1Effect
            collisionShape = Some ( Plane3d(V3d(1,0,0), V3d(-5,0,0)) |> BulletHelper.Shape.Plane )
        }
    let wall2 = 
        { defaultObject with
            id = newId()
            friction = 0.75
            restitution = 0.75
            model = Sg.fullScreenQuad |> Sg.trafo (Mod.constant (Trafo3d.Scale 100.0 * Trafo3d.RotationY (float -MathHelper.PiOver2) * Trafo3d.RotationX (float MathHelper.PiOver2) * Trafo3d.Translation(5.0, 0.0, 0.0))) |> wall2Effect
            collisionShape = Some ( Plane3d(V3d(-1,0,0), V3d(5,0,0)) |> BulletHelper.Shape.Plane )
        }
    let wall3 = 
        { defaultObject with
            id = newId()
            friction = 0.75
            restitution = 0.75
            model = Sg.fullScreenQuad |> Sg.trafo (Mod.constant (Trafo3d.Scale 100.0 * Trafo3d.RotationX (float MathHelper.PiOver2) * Trafo3d.RotationX (float MathHelper.PiOver2) * Trafo3d.Translation(0.0, 0.0, -5.0))) |> wall3Effect
            collisionShape = Some ( Plane3d(V3d(0,0,1), V3d(0,0,-5)) |> BulletHelper.Shape.Plane )
        }
    let wall4 = 
        { defaultObject with
            id = newId()
            friction = 0.75
            restitution = 0.75
            model = Sg.fullScreenQuad |> Sg.trafo (Mod.constant (Trafo3d.Scale 100.0 * Trafo3d.RotationX (float -MathHelper.PiOver2) * Trafo3d.RotationX (float MathHelper.PiOver2) * Trafo3d.Translation(0.0, 0.0, 5.0))) |> wall4Effect
            collisionShape = Some ( Plane3d(V3d(0,0,-1), V3d(0,0,5)) |> BulletHelper.Shape.Plane )
        }
    let ball = 
        { defaultObject with
            id = newId()
            isManipulable = true
            boundingBox = Box3d.FromCenterAndSize(V3d.Zero, V3d(0.2426))
            trafo = Trafo3d.Translation(2.0, 0.0, 0.0)
            model = ballSg |> ballEffect
            mass = Mass 0.625f
            collisionShape = Some (BulletHelper.Shape.Sphere 0.1213)
            restitution = 0.85
            friction = 0.75
            CcdSpeedThreshold = 0.2
            CcdSphereRadius = 1.2
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
                            |> Sg.transform (trafo * Trafo3d.Translation(-bounds.Center))

                    let collShape = 
                        match shape with
                            | None ->
                                //Some (triangles |> BulletHelper.TriangleMesh)
                                Some (BulletHelper.Box (bounds.Size))
                            | Some s ->
                                s

                    { defaultObject with
                        id = newId()
                        isManipulable = canMove
                        boundingBox = Box3d.FromCenterAndSize(V3d.Zero, bounds.Size)
                        trafo = Trafo3d.Translation(bounds.Center)
                        model = sg 
                        mass = mass
                        collisionShape = collShape
                        restitution = restitution
                    }
                )

        let replicate (objects : Object list, amount : int) = 
            [
                for (o) in objects do
                    for i in 1..amount do
                        let offset = Trafo3d.Translation(0.0, float i*2.5, 0.0)
                        yield ({o with 
                                    id = newId()
                                    trafo = offset * o.trafo})
            ]

        let manipulableObjects = replicate ((toObjects true manipulableModels), 1)
        let ballObjects = replicate ([ball], 1)
        
        manipulableObjects @ 
        ballObjects @
        toObjects false staticModels 
        @  [groundObject; wall1; wall2; wall3; wall4]
        
    let sceneObj =
        {
            cam1Object = camObject
            cam2Object = camObject
            controller1Object = leftHandObject
            controller2Object = rightHandObject
            objects = PersistentHashSet.ofList objects
            moveDirection = V3d.Zero
            viewTrafo = Trafo3d.Identity
            lastViewTrafo = Trafo3d.Identity
            interactionType = VrInteractions.VrInteractionTechnique.VirtualHand
            gravity = V3d(0.0, -9.81, 0.0)
            physicsDebugDraw = true
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
