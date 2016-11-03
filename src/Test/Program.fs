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

// mod bind example:
//let flag = Mod.init true
//let heavyComp = Mod.init 20
//let easyComp = Mod.init 10
//let switchComp = Mod.bind (fun flagValue -> if flagValue then heavyComp else easyComp) flag

//    let m = firstObj.transformedBB.GetValue()
//
//    let transformedBox = Mod.map (fun (t : Trafo3d) -> firstObj.model.bounds.Transformed(t)) firstObj.trafo
//
//    transact (fun () -> firstObj.trafo.Value <- Trafo3d.Translation(V3d.OOO))
//    transact (fun () -> Mod.change firstObj.trafo (Trafo3d.Translation(V3d.OOO)))


[<EntryPoint>]
let main argv =
    Ag.initialize()
    Aardvark.Init()

    use app = new OpenGlApplication()
    let vrWin = VrWindow(app.Runtime, true)

    let staticModels =
        [
            //@"C:\Aardwork\sponza\sponza.obj", Trafo3d.Scale 0.01, Mass.Infinite
        ]

    let manipulableModels =
        [
            @"C:\Aardwork\witcher\geralt.obj", Trafo3d.Translation(0.0, 4.0, 0.0), Mass 80.0f
            @"C:\Aardwork\ironman\ironman.obj", Trafo3d.Scale 0.5 * Trafo3d.Translation(2.0, 0.0, 0.0), Mass 100.0f
            //@"C:\Aardwork\Stormtrooper\Stormtrooper.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0)
            @"C:\Aardwork\lara\lara.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0), Mass 60.0f
        ]
        
    let handBox = Box3d.FromCenterAndSize(V3d.OOO, 0.1 * V3d.III)
    let handSg = Sg.box (Mod.constant C4b.Green) (Mod.constant handBox) 
    let beamSg = Sg.lines (Mod.constant C4b.Red) (Mod.constant ( [| Line3d(V3d.OOO, -V3d.OOI * 100.0) |]) ) 
    let virtualHandEffect = Sg.effect [
                                DefaultSurfaces.trafo |> toEffect
                                DefaultSurfaces.uniformColor (MutableScene.virtualHandColor) |> toEffect
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

    let leftHandObject : Object = 
        {
            id = newId()
            isManipulable = false
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = Sg.ofList [handSg |> handEffect; beamSg |> beamEffect]
            collisionShape = None
            mass = Infinite
        }
    let rightHandObject : Object = 
        {
            id = newId()
            isManipulable = false
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = handSg |> virtualHandEffect
            collisionShape = None
            mass = Infinite
        }
    let camObject : Object = 
        {
            id = newId()
            isManipulable = false
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = handSg |> handEffect
            collisionShape = None
            mass = Infinite
        }
    let groundObject : Object = 
        {
            id = newId()
            isManipulable = false
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = Sg.fullScreenQuad |> Sg.trafo (Mod.constant (Trafo3d.Scale 100.0 * Trafo3d.RotationX (float MathHelper.PiOver2))) |> groundEffect

            collisionShape = Some ( Plane3d(V3d(0,1,0), V3d(0,0,0)) |> Shape.Plane )
            mass = Infinite
        }


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


    let objects =
        let toObjects (canMove : bool) (l : list<_>) =
            l |> List.mapi (fun i (file, (trafo : Trafo3d), mass) ->
                    let assimpScene : Loader.Scene = file |> Loader.Assimp.load 
                    let triangles = createShape trafo assimpScene.root
                    let bounds = triangles |> Seq.collect (fun t -> [t.P0; t.P1; t.P2]) |> Box3d

                    let sg = 
                        assimpScene 
                            |> Sg.AdapterNode :> ISg 
                            |> Sg.transform (trafo * Trafo3d.Translation(-bounds.Center))

                    {
                        id = newId()
                        isManipulable = canMove
                        boundingBox = Box3d.FromCenterAndSize(V3d.Zero, bounds.Size)
                        trafo = Trafo3d.Translation(bounds.Center)
                        model = sg 
                        mass = mass
                        collisionShape = Some (Box (bounds.Size))
                    }
                )

        toObjects true manipulableModels @ 
        toObjects false staticModels 
        @  [groundObject]
        

    let sceneObj =
        {
            cam1Object = camObject
            cam2Object = camObject
            controller1Object = leftHandObject
            controller2Object = rightHandObject
            activeObjects = PersistentHashSet.empty
            things = PersistentHashSet.ofList objects
            moveDirection = V3d.Zero
            viewTrafo = Trafo3d.Identity
            lastTrafo = Trafo3d.Identity
            interactionType = VrInteractionTechnique.VirtualHand
        }


    let scene =
        MutableScene.createScene sceneObj vrWin
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
    
    match VrDriver.devices.Length with
        | 0 -> printfn "Found no device!"
        | 1 -> printfn "Found only 1 device!"
        | 2 -> printfn "Found only 1 controller!"
        | _ -> () //printfn "Found all %A devices." VrDriver.devices.Length

    vrWin.Run()

    OpenVR.Shutdown()

    0
