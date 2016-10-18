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

type Object = 
    {
        trafo   : ModRef<Trafo3d>
        model   : Loader.Scene
    }

type Scene = 
    {
        objects : list<Object>
        staticGeometry : list<Object>
    }

let mutable grappedObj : Option<Object> = None

let respondToSensors (sensors : unit) (scene : Scene) =
    // grab sensors,
    // compute trafos
    // modify scene objects
    let firstObj = scene.objects.[0]
    let objectInWorldSpace = firstObj.model.bounds.Transformed(firstObj.trafo.Value)

    // logic happens
    //


    let x = 
        if false then 
            transact (fun () -> firstObj.trafo.Value <- Trafo3d.Translation(V3d.OOO)) 
            1
        else 
            299


    transact (fun () -> firstObj.trafo.Value <- Trafo3d.Translation(V3d.OOO))

    ()


let makeSceneGraph (s : Scene) : ISg =
        
    let flip = Trafo3d.FromBasis(V3d.IOO, V3d.OOI, -V3d.OIO, V3d.Zero)

    let objects = 
        List.concat [s.objects; s.staticGeometry]
        |> List.map (fun o -> 
                let scene = o.model |> Sg.AdapterNode
                let transformed = o.trafo |> Mod.map (fun t -> flip * t) 
                Sg.trafo transformed scene
           )
        |> Sg.ofSeq
    objects
    

[<EntryPoint>]
let main argv =
    
    Ag.initialize()
    Aardvark.Init()

// Hello World test
//    VRTest.VRTest.helloWorld () |> ignore
//    System.Environment.Exit 0

    use app = new OpenGlApplication()
    
    let vrWin = VrWindow(app.Runtime, 1)
    let screenWin = app.CreateSimpleRenderWindow()

    let box = Sg.box' C4b.Red Box3d.Unit

    //let firstController = VrDriver.devices |> Array.find (fun c -> c.Type = VrDeviceType.Controller)
    //let trafo = firstController.DeviceToWorld

    let controllers = VrDriver.devices |> Array.toList |> List.filter (fun c -> c.Type = VrDeviceType.Controller)

    let moveController =
        controllers |> List.map (fun c -> 
            controller {
                let! state = c.Axis.[1].Position

                match state with
                    | Some dir -> 
                        let speed = dir.X
                        let! dt = differentiate vrWin.Time
                        return fun (t : Trafo3d) ->
                            let forward = c.DeviceToWorld.GetValue().Forward.TransformDir(V3d.OOI)
                            t * Trafo3d.Translation(forward * speed * 8.0 * dt.TotalSeconds)
                    | _ ->
                        ()
            }
        ) |> AFun.chain

    let moveTrafo = AFun.integrate moveController Trafo3d.Identity
    let moveTrafoInv = moveTrafo |> Mod.map (fun t -> t.Inverse)

    

    let controllerStuff =
        controllers |> List.map (fun c ->

            let finalHandTrafo = Mod.map2 (*) c.DeviceToWorld moveTrafoInv

            let controllerBox = 
                Sg.box (Mod.constant C4b.Green) (Mod.constant <| Box3d.FromCenterAndSize(V3d.OOO,V3d.III))
                    |> Sg.scale 0.1
                    |> Sg.trafo finalHandTrafo

            let beam = 
                Sg.lines (Mod.constant C4b.Red) (finalHandTrafo |> Mod.map (fun d -> 
                        let origin = d.Forward.TransformPos(V3d.OOO)
                        let target = origin + d.Forward.TransformDir(-V3d.OOI) * 100.0
                        [| Line3d(origin,target) |]) 
                ) 

            Sg.ofList [
                controllerBox
                    |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.constantColor C4f.White |> toEffect
                        DefaultSurfaces.simpleLighting |> toEffect
                    ]
                beam
                    |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.vertexColor |> toEffect
                        DefaultSurfaces.thickLine |> toEffect
                    ]
            ]

        )

    let lines =
        Sg.ofList [
            Sg.lines (Mod.constant C4b.Red) (Mod.constant [|Line3d(V3d.OOO, V3d.IOO)|])
            Sg.lines (Mod.constant C4b.Green) (Mod.constant [|Line3d(V3d.OOO, V3d.OIO)|])
            Sg.lines (Mod.constant C4b.Blue) (Mod.constant [|Line3d(V3d.OOO, V3d.OOI)|])
           
        ]

    let debugStuff =
        Sg.ofList [
            yield lines
                |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.vertexColor |> toEffect
                ]

            yield! controllerStuff
           
        ]

    let initial = CameraView.lookAt V3d.Zero V3d.IOO V3d.OOI
    let camera = DefaultCameraController.control vrWin.Mouse vrWin.Keyboard vrWin.Time initial
    
    let bla =
        Sg.box (Mod.constant C4b.Green) (Mod.constant <| Box3d.FromMinAndSize(V3d.Zero,V3d.III))
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
            ]

    let viewTrafo = Mod.map2 (*) moveTrafo vrWin.View
    let lodTrafo = Mod.map2 (*) moveTrafo vrWin.LodView
    
    let staticModels =
        [
            @"C:\Aardwork\sponza\sponza.obj", Trafo3d.Scale 0.01
        ]

    let manipulableModels =
        [
            @"C:\Aardwork\witcher\geralt.obj", Trafo3d.Translation(0.0, 0.0, 1.0)
            @"C:\Aardwork\ironman\ironman.obj", Trafo3d.Scale 0.5 * Trafo3d.Translation(2.0, 0.0, 0.0)
            //@"C:\Aardwork\Stormtrooper\Stormtrooper.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0)
            @"C:\Aardwork\lara\lara.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0)
        ]

    let myScene : Scene = {
        objects = staticModels |> List.map (fun (file, initialTrafo) -> {model = file |> Loader.Assimp.load; trafo = ModRef<Trafo3d>(initialTrafo)});
        staticGeometry = manipulableModels |> List.map (fun (file, initialTrafo) -> {model = file |> Loader.Assimp.load; trafo = ModRef<Trafo3d>(initialTrafo)});
    }

    let scene =
        
        myScene
            |> makeSceneGraph
            |> Sg.andAlso debugStuff
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.diffuseTexture |> toEffect
                DefaultSurfaces.normalMap |> toEffect
                DefaultSurfaces.lighting false |> toEffect
            ]
            
    let frustum = screenWin.Sizes |> Mod.map(fun s -> Frustum.perspective 60.0 0.01 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
            

    let vrSg = 
        scene
            |> Sg.viewTrafo viewTrafo
            |> Sg.projTrafo vrWin.Projection
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)

    let screenSg = 
        scene
            |> Sg.viewTrafo viewTrafo
            |> Sg.projTrafo frustum
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)
 
    let task = app.Runtime.CompileRender(vrWin.FramebufferSignature, vrSg)
    vrWin.RenderTask <- task

    let screenTask = app.Runtime.CompileRender(screenWin.FramebufferSignature, screenSg)
    screenWin.RenderTask <- screenTask

    vrWin.Run()
    screenWin.Run()

    printfn "shutting down"
    //printfn "%A" hasDevice
    OpenVR.Shutdown()

    0 // return an integer exit code
