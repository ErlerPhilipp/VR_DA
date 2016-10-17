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

[<EntryPoint>]
let main argv =
    
    Ag.initialize()
    Aardvark.Init()

// Hello World test
//    VRTest.VRTest.helloWorld () |> ignore
//    System.Environment.Exit 0

    use app = new OpenGlApplication()

    let win = VrWindow(app.Runtime, 1)

    let box = Sg.box' C4b.Red Box3d.Unit
        
    let flip = Trafo3d.FromBasis(V3d.IOO, -V3d.OOI, V3d.OIO, V3d.Zero)
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
                        let! dt = differentiate win.Time
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
    let camera = DefaultCameraController.control win.Mouse win.Keyboard win.Time initial



    let scene = 
        [ for x in -5 .. 5 do
            for y in -5 .. 5 do
                for z in -5 .. 5 do
                    if x <> 0 && y <> 0 && z <> 0 then
                        yield Sg.translate (2.0 * float x) (2.0 * float y) (2.0 * float z) box        
            yield debugStuff
        ] 
            |> Sg.ofList
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.simpleLighting |> toEffect
            ]
    
    let bla =
        Sg.box (Mod.constant C4b.Green) (Mod.constant <| Box3d.FromMinAndSize(V3d.Zero,V3d.III))
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
            ]

    let trafo = Mod.map2 (*) moveTrafo win.View
    let lodTrafo = Mod.map2 (*) moveTrafo win.LodView

    let scene = 
        PointCloud.scene lodTrafo trafo win app.Runtime
            |> Sg.andAlso debugStuff

    let models =
        [
            @"C:\Aardwork\sponza\sponza.obj", Trafo3d.Scale 0.01
            @"C:\Aardwork\witcher\geralt.obj", Trafo3d.Translation(0.0, 0.0, 1.0)
            @"C:\Aardwork\ironman\ironman.obj", Trafo3d.Scale 0.5 * Trafo3d.Translation(2.0, 0.0, 0.0)
            //@"C:\Aardwork\Stormtrooper\Stormtrooper.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0)
            @"C:\Aardwork\lara\lara.dae", Trafo3d.Scale 0.5 * Trafo3d.Translation(-2.0, 0.0, 0.0)
        ]

//    
    let scene =
        let flip = Trafo3d.FromBasis(V3d.IOO, V3d.OOI, -V3d.OIO, V3d.Zero)

        models
            |> List.map (fun (file, trafo) -> file |> Loader.Assimp.load |> Sg.AdapterNode |> Sg.transform (flip * trafo))
            |> Sg.ofList
            |> Sg.andAlso debugStuff
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.constantColor C4f.White |> toEffect
                DefaultSurfaces.diffuseTexture |> toEffect
                DefaultSurfaces.normalMap |> toEffect
                DefaultSurfaces.lighting false |> toEffect
            ]


    let sg = 
        scene
            //|> Sg.trafo moveTrafo
            |> Sg.viewTrafo trafo //win.View
            //|> Sg.viewTrafo (Mod.constant Trafo3d.Identity)
            |> Sg.projTrafo win.Projection
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)
    win.RenderTask <- task
    win.Run()

    //printfn "%A" hasDevice
    OpenVR.Shutdown()

    0 // return an integer exit code
