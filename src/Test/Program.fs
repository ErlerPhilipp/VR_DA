﻿// Learn more about F# at http://fsharp.org
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

// Hello World test
//    VRTest.VRTest.helloWorld () |> ignore
//    System.Environment.Exit 0

    use app = new OpenGlApplication()
    
    let vrWin = VrWindow(app.Runtime, true)

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
        
    let handBox = Box3d.FromCenterAndSize(V3d.OOO, 0.1 * V3d.III)
    let handSg = Sg.box (Mod.constant C4b.Green) (Mod.constant handBox) 
    let beamSg = Sg.lines (Mod.constant C4b.Red) (Mod.constant ( [| Line3d(V3d.OOO, -V3d.OOI * 100.0) |]) ) 
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

    let leftHandObject : Object = 
        {
            id = newId()
            canMove = false
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = Sg.ofList [handSg |> handEffect; beamSg |> beamEffect]
        }
    let rightHandObject : Object = 
        {
            id = newId()
            canMove = false
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = handSg |> handEffect
        }

    let objects =
        let toObjects (canMove : bool) (l : list<_>) =
            l |> List.mapi (fun i (file, trafo) ->
                    let scene = file |> Loader.Assimp.load 
                    let sg = scene |> Sg.AdapterNode :> ISg
                    {
                        id = newId()
                        canMove = canMove
                        boundingBox = scene.bounds
                        trafo = trafo
                        model = sg
                    }
                )

        toObjects true manipulableModels @ 
        toObjects false staticModels
        
    let getControllerObject (i : int) =
        match i with
            | 0 -> None // hmd
            | 1 -> Some rightHandObject // cam 1
            | 2 -> Some rightHandObject // cam 2
            | 3 -> Some leftHandObject // controller 1
            | 4 -> Some rightHandObject // controller 2
            | _ -> None // error

    let sceneObj =
        {
            controllerObjects = List.init VrDriver.devices.Length getControllerObject
            activeObjects = PersistentHashSet.empty
            things = PersistentHashSet.ofList objects
            moving = false
            moveDirection = V3d.Zero
            viewTrafo = Trafo3d.Identity
            lastTrafo = Trafo3d.Identity
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
        //  |> Sg.andAlso debugStuff
            |> Sg.projTrafo vrWin.Projection
            |> Sg.uniform "LineWidth" (Mod.constant 5.0)
            |> Sg.uniform "ViewportSize" (Mod.constant VrDriver.desiredSize)

 
    let task = app.Runtime.CompileRender(vrWin.FramebufferSignature, vrSg)
    vrWin.RenderTask <- task
    
    match VrDriver.devices.Length with
        | 0 -> printf "Found no device!\n"
        | 1 -> printf "Found only 1 device!\n"
        | 2 -> printf "Found only 1 controller!\n"
        | _ -> () //printf "Found all devices.\n"
    //printfn "number of devices: %A" VrDriver.devices.Length

    vrWin.Run()

    OpenVR.Shutdown()

    0
