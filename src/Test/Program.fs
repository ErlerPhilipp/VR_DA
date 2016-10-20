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

type pset<'a> = PersistentHashSet<'a>
type Object =
    {
        id              : int
        canMove         : bool
        boundingBox     : Box3d
        trafo           : Trafo3d
        model           : ISg
    }

type Scene =
    {
        activeObjects     : pset<Object>
        things            : pset<Object>
        viewTrafo         : Trafo3d
        lastTrafo         : Trafo3d
        controllerObjects : list<Option<Object>>

    }
    
let mutable currentId = 0
let newId() = 
    currentId <- currentId + 1
    currentId

module MutableScene =

    type MObject =
        {
            mutable original : Object
            mtrafo : ModRef<Trafo3d>
            mmodel : ModRef<ISg>
        }

    type MScene =
        {
            mutable original : Scene
            mactiveObjects : cset<MObject>
            mthings : cset<MObject>
            mviewTrafo : ModRef<Trafo3d>
            mcontrollerObjects : array<ModRef<Option<MObject>>>
        }

    type Conversion private() =
        static member Create(o : Object) =
            {
                original = o
                mtrafo = Mod.init o.trafo
                mmodel = Mod.init o.model
            }

        static member Create(s : Scene) =
            {
                original = s
                mactiveObjects = CSet.ofSeq (PersistentHashSet.toSeq s.activeObjects |> Seq.map Conversion.Create)
                mthings = CSet.ofSeq (PersistentHashSet.toSeq s.things |> Seq.map Conversion.Create)
                mviewTrafo = Mod.init s.viewTrafo
                mcontrollerObjects = s.controllerObjects |> List.toArray |> Array.map (fun o -> o |> Option.map Conversion.Create |> Mod.init)
            }

        static member Update(m : MObject, o : Object) =
            if not (System.Object.ReferenceEquals(m.original, o)) then
                m.original <- o
                m.mmodel.Value <- o.model
                m.mtrafo.Value <- o.trafo

        static member Update(m : MScene, s : Scene) =
            if not (System.Object.ReferenceEquals(m.original, s)) then
                m.original <- s

                m.mviewTrafo.Value <- s.viewTrafo
                
                let controllers = s.controllerObjects |> List.toArray
                for i in 0 .. m.mcontrollerObjects.Length-1 do
                    let target = m.mcontrollerObjects.[i]
                    match controllers.[i] with
                        | Some d ->
                            match target.Value with
                                | Some ctrl -> Conversion.Update(ctrl, d)
                                | None -> target.Value <- Some (Conversion.Create d)
                        | None ->
                            target.Value <- None

                let table = 
                    Seq.append m.mthings m.mactiveObjects |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                
                for t in PersistentHashSet.toSeq s.things do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t)
                        | _ ->
                            let mo = Conversion.Create(t)
                            m.mthings.Add mo |> ignore
                

                for t in PersistentHashSet.toSeq s.activeObjects do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t)
                        | _ ->
                            let mo = Conversion.Create(t)
                            m.mactiveObjects.Add mo |> ignore
                    ()
                
                m.mactiveObjects.ExceptWith table.Values
            

    type Message =
        | Add of Object
        | Remove of Object
        | DevicePress of int * int * Trafo3d
        | DeviceRelease of int * int * Trafo3d
        | DeviceMove of int * Trafo3d
        | TimeElapsed of System.TimeSpan
        | UpdateViewTrafo of Trafo3d

    let change (i : int) (f : 'a -> 'a) (l : list<'a>) =
        l |> List.mapi (fun ii v -> if ii = i then f v else v)

    let update (scene : Scene) (message : Message) : Scene =
        match message with
            | TimeElapsed _ | UpdateViewTrafo _ | DeviceMove _ -> ()
            | _ -> printfn "%A" message

        let scene =
            match message with
                | DeviceMove(d, t) ->
                    { scene with 
                        controllerObjects = scene.controllerObjects |> change d (fun o -> 
                            match o with
                                | Some o -> Some { o with trafo = t }
                                | None -> None
                        )
                    }
                | _ -> 
                    scene

        match message with
            | DevicePress(4, _, t)  ->
                let worldLocation = t.Forward.C3.XYZ

                let pickedObjs = 
                    scene.things 
                        |> PersistentHashSet.toList 
                        |> List.choose (fun o -> 
                            if o.canMove then
                                let modelLocation = o.trafo.Backward.TransformPos worldLocation
                                if o.boundingBox.Contains modelLocation then
                                    Some o
                                else
                                    None
                            else
                                None
                            ) 
                        |> PersistentHashSet.ofList

                if PersistentHashSet.isEmpty pickedObjs then
                    scene
                else
                    { scene with 
                        lastTrafo       = t
                        activeObjects   = PersistentHashSet.union scene.activeObjects pickedObjs
                        things          = PersistentHashSet.difference scene.things pickedObjs
                    }

            | DeviceMove(_, t) ->
                if PersistentHashSet.isEmpty scene.activeObjects then
                    scene
                else    
                    let deltaTrafo = scene.lastTrafo.Inverse * t
                    { scene with 
                        activeObjects =
                            scene.activeObjects |> PersistentHashSet.map (fun a ->
                                { a with trafo = a.trafo * deltaTrafo }
                            ) 
                        lastTrafo = t
                    }

            | DeviceRelease(4, _, _) ->
                { scene with 
                    activeObjects = PersistentHashSet.empty
                    things = PersistentHashSet.union scene.activeObjects scene.things 
                    lastTrafo = Trafo3d.Identity
                }

            | TimeElapsed(dt) ->
                scene // do cam

            | UpdateViewTrafo trafo -> 
                { scene with viewTrafo = trafo }

            | _ ->
                scene


    let createScene (initialScene : Scene) (win : NewVrStuff.VrWindow) =
        let mutable scene = initialScene
        let mscene = Conversion.Create initialScene

        let perform (msg : Message) =
            transact (fun () ->
                scene <- update scene msg
                Conversion.Update(mscene, scene)
            )

        let deviceCount = VrDriver.devices.Length
        let oldTrafos = Array.zeroCreate deviceCount
        let update (dt : System.TimeSpan) (trafos : Trafo3d[]) (e : VREvent_t) =
            perform (TimeElapsed dt)

            //printfn "number of devices: %A" VrDriver.devices.Length
            
            for i in 0 .. VrDriver.devices.Length-1 do
                let t = trafos.[i]
                if oldTrafos.[i] <> t then
                    oldTrafos.[i] <- t
                    if i = 0 then 
                        perform (UpdateViewTrafo(t.Inverse)) 
                    else 
                        perform (DeviceMove(i, t))
                
            if e.trackedDeviceIndex >= 0u && e.trackedDeviceIndex < uint32 deviceCount then
                let deviceId = e.trackedDeviceIndex |> int
                let button = int e.data.controller.button |> unbox<EVRButtonId>
                let axis = button - EVRButtonId.k_EButton_Axis0 |> int
                let trafo = trafos.[deviceId]

                match unbox<EVREventType> (int e.eventType) with
                    | EVREventType.VREvent_ButtonPress -> perform(DevicePress(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonUnpress -> perform(DeviceRelease(deviceId, axis, trafo))
                    | _ -> ()

            ()

        win.Update <- update

        let toSg (t : MObject) =
            t.mmodel
                |> Sg.dynamic
                |> Sg.trafo t.mtrafo

        let objects = 
            mscene.mcontrollerObjects |> ASet.ofArray |> ASet.chooseM (fun m -> m :> IMod<_>) |> ASet.map toSg
                |> Sg.set
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.simpleLighting
                   }

        let sgs = 
            ASet.union' [mscene.mthings; mscene.mactiveObjects]
                |> ASet.map toSg
                |> Sg.set

        Sg.ofList [sgs; objects]
            |> Sg.viewTrafo mscene.mviewTrafo

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
//    let beam = 
//        Sg.lines (Mod.constant C4b.Red) (finalHandTrafo |> Mod.map (fun d -> 
//                let origin = V3d.OOO
//                let target = origin + d.Forward.TransformDir(-V3d.OOI) * 100.0
//                [| Line3d(origin,target) |]) 
//        ) 

    let leftHandObject : Object = 
        {
            id = newId()
            canMove = false
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = handSg
        }
    let rightHandObject : Object = 
        {
            id = newId()
            canMove = false
            boundingBox = handBox
            trafo = Trafo3d.Identity
            model = handSg
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
        
//    let controllerStuff =
//        controllers |> List.map (fun c ->
//
//            let finalHandTrafo = Mod.map2 (*) c.DeviceToWorld moveTrafoInv
//
//            let controllerBox = 
//                Sg.box (Mod.constant C4b.Green) (Mod.constant <| Box3d.FromCenterAndSize(V3d.OOO,V3d.III))
//                    |> Sg.scale 0.1
//                    |> Sg.trafo finalHandTrafo
//
//            let beam = 
//                Sg.lines (Mod.constant C4b.Red) (finalHandTrafo |> Mod.map (fun d -> 
//                        let origin = d.Forward.TransformPos(V3d.OOO)
//                        let target = origin + d.Forward.TransformDir(-V3d.OOI) * 100.0
//                        [| Line3d(origin,target) |]) 
//                ) 
//
//            Sg.ofList [
//                controllerBox
//                    |> Sg.effect [
//                        DefaultSurfaces.trafo |> toEffect
//                        DefaultSurfaces.constantColor C4f.White |> toEffect
//                        DefaultSurfaces.simpleLighting |> toEffect
//                    ]
//                beam
//                    |> Sg.effect [
//                        DefaultSurfaces.trafo |> toEffect
//                        DefaultSurfaces.vertexColor |> toEffect
//                        DefaultSurfaces.thickLine |> toEffect
//                    ]
//            ]
//
//        )


    let getControllerObject (i : int) =
        Some leftHandObject
//        if i > 0 then Some leftHandObject
//        else None

    let sceneObj =
        {
            controllerObjects = List.init VrDriver.devices.Length getControllerObject
            activeObjects = PersistentHashSet.empty
            things = PersistentHashSet.ofList objects
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

    vrWin.Run()

    OpenVR.Shutdown()

    0
