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
        activeObjects   : pset<Object>
        things          : pset<Object>
        viewTrafo       : Trafo3d
        lastTrafo       : Trafo3d
    }

module NewStuff =

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

   




    let update (scene : Scene) (message : Message) : Scene =
        match message with
            | TimeElapsed _ | UpdateViewTrafo _ | DeviceMove _ -> ()
            | _ -> printfn "%A" message

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

            | DeviceMove(4, t) ->
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

            for i in 0 .. VrDriver.devices.Length-1 do
                let t = trafos.[i]
                if oldTrafos.[i] <> t then
                    oldTrafos.[i] <- t
                    if i = 0 then perform (UpdateViewTrafo(t.Inverse))
                    else perform (DeviceMove(i, t))
                
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
        let sgs = 
            ASet.union' [mscene.mthings; mscene.mactiveObjects]
                |> ASet.map (fun t -> 
                    t.mmodel
                        |> Sg.dynamic
                        |> Sg.trafo t.mtrafo
                )

        Sg.set sgs
            |> Sg.viewTrafo mscene.mviewTrafo



module OldStuff = 
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

let mutable grabbedObj : Option<Object> = None

//let respondToSensors (sensors : unit) (scene : Scene) =
//    // grab sensors,
//    // compute trafos
//    // modify scene objects
//    let firstObj = scene.objects.[0]
//    let objectInWorldSpace = firstObj.model.bounds.Transformed(firstObj.trafo.Value)
//
//    // logic happens
//    //
//
//
//    let x = 
//        if false then 
//            transact (fun () -> firstObj.trafo.Value <- Trafo3d.Translation(V3d.OOO)) 
//            1
//        else 
//            299
//
//
//    transact (fun () -> firstObj.trafo.Value <- Trafo3d.Translation(V3d.OOO))
//
//    ()

//
//let makeSceneGraph (s : Scene) : ISg =
//        
//    let flip = Trafo3d.FromBasis(V3d.IOO, V3d.OOI, -V3d.OIO, V3d.Zero)
//
//    let objects = 
//        List.concat [s.objects; s.staticGeometry]
//        |> List.map (fun o -> 
//                let scene = o.model |> Sg.AdapterNode
//                let transformed = o.trafo |> Mod.map (fun t -> flip * t) 
//                Sg.trafo transformed scene
//           )
//        |> Sg.ofSeq
//    objects
//    

[<EntryPoint>]
let main argv =
    
    Ag.initialize()
    Aardvark.Init()

// Hello World test
//    VRTest.VRTest.helloWorld () |> ignore
//    System.Environment.Exit 0

    use app = new OpenGlApplication()
    
    let vrWin = NewVrStuff.VrWindow(app.Runtime)

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

    let objects =
        let mutable currentId = 0
        let newId() = 
            currentId <- currentId + 1
            currentId

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

    let scene =
        {
            activeObjects = PersistentHashSet.empty
            things = PersistentHashSet.ofList objects
            viewTrafo = Trafo3d.Identity
            lastTrafo = Trafo3d.Identity
        }


    let scene =
        NewStuff.createScene scene vrWin
            //|> Sg.andAlso debugStuff
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
