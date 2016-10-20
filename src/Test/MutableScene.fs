namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

[<AutoOpen>]
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


