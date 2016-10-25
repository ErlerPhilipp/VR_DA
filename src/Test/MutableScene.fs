namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

[<AutoOpen>]
module MutableScene =
    
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
            
            cam1Object        : Object
            cam2Object        : Object
            controller1Object : Object
            controller2Object : Object

            moveDirection     : V3d
        }
        
    let assignedInputs = VrDriver.inputAssignment ()

    let mutable currentId = 0
    let newId() = 
        currentId <- currentId + 1
        currentId

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
            
            mcam1Object        : ModRef<MObject>
            mcam2Object        : ModRef<MObject>
            mcontroller1Object : ModRef<MObject>
            mcontroller2Object : ModRef<MObject>
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

                mcam1Object = s.cam1Object |> Conversion.Create |> Mod.init
                mcam2Object = s.cam2Object |> Conversion.Create |> Mod.init
                mcontroller1Object = s.controller1Object |> Conversion.Create |> Mod.init
                mcontroller2Object = s.controller2Object |> Conversion.Create |> Mod.init
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
                
                Conversion.Update(m.mcam1Object.Value, s.cam1Object)
                Conversion.Update(m.mcam2Object.Value, s.cam2Object)
                Conversion.Update(m.mcontroller1Object.Value, s.controller1Object)
                Conversion.Update(m.mcontroller2Object.Value, s.controller2Object)

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
        | DeviceTouch of int * int * Trafo3d
        | DeviceUntouch of int * int * Trafo3d
        | DeviceMove of int * Trafo3d
        | TimeElapsed of System.TimeSpan
        | UpdateViewTrafo of Trafo3d

    let change (i : int) (f : 'a -> 'a) (l : list<'a>) =
        l |> List.mapi (fun ii v -> if ii = i then f v else v)

    let filterIndices (set : Set<int>) (l : list<'a>) =
        let rec filterIndices (i : int) (set : Set<int>) (l : list<'a>) =
            match l with
                | [] -> []
                | h :: rest ->
                    if Set.contains i set then h :: filterIndices (i+1) set rest
                    else filterIndices (i+1) set rest
        filterIndices 0 set l

    let update (scene : Scene) (message : Message) : Scene =
        match message with
            | TimeElapsed _ | UpdateViewTrafo _ | DeviceMove _ | DeviceTouch _ | DevicePress _ | DeviceUntouch _ | DeviceRelease _ -> ()
            | _ -> printfn "%A" message

        let scene =
            match message with
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                    { scene with 
                        controller1Object = {scene.controller1Object with trafo = t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller2Id ->
                    { scene with 
                        controller2Object = {scene.controller2Object with trafo = t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam1Id ->
                    { scene with 
                        cam1Object = {scene.cam1Object with trafo = t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam2Id ->
                    { scene with 
                        cam2Object = {scene.cam2Object with trafo = t}
                    }
                | _ -> 
                    scene


        match message with
            | DevicePress(deviceId, _, t) when deviceId = assignedInputs.controller2Id ->
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
                    
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                let direction = t.Forward.TransformDir(V3d.OOI)
                { scene with moveDirection = direction }
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
                    
            | DeviceRelease(deviceId, _, _) when deviceId = assignedInputs.controller2Id ->
                { scene with 
                    activeObjects = PersistentHashSet.empty
                    things = PersistentHashSet.union scene.activeObjects scene.things 
                    lastTrafo = Trafo3d.Identity
                }

            | TimeElapsed(dt) ->
                let maxSpeed = 10.0
                    
                let mutable state = VRControllerState_t()
                let axisPosition =
                    if system.GetControllerState(uint32 assignedInputs.controller1Id, &state) then
                        Some (V2d(state.[1].x, state.[1].y))
                    else None

                let axisValue = if axisPosition.IsSome then axisPosition.Value.X else 0.0

                let deathZone = 0.1
                let axisWithDeathZone = clamp 0.0 1.0 (axisValue * (1.0 + deathZone) - deathZone)
                //printfn "axisWithDeathZone: %A" axisWithDeathZone

                let dp = Trafo3d.Translation(scene.moveDirection * dt.TotalSeconds * maxSpeed * axisWithDeathZone)
                { scene with
                    // only move static things, keep active things like controllers
                    things = scene.things |> PersistentHashSet.map (fun o -> { o with trafo = o.trafo * dp })
                }

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
                    | EVREventType.VREvent_ButtonTouch -> perform(DeviceTouch(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonUntouch -> perform(DeviceUntouch(deviceId, axis, trafo))
                    | _ -> () //printfn "%A" (e.eventType)
    
            ()

        win.Update <- update

        let toSg (t : MObject) =
            t.mmodel
                |> Sg.dynamic
                |> Sg.trafo t.mtrafo

        let visibleDevices = [mscene.mcontroller1Object.Value; mscene.mcontroller2Object.Value; mscene.mcam1Object.Value; mscene.mcam2Object.Value]
        let objects = 
            visibleDevices |> List.map toSg
                |> Sg.ofList
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


