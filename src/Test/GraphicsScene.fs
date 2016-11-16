namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

module GraphicsScene =
    open LogicalScene
    open VrInteractions
    open VrWindow

    type MObject =
        {
            mutable original    : Object
            mtrafo              : ModRef<Trafo3d>
            mmodel              : ModRef<ISg>
        }

    type MScene =
        {
            mutable original    : Scene
            mobjects            : cset<MObject>
            mviewTrafo          : ModRef<Trafo3d>
            
            mcam1Object         : ModRef<MObject>
            mcam2Object         : ModRef<MObject>
            mcontroller1Object  : ModRef<MObject>
            mcontroller2Object  : ModRef<MObject>
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
                mobjects = CSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map Conversion.Create)
                mviewTrafo = Mod.init s.viewTrafo

                mcam1Object = getObjectWithId(s.cam1ObjectId, s.objects) |> Conversion.Create |> Mod.init
                mcam2Object = getObjectWithId(s.cam2ObjectId, s.objects) |> Conversion.Create |> Mod.init
                mcontroller1Object = getObjectWithId(s.controller1ObjectId, s.objects) |> Conversion.Create |> Mod.init
                mcontroller2Object = getObjectWithId(s.controller2ObjectId, s.objects) |> Conversion.Create |> Mod.init
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
                
                Conversion.Update(m.mcam1Object.Value, getObjectWithId(s.cam1ObjectId, s.objects))
                Conversion.Update(m.mcam2Object.Value, getObjectWithId(s.cam2ObjectId, s.objects))
                Conversion.Update(m.mcontroller1Object.Value, getObjectWithId(s.controller1ObjectId, s.objects))
                Conversion.Update(m.mcontroller2Object.Value, getObjectWithId(s.controller2ObjectId, s.objects))

                let table = 
                    m.mobjects |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                
                for t in PersistentHashSet.toSeq s.objects do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t)
                        | _ ->
                            let mo = Conversion.Create(t)
                            m.mobjects.Add mo |> ignore
                
                m.mobjects.ExceptWith table.Values
            

    let createScene (initialScene : Scene) (win : VrWindow) =
        let mutable scene = initialScene
        let mscene = Conversion.Create initialScene

        let mutable shouldDoPhysics = true


        let perform (msg : Message) =
            transact (fun () ->
                scene <- update scene msg
                Conversion.Update(mscene, scene)
            )

        let deviceCount = VrDriver.devices.Length
        let oldTrafos = Array.zeroCreate deviceCount
        let update (dt : System.TimeSpan) (trafos : Trafo3d[]) (e : VREvent_t) =

            let timeStepThreshold = 0.5
            if dt.TotalSeconds < timeStepThreshold && scene.enablePhysics then

                transact (fun () -> 
                    scene <- PhysicsScene.stepSimulation dt scene 
                    Conversion.Update(mscene,scene)
                )

                PhysicsScene.debugDrawer.flush()

                perform (TimeElapsed dt)
            
            for i in 0 .. VrDriver.devices.Length-1 do
                let t = trafos.[i]
                if oldTrafos.[i] <> t then
                    oldTrafos.[i] <- t
                    if i = VrDriver.assignedInputs.hmdId then 
                        perform (UpdateViewTrafo(scene.deviceOffset * t.Inverse)) 
                    else 
                        perform (DeviceMove(i, t * scene.deviceOffset.Inverse))
                
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

        let sgs = 
            mscene.mobjects
                |> ASet.map toSg
                |> Sg.set

        Sg.ofList [sgs; PhysicsScene.debugDrawer.debugDrawerSg]
            |> Sg.viewTrafo mscene.mviewTrafo
