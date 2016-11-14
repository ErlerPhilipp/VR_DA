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

        let perform (msg : Message) =
            transact (fun () ->
                scene <- update scene msg
                Conversion.Update(mscene, scene)
            )

        let deviceCount = VrDriver.devices.Length
        let oldTrafos = Array.zeroCreate deviceCount
        let update (dt : System.TimeSpan) (trafos : Trafo3d[]) (e : VREvent_t) =

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
            mscene.mobjects
                |> ASet.map toSg
                |> Sg.set

        Sg.ofList [sgs; objects; PhysicsScene.debugDrawer.debugDrawerSg]
            |> Sg.viewTrafo mscene.mviewTrafo
