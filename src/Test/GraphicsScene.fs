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
            misGrabbable        : ModRef<bool>
        }

    type MScene =
        {
            mutable original    : Scene
            mobjects            : cset<MObject>
            mviewTrafo          : ModRef<Trafo3d>
        }

    type Conversion private() =
        static member Create(o : Object) =
            {
                original = o
                mtrafo = Mod.init o.trafo
                mmodel = Mod.init o.model
                misGrabbable = Mod.init o.isGrabbable
            }

        static member Create(s : Scene) =
            {
                original = s
                mobjects = CSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map Conversion.Create)
                mviewTrafo = Mod.init s.viewTrafo
            }

        static member Update(m : MObject, o : Object) =
            if not (System.Object.ReferenceEquals(m.original, o)) then
                m.original <- o
                m.mmodel.Value <- o.model
                m.mtrafo.Value <- o.trafo
                m.misGrabbable.Value <- o.isGrabbable

        static member Update(m : MScene, s : Scene) =
            if not (System.Object.ReferenceEquals(m.original, s)) then
                m.original <- s

                m.mviewTrafo.Value <- s.viewTrafo

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

        let deviceCount = VrDriver.devices.Length
        let oldTrafos = Array.zeroCreate deviceCount
        let mutable oldDeviceOffset = Trafo3d.Identity
        let update (dt : System.TimeSpan) (trafos : Trafo3d[]) (e : VREvent_t) =
            
            scene <- LogicalScene.update scene StartFrame

            let timeStepThreshold = 0.5
            if dt.TotalSeconds < timeStepThreshold && scene.enablePhysics then
                scene <- PhysicsScene.stepSimulation dt scene 
                PhysicsScene.debugDrawer.flush()
                    
            scene <- LogicalScene.update scene (TimeElapsed dt)

            let deviceOffsetHasChanged = oldDeviceOffset <> scene.deviceOffset
            oldDeviceOffset <- scene.deviceOffset

            for i in 0 .. VrDriver.devices.Length-1 do
                let t = trafos.[i]
                if oldTrafos.[i] <> t || deviceOffsetHasChanged then
                    oldTrafos.[i] <- t
                    scene <- LogicalScene.update scene (DeviceMove(i, t * scene.deviceOffset.Inverse))
                
            if e.trackedDeviceIndex >= 0u && e.trackedDeviceIndex < uint32 deviceCount then
                let deviceId = e.trackedDeviceIndex |> int
                let button = int e.data.controller.button |> unbox<EVRButtonId>
                let axis = button - EVRButtonId.k_EButton_Axis0 |> int
                let trafo = trafos.[deviceId]

                match unbox<EVREventType> (int e.eventType) with
                    | EVREventType.VREvent_ButtonPress -> scene <- LogicalScene.update scene (DevicePress(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonUnpress -> scene <- LogicalScene.update scene (DeviceRelease(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonTouch -> scene <- LogicalScene.update scene (DeviceTouch(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonUntouch -> scene <- LogicalScene.update scene (DeviceUntouch(deviceId, axis, trafo))
                    | _ -> () //printfn "%A" (e.eventType)
  
            transact (fun () ->
                Conversion.Update(mscene, scene)
            )
            ()

        win.Update <- update

        let toSg (t : MObject) =
            t.mmodel
                |> Sg.dynamic
                |> Sg.uniform "isHighlighted" t.misGrabbable
                |> Sg.trafo t.mtrafo

        let sgs = 
            mscene.mobjects
                |> ASet.map toSg
                |> Sg.set

        Sg.ofList [sgs; PhysicsScene.debugDrawer.debugDrawerSg]
            |> Sg.viewTrafo mscene.mviewTrafo
