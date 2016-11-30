﻿namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

open ShadowVolumes

module GraphicsScene =
    open LogicalScene
    open VrInteractions
    open VrWindow

    type MObject =
        {
            mutable original    : Object
            mtrafo              : ModRef<Trafo3d>
            mmodel              : ModRef<ISg>
            mhasHighlight       : ModRef<bool>
            mtilingFactor       : ModRef<V2d>
        }

    type MScene =
        {
            mutable original    : Scene
            mobjects            : cset<MObject>
            mviewTrafo          : ModRef<Trafo3d>
            mlightPos           : ModRef<V3d>
        }

    type Conversion private() =
        static member Create(o : Object) =
            {
                original = o
                mtrafo = Mod.init o.trafo
                mmodel = Mod.init o.model
//                mhasHighlight = Mod.init o.isGrabbable
                mhasHighlight = Mod.init o.hasScored
                mtilingFactor = Mod.init o.tilingFactor
            }

        static member Create(s : Scene) =
            let lightPos = LogicalScene.getTrafoOfFirstObjectWithId(s.lightId, s.objects).Forward.TransformPos(V3d())
            {
                original = s
                mobjects = CSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map Conversion.Create)
                mviewTrafo = Mod.init s.viewTrafo
                mlightPos = Mod.init (lightPos)
            }

        static member Update(mo : MObject, o : Object) =
            if not (System.Object.ReferenceEquals(mo.original, o)) then
                mo.original <- o
                mo.mmodel.Value <- o.model
                mo.mtrafo.Value <- o.trafo
                mo.mhasHighlight.Value <- o.hasScored
//                mo.mhasHighlight.Value <- o.isGrabbable

        static member Update(ms : MScene, s : Scene) =
            if not (System.Object.ReferenceEquals(ms.original, s)) then
                let lightPos = LogicalScene.getTrafoOfFirstObjectWithId(s.lightId, s.objects).Forward.TransformPos(V3d())

                ms.original <- s

                ms.mviewTrafo.Value <- s.viewTrafo

                ms.mlightPos.Value <- lightPos

                let table = 
                    ms.mobjects |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                for t in PersistentHashSet.toSeq s.objects do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t)
                        | _ ->
                            let mo = Conversion.Create(t)
                            ms.mobjects.Add mo |> ignore
                
                ms.mobjects.ExceptWith table.Values
            

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
                |> Sg.uniform "isHighlighted" t.mhasHighlight
                |> Sg.uniform "tilingFactor" t.mtilingFactor
                |> Sg.trafo t.mtrafo
        
        let objectsInScene = 
            mscene.mobjects
                |> ASet.map toSg
                |> Sg.set
                
        let toShadowCasterSg (t : MObject) =
            if t.original.castsShadow then
                t.mmodel
                    |> Sg.dynamic
                    |> Sg.uniform "isHighlighted" t.mhasHighlight
                    |> Sg.uniform "tilingFactor" t.mtilingFactor
                    |> Sg.trafo t.mtrafo
            else
                Sg.ofList []

        let shadowCasterInScene = 
            mscene.mobjects
                |> ASet.map toShadowCasterSg
                |> Sg.set

        let sgs = 
            objectsInScene
                |> Sg.fillMode ShadowVolumes.mode
                |> Sg.andAlso (ShadowVolumes.shadows (shadowCasterInScene))
                |> Sg.uniform "LightLocation" mscene.mlightPos
                |> Sg.uniform "SpecularExponent" (Mod.constant 32)
                |> Sg.uniform "AmbientFactor" (Mod.constant 0.03)
                |> Sg.uniform "LinearAttenuation" (Mod.constant 0.05)

        Sg.ofList [sgs; PhysicsScene.debugDrawer.debugDrawerSg]
            |> Sg.viewTrafo mscene.mviewTrafo
