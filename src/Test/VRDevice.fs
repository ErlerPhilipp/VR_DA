namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.VR

[<AutoOpen>]
module VrDevice =

    type VrDevice(system : CVRSystem, deviceType : VrDeviceType, index : int) =
    
        let getString (prop : ETrackedDeviceProperty) =
            let builder = System.Text.StringBuilder(4096, 4096)
            let mutable err = ETrackedPropertyError.TrackedProp_Success
            let len = system.GetStringTrackedDeviceProperty(uint32 index, prop, builder, uint32 builder.Capacity, &err)
            builder.ToString()

        let getInt (prop : ETrackedDeviceProperty) =
            let mutable err = ETrackedPropertyError.TrackedProp_Success
            let len = system.GetInt32TrackedDeviceProperty(uint32 index, prop, &err)

            len

        let vendor  = lazy ( getString ETrackedDeviceProperty.Prop_ManufacturerName_String )
        let model   = lazy ( getString ETrackedDeviceProperty.Prop_ModelNumber_String )
    
        let axis = 
            [|
                for i in 0..4 do
                    let t = getInt (ETrackedDeviceProperty.Prop_Axis0Type_Int32 + unbox i) |> unbox<EVRControllerAxisType>
                    if t <> EVRControllerAxisType.k_eControllerAxis_None then
                        yield VrAxis(system, t, index, i)
            |]

    
        let deviceToWorld = Mod.init Trafo3d.Identity
        let worldToDevice = deviceToWorld |> Mod.map (fun t -> t.Inverse)

        let mutable velocity = V3d(0.0, 0.0, 0.0)
        let mutable angularVelocity = V3d(0.0, 0.0, 0.0)

        member x.Update(currentPose : TrackedDevicePose_t) =
//            if axis.Length > 0 then
//                let mutable state = VRControllerState_t()
//                if system.GetControllerState(uint32 index, &state) then
//                    for a in axis do
//                        a.Update(e, state)

            if currentPose.bPoseIsValid then
                transact (fun () -> 
                    let theirs = currentPose.mDeviceToAbsoluteTracking.Trafo
                    deviceToWorld.Value <- theirs * flip.Inverse
                )
                velocity <- V3d(currentPose.vVelocity.v0, currentPose.vVelocity.v1, currentPose.vVelocity.v2)
                angularVelocity <- V3d(currentPose.vAngularVelocity.v0, currentPose.vAngularVelocity.v1, currentPose.vAngularVelocity.v2)
                if index = 2 then printfn "update device velocity to %A" velocity

        member x.Type = deviceType
        member x.Index = index
        member x.Vendor = vendor.Value
        member x.Model = model.Value
        member x.Axis = axis
        member x.DeviceToWorld = deviceToWorld
        member x.WorldToDevice = worldToDevice
        member x.Velocity = velocity
        member x.AngularVelocity = angularVelocity
        
type InputDevices =
    {
        hmd : VrDevice
        controller1 : VrDevice
        controller2 : VrDevice
        cam1 : VrDevice
        cam2 : VrDevice
    }
        
type InputAssignment =
    {
        hmdId : int
        controller1Id : int
        controller2Id : int
        cam1Id : int
        cam2Id : int
    }