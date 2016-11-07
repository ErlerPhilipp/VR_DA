namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.VR

module VrDevice =
    open VrTypes
    open VrConversions

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
                        yield VrAxis.VrAxis(system, t, index, i)
            |]

    
        let deviceToWorld = Mod.init Trafo3d.Identity
        let worldToDevice = deviceToWorld |> Mod.map (fun t -> t.Inverse)

        let mutable velocity = V3d(0.0, 0.0, 0.0)
        let mutable angularVelocity = V3d(0.0, 0.0, 0.0)

        member x.Update(currentPose : TrackedDevicePose_t) =

            if currentPose.bPoseIsValid then
                transact (fun () -> 
                    let theirs = currentPose.mDeviceToAbsoluteTracking.Trafo
                    deviceToWorld.Value <- theirs * flip.Inverse
                )
                velocity <- V3d(currentPose.vVelocity.v0, currentPose.vVelocity.v1, currentPose.vVelocity.v2)
                angularVelocity <- V3d(currentPose.vAngularVelocity.v0, currentPose.vAngularVelocity.v1, currentPose.vAngularVelocity.v2)

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
        hmd : VrDevice.VrDevice
        controller1 : VrDevice.VrDevice
        controller2 : VrDevice.VrDevice
        cam1 : VrDevice.VrDevice
        cam2 : VrDevice.VrDevice
    }
        
type InputAssignment =
    {
        hmdId : int
        controller1Id : int
        controller2Id : int
        cam1Id : int
        cam2Id : int
    }