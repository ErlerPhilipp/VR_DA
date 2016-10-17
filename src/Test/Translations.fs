namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.VR

[<AutoOpen>]
module Translations =
    let toDeviceType (c : ETrackedDeviceClass) =
        match c with
            | ETrackedDeviceClass.HMD -> VrDeviceType.Hmd
            | ETrackedDeviceClass.Controller -> VrDeviceType.Controller
            | ETrackedDeviceClass.TrackingReference -> VrDeviceType.TrackingReference
            | _ -> VrDeviceType.Other

    let flip = Trafo3d.FromBasis(V3d.IOO, -V3d.OOI, V3d.OIO, V3d.Zero)

    type VRControllerState_t with
        member x.Item
            with get (i : int) =
                match i with
                    | 0 -> x.rAxis0
                    | 1 -> x.rAxis1
                    | 2 -> x.rAxis2
                    | 3 -> x.rAxis3
                    | _ -> x.rAxis4


