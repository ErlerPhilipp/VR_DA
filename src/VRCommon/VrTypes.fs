namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

module VrTypes =

    type VrDeviceType =
        | Other = 0
        | Hmd = 1
        | Controller = 2
        | TrackingReference = 3
            
    type VRControllerState_t with
        member x.Item
            with get (i : int) =
                match i with
                    | 0 -> x.rAxis0
                    | 1 -> x.rAxis1
                    | 2 -> x.rAxis2
                    | 3 -> x.rAxis3
                    | _ -> x.rAxis4

    let MonitorWindowSize = V2i(1600, 900)