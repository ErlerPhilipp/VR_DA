namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.SceneGraph

module VrTypes =

    type VrDeviceType =
        | Other = 0
        | Hmd = 1
        | Controller = 2
        | TrackingReference = 3

    type VrEye =
        | Left = 1
        | Right = 2
        
    exception VrException of string

    type VrInteractionTechnique =
        | VirtualHand = 1
        | GoGo = 2

    let nextInteractionTechnique (it : VrInteractionTechnique) =
        match it with
            | VrInteractionTechnique.VirtualHand -> VrInteractionTechnique.GoGo
            | VrInteractionTechnique.GoGo -> VrInteractionTechnique.VirtualHand
            | _ -> VrInteractionTechnique.VirtualHand
            
    let colorForInteractionTechnique(it : VrInteractionTechnique) =
        match it with
            | VrInteractionTechnique.VirtualHand -> C4f.White
            | VrInteractionTechnique.GoGo -> C4f.Green
            | _ -> C4f.White
            
    type VRControllerState_t with
        member x.Item
            with get (i : int) =
                match i with
                    | 0 -> x.rAxis0
                    | 1 -> x.rAxis1
                    | 2 -> x.rAxis2
                    | 3 -> x.rAxis3
                    | _ -> x.rAxis4

