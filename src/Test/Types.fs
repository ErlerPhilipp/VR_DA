namespace Aardvark.VR

open Aardvark.Base
open Aardvark.SceneGraph

[<AutoOpen>]
module Types =

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

