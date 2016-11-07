namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

module VrInteractions =
    open VrTypes
    
    type Message =
        | DevicePress of int * int * Trafo3d
        | DeviceRelease of int * int * Trafo3d
        | DeviceTouch of int * int * Trafo3d
        | DeviceUntouch of int * int * Trafo3d
        | DeviceMove of int * Trafo3d
        | TimeElapsed of System.TimeSpan
        | UpdateViewTrafo of Trafo3d
    
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

    let getVirtualHandTrafo (realHandTrafo : Trafo3d, realHeadTrafo: Trafo3d, interactionType : VrInteractionTechnique) = 
        if interactionType = VrInteractionTechnique.VirtualHand then
            realHandTrafo
        else
            let handPos = realHandTrafo.Forward.TransformPos V3d.Zero
            let headPos = realHeadTrafo.Backward.TransformPos V3d.Zero
            let chestPos = headPos |> Trafo3d.Translation(0.0, -0.2, 0.0).Forward.TransformPos
            let chestToHand = handPos - chestPos
            //printfn "hand: %A, chest: %A" (chestPos) (handPos)
            let headToHandDist = chestToHand.Length
                    
            let linearExtensionLimit = 0.5

            if headToHandDist < linearExtensionLimit then
                realHandTrafo
            else
                let quadraticTermFactor = 200.0
                let quadraticExtension = headToHandDist - linearExtensionLimit
                let gogoAdditionalExtension = max 0.0 quadraticTermFactor * quadraticExtension * quadraticExtension // R_r + k(R_r - D)^2
                let gogoHandPosOffset = chestToHand.Normalized * gogoAdditionalExtension
                let gogoHandTrafo = realHandTrafo * Trafo3d.Translation(gogoHandPosOffset)
                //printfn "arm length: %A gogo arm length: %A, pos: %A" headToHandDist (1.0+gogoAdditionalExtension) (gogoHandTrafo.GetViewPosition())
                gogoHandTrafo

    let getAxisValueWithDeathZone (value : float) = 
        let deathZone = 0.1
        let axisWithDeathZone = clamp 0.0 1.0 (value * (1.0 + deathZone) - deathZone)
        axisWithDeathZone
