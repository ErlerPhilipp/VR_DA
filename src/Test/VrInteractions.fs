﻿namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

open BulletSharp.Math

module VrInteractions =
    open VrTypes
    
    type VrInteractionTechnique =
        | VirtualHand = 1
        | GoGo = 2
        
    type VrMovementTechnique =
        | Flying = 1
        | Teleport = 2

    let getAxisValueWithDeathZone (value : float) = 
        let deathZone = 0.1
        let axisWithDeathZone = clamp 0.0 1.0 (value * (1.0 + deathZone) - deathZone)
        axisWithDeathZone

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

    let getVirtualHandTrafoAndExtensionFactor (interactionType : VrInteractionTechnique, realHandTrafo : Trafo3d, realHeadTrafo: Trafo3d, trackingTrafo : Trafo3d) = 
        if interactionType = VrInteractionTechnique.VirtualHand then
            (realHandTrafo, 1.0)
        else
            let handPosTrackingSpace = (realHandTrafo * trackingTrafo).Forward.TransformPos(V3d.Zero)
            let headPosTrackingSpace = (realHeadTrafo.Inverse * trackingTrafo).Forward.TransformPos V3d.Zero
            let headToChestOffsetTrackingSpace = V3d(0.0, -0.15, 0.0)
            let chestPosTrackingSpace = Trafo3d.Translation(headToChestOffsetTrackingSpace).Forward.TransformPos(headPosTrackingSpace)
            let chestToHandTrackingSpace = handPosTrackingSpace - chestPosTrackingSpace
            //printfn "hand: %A, chest: %A" (chestPos) (handPos)
            let headToHandDistTrackingSpace = chestToHandTrackingSpace.Length
                    
            let linearExtensionLimit = 0.5

            if headToHandDistTrackingSpace < linearExtensionLimit then
                (realHandTrafo, 1.0)
            else
                let gogoQuadraticTermFactor = 200.0

                let quadraticExtension = headToHandDistTrackingSpace - linearExtensionLimit
                let gogoAdditionalExtension = max 0.0 gogoQuadraticTermFactor * quadraticExtension * quadraticExtension // R_r + k(R_r - D)^2
                let gogoHandPosOffset =  trackingTrafo.Backward.TransformDir(chestToHandTrackingSpace.Normalized) * gogoAdditionalExtension
                let gogoHandTrafo = realHandTrafo * Trafo3d.Translation(gogoHandPosOffset)
                //printfn "arm length: %A gogo arm length: %A, pos: %A" headToHandDist (1.0+gogoAdditionalExtension) (gogoHandTrafo.GetViewPosition())
                (gogoHandTrafo, gogoAdditionalExtension)
        
    let nextMovementTechnique (it : VrMovementTechnique) =
        match it with
            | VrMovementTechnique.Flying -> VrMovementTechnique.Teleport
            | VrMovementTechnique.Teleport -> VrMovementTechnique.Flying
            | _ -> VrMovementTechnique.Flying

    let getTrafoAfterFlying (currentTrafo : Trafo3d, moveDirection : V3d, deltaTime : float, axisValue: float) = 
        let axisWithDeathZone = getAxisValueWithDeathZone(axisValue)
        let maxSpeed = 10.0
        currentTrafo * Trafo3d.Translation(moveDirection * -deltaTime * maxSpeed * axisWithDeathZone)

    let getTrafoAfterTeleport (currentTrafo : Trafo3d, hmdTrafo : Trafo3d, targetPos : V3d, targetNormal : V3d) = 
        let trackingSpaceOrigin = currentTrafo.Forward.TransformPos(V3d())
        let hmdPosWorldSpace = hmdTrafo.Forward.TransformPos(V3d())
        let trackingOriginTargetPos = targetPos
        let translationTrafo = Trafo3d.Translation(trackingOriginTargetPos)
//        let trackingOffsetTrafo = Trafo3d.Translation(-(hmdPosWorldSpace - trackingSpaceOrigin).XOZ) // keep height of hmd
//        translationTrafo * trackingOffsetTrafo
                
        let currY = V3d.OIO
        let targetY = targetNormal.Normalized
        let currYDotTargetY = currY.Dot(targetY)
        let tinyValue = 0.01
        let rotationTrafo = 
            let currX = V3d.IOO
            let currZ = V3d.OOI
            let currXDotTargetY = currX.Dot(targetY)
            let currZDotTargetY = currZ.Dot(targetY)
            let rotAxisIsTargetXAxis = abs(currXDotTargetY) < abs(currZDotTargetY) + tinyValue
            let rotAxis = if rotAxisIsTargetXAxis then currX else currZ
            let forwardAxis = targetY.Cross(rotAxis)
            let tryTrafo =  if rotAxisIsTargetXAxis then 
                                Trafo3d.FromBasis(rotAxis, targetY, forwardAxis, V3d())
                            else
                                Trafo3d.FromBasis(forwardAxis, targetY, rotAxis, V3d())
            let switchXY = Trafo3d.FromBasis(V3d.OIO, V3d.IOO, V3d.OOI, V3d())
            let mirrorFix = Trafo3d.Scale(1.0, 1.0, -1.0)
            mirrorFix * switchXY.Inverse * Trafo3dExtensions.GetOrthoNormalOrientation(switchXY * tryTrafo)

//        rotationTrafo * trackingOffsetTrafo * translationTrafo
        rotationTrafo * translationTrafo
