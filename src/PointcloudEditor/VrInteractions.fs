namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

module VrInteractions =
    open VrTypes
    open VrDriver
    open LogicalSceneTypes

    let getAxisValueWithDeathZone (value : float) = 
        let deathZone = 0.1
        let axisWithDeathZone = clamp 0.0 1.0 (value * (1.0 + deathZone) - deathZone)
        axisWithDeathZone

    let scaleFactorToFrameScaleFactor(scalingFactor : float, dt : float) =
            let scalingSpeed = 1.1
            1.0 + scalingFactor * scalingSpeed * (dt)

    let transformForPointCloud(deltaTrafo : Trafo3d, currTrafo : Trafo3d, pivot : V3d) =
            let pointcloudOffset = currTrafo.Forward.TransformPos(V3d())
            let centroidToPivot = Trafo3d.Translation(-pivot)
            centroidToPivot * deltaTrafo * centroidToPivot.Inverse
//            let toPointcloudOrigin = Trafo3d.Translation(-pointcloudOffset)
//            let centroidToPivot = Trafo3d.Translation(pointcloudOffset-pivot)
//            toPointcloudOrigin * centroidToPivot * deltaTrafo * centroidToPivot.Inverse * toPointcloudOrigin.Inverse

    let scaleForPointCloud(scalingFactor : float, currTrafo : Trafo3d, pivot : V3d) =
            let deltaTrafo = Trafo3d.Scale(scalingFactor)
            transformForPointCloud(deltaTrafo, currTrafo, pivot)

    let scalePointCloudWithTrackpad (scene : Scene, dt : float) =
        let getScaleFactorForController(firstController : bool) =
            let interactionInfo = if firstController then scene.interactionInfo1 else scene.interactionInfo2

            if interactionInfo.trackpadPressed then 
                let getAxisValue(deviceId : uint32) = 
                    let mutable state = VRControllerState_t()
                    let axisPosition =
                        if system.GetControllerState(deviceId, &state) then
                            Some (V2d(state.[0].x, state.[0].y))
                        else None
                    let axisValue = if axisPosition.IsSome then axisPosition.Value else V2d()
                    axisValue

                let axisValue = if firstController then getAxisValue(uint32 assignedInputs.controller1Id) else getAxisValue(uint32 assignedInputs.controller2Id)
                let scalingFactor = axisValue.Y
                scalingFactor
            else
                0.0

        let pivot = V3d()
        let currCentroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
        let scaleFactor1 = getScaleFactorForController(true)
        let scaleFactor2 = getScaleFactorForController(false)
        let scaleFactor = scaleFactor1 + scaleFactor2
        let scaleFactorForFrame = scaleFactorToFrameScaleFactor(scaleFactor, dt)
        let deltaTrafo = scaleForPointCloud(scaleFactorForFrame, currCentroidTrafo, pivot)
        let newCentroidTrafo = currCentroidTrafo * deltaTrafo
        let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.centroidId, newCentroidTrafo, scene.objects)
        newObjects

    let transformPointCloudWithPinch (scene : Scene) =
        let currController1Trafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.controller1ObjectId, scene.objects)
        let currController2Trafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.controller2ObjectId, scene.objects)
        let oldController1Trafo = scene.interactionInfo1.lastContrTrafo
        let oldController2Trafo = scene.interactionInfo2.lastContrTrafo

        let getControllerMiddleTrafo(controller1Trafo : Trafo3d, controller2Trafo : Trafo3d) =
            let controller1Pos = controller1Trafo.Forward.TransformPos(V3d())
            let controller2Pos = controller2Trafo.Forward.TransformPos(V3d())
            let controller1Forward = controller1Trafo.Forward.TransformPos(V3d.IOO)
            let controller2Forward = controller2Trafo.Forward.TransformPos(V3d.IOO)

            let translation = (controller1Pos + controller2Pos) / 2.0
            let scale = (controller1Pos - controller2Pos).Length
            let ctr1To2 = controller2Pos - controller1Pos

            let rotateAxisToX = Trafo3d.RotateInto(ctr1To2, V3d.IOO)
            let forward1OnYZ = rotateAxisToX.Forward.TransformDir(controller1Forward)
            let forward1OnYZ = forward1OnYZ.OYZ.Normalized
            let forward2OnYZ = rotateAxisToX.Forward.TransformDir(controller2Forward)
            let forward2OnYZ = forward2OnYZ.OYZ.Normalized
            let angle1 = acos(V3d.Dot(V3d.OIO, forward1OnYZ))
            let angle2 = acos(V3d.Dot(V3d.OIO, forward2OnYZ))

            (translation, scale, ctr1To2, angle1, angle2)
            
        let (currMiddleTrans, currMiddleScale, currCtr1To2, _, _) = getControllerMiddleTrafo(currController1Trafo, currController2Trafo)
        let (oldMiddleTrans, oldMiddleScale, oldCtr1To2, _, _) = getControllerMiddleTrafo(oldController1Trafo, oldController2Trafo)
//        let (currMiddleTrans, currMiddleScale, currCtr1To2, currAngle1, currAngle2) = getControllerMiddleTrafo(currController1Trafo, currController2Trafo)
//        let (oldMiddleTrans, oldMiddleScale, oldCtr1To2, oldAngle1, oldAngle2) = getControllerMiddleTrafo(oldController1Trafo, oldController2Trafo)
        
        let deltaTrans = Trafo3d.Translation(currMiddleTrans - oldMiddleTrans)
        let deltaScale = Trafo3d.Scale(currMiddleScale / oldMiddleScale)
        let deltaRot =   Trafo3d.RotateInto(oldCtr1To2, currCtr1To2)
        
//        let deltaAngle1 = (currAngle1 - oldAngle1) % Constant.PiTimesTwo
//        let deltaAngle2 = (currAngle2 - oldAngle2) % Constant.PiTimesTwo
//        let avgDeltaAngle = ((deltaAngle1 + deltaAngle2) % Constant.PiTimesTwo) / 2.0
        let controllerDeltaRot = Trafo3d.Identity// Trafo3d.Rotation(currCtr1To2.Normalized, avgDeltaAngle)
//        printfn "avgDeltaAngle = %A, currCtr1To2 = %A" avgDeltaAngle currCtr1To2
//        printfn "deltaTrans = %A, deltaScale = %A" deltaTrans deltaScale

        let pivot = currMiddleTrans
        let currCentroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
        let deltaTrafo = deltaScale * deltaRot * controllerDeltaRot * deltaTrans
        let deltaTrafo = transformForPointCloud(deltaTrafo, currCentroidTrafo, pivot)
        let newCentroidTrafo = currCentroidTrafo * deltaTrafo
        let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.centroidId, newCentroidTrafo, scene.objects)
        newObjects