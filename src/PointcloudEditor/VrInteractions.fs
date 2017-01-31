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
            let toPointcloudOrigin = Trafo3d.Translation(-pointcloudOffset)
            let centroidToPivot = Trafo3d.Translation(pointcloudOffset-pivot)
            toPointcloudOrigin * centroidToPivot * deltaTrafo * centroidToPivot.Inverse * toPointcloudOrigin.Inverse

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
            let translation = (controller1Pos + controller2Pos) / 2.0
            let scale = (controller1Pos - controller2Pos).Length
            let rotation = Trafo3d.RotateInto(V3d.IOO, (controller1Pos - controller2Pos))
            (translation, scale, rotation)

        let (currMiddleTrans, currMiddleScale, currMiddleRot) = getControllerMiddleTrafo(currController1Trafo, currController2Trafo)
        let (oldMiddleTrans, oldMiddleScale, oldMiddleRot) = getControllerMiddleTrafo(oldController1Trafo, oldController2Trafo)
        
        let deltaTrans = currMiddleTrans - oldMiddleTrans
        let deltaScale = currMiddleScale / oldMiddleScale
        let deltaRot = oldMiddleRot.Inverse * currMiddleRot
//        printfn "deltaTrans = %A, deltaScale = %A" deltaTrans deltaScale

        let pivot = currMiddleTrans
        let currCentroidTrafo = getTrafoOfFirstObjectWithId(scene.specialObjectIds.centroidId, scene.objects)
        let deltaTrafo = Trafo3d.Scale(deltaScale) * deltaRot * Trafo3d.Translation(deltaTrans)
        let deltaTrafo = transformForPointCloud(deltaTrafo, currCentroidTrafo, pivot)
        let newCentroidTrafo = currCentroidTrafo * deltaTrafo
        let newObjects = setTrafoOfObjectsWithId(scene.specialObjectIds.centroidId, newCentroidTrafo, scene.objects)
        newObjects