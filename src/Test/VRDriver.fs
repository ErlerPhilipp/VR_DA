namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

[<AutoOpen>]
module VrDriver =
    let inline fail fmt = 
        Printf.kprintf (fun str -> 
            Log.error "%s" str
            raise <| VrException str
        ) fmt

    let inline check e =
        if int e <> 0 then 
            let str = sprintf "[VrDriver] got error %A" e
            Log.error "%s" str
            raise <| VrException str

    [<CompiledName("System")>]
    let system =
        let mutable err = EVRInitError.None
        let sys = OpenVR.Init(&err, EVRApplicationType.VRApplication_Scene)
        if err <> EVRInitError.None then
            fail "[VrDriver] could not initialize: %s" (OpenVR.GetStringForHmdError err)
        sys
        
    [<CompiledName("Compositor")>]
    let compositor =
        OpenVR.Compositor
        
    [<CompiledName("DesiredSize")>]
    let desiredSize =
        let mutable width = 0u
        let mutable height = 0u
        system.GetRecommendedRenderTargetSize(&width,&height)
        V2i(int width, int height)
        
    

    [<CompiledName("Devices")>]
    let devices =
        [|
            for i in 0u .. OpenVR.k_unMaxTrackedDeviceCount-1u do
                let deviceType = system.GetTrackedDeviceClass i
                if deviceType <> ETrackedDeviceClass.Invalid then
                    yield VrDevice(system, toDeviceType deviceType, int i)
        |]

    let getInputDevices () = 
        let controllers = devices |> Array.filter(fun d -> d.Type = VrDeviceType.Controller)
        let cams = devices |> Array.filter(fun d -> d.Type = VrDeviceType.TrackingReference)
        {
            hmd = devices |> Array.find(fun d -> d.Type = VrDeviceType.Hmd)
            controller1 = controllers.[0]
            controller2 = controllers.[1]
            cam1 = cams.[0]
            cam2 = cams.[1]
        }

    let getInputAssignment () =
        // todo regrab devices
        let hmdIndex = devices |> Array.findIndex (fun d -> d.Type = VrDeviceType.Hmd)
        let controllers = devices |> Array.mapi (fun index d -> index,d) |> Array.filter (fun (i,d) -> d.Type = VrDeviceType.Controller)
        match controllers with
            | [|(id1,_); (id2,_)|] -> 
                let cams = devices |> Array.mapi (fun index d -> index,d) |> Array.filter (fun (i,d) -> d.Type = VrDeviceType.TrackingReference)
                match cams with
                    | [|(camid1,_); (camid2,_)|] -> 
                    {
                        hmdId = hmdIndex
                        controller1Id = id1
                        controller2Id = id2
                        cam1Id = camid1
                        cam2Id = camid2
                    }
                    | _ -> failwithf "could not get input assignment. need exactly 2 cams, was: %d" cams.Length
            | _ -> failwithf "could not get input assignment. need exactly 2 controllers, was: %d" controllers.Length
        
    let assignedInputs = getInputAssignment ()
    let inputDevices = getInputDevices ()

    let updateInputDevices () =

        let mutable pose = TrackedDevicePose_t()
        let mutable gamePose = TrackedDevicePose_t()
            
        let err = compositor.GetLastPoseForTrackedDeviceIndex ((uint32 assignedInputs.hmdId), &pose, &gamePose)
        inputDevices.hmd.Update (pose)
        let err = compositor.GetLastPoseForTrackedDeviceIndex ((uint32 assignedInputs.controller1Id), &pose, &gamePose)
        inputDevices.controller1.Update (pose)
        let err = compositor.GetLastPoseForTrackedDeviceIndex ((uint32 assignedInputs.controller2Id), &pose, &gamePose)
        inputDevices.controller2.Update (pose)
        let err = compositor.GetLastPoseForTrackedDeviceIndex ((uint32 assignedInputs.cam1Id), &pose, &gamePose)
        inputDevices.cam1.Update (pose)
        let err = compositor.GetLastPoseForTrackedDeviceIndex ((uint32 assignedInputs.cam1Id), &pose, &gamePose)
        inputDevices.cam1.Update (pose)

        ()