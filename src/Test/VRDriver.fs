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
