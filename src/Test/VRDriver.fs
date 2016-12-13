namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

open System.Collections.Generic

module VrDriver =
    open VrTypes
    open VrConversions
    
    exception VrException of string

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
                    yield VrDevice.VrDevice(system, toDeviceType deviceType, int i)
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

module Vibration = 
    open System.Threading
    open System.Collections.Concurrent

    type private VibrationEvent = int * float // duration in microsec, strength

    let private threads = ConcurrentDictionary<uint32, List<VibrationEvent>*CancellationTokenSource*SemaphoreSlim>()

    let private triggerHapticPulse(deviceIndex : uint32, axis : uint32, durationUs : uint16) = 
        OpenVR.System.TriggerHapticPulse(deviceIndex, axis, char durationUs)
        
    let private vibrationThread (deviceIndex : uint32) =
        let l = List<VibrationEvent>()
        let semaphore = new SemaphoreSlim(0)
        let cts = new System.Threading.CancellationTokenSource()
        let worker =
            async {
                do! Async.SwitchToNewThread()
                while not cts.IsCancellationRequested do
                    let sleepTimeMs = 5
                    semaphore.Wait(cts.Token)
                    let strength = lock l (fun () -> 
                        if not (l.IsEmpty()) then          
                            let durationUs, strength = l.[0]
                            let newDurationUs = durationUs - sleepTimeMs * 1000
                            if newDurationUs > 0 then
                                l.[0] <- (newDurationUs, strength)
                                semaphore.Release() |> ignore
                            else
                                l.RemoveAt(0)
                                    
                            strength
                        else
                            failwith("List empty although semaphore released")
                    )

                    let minDurationUs = 0us
                    let maxDurationUs = 250us // empirical max strength
                    let pulseDurationUs = Aardvark.Base.Fun.Lerp(strength, minDurationUs, maxDurationUs)
                    triggerHapticPulse(deviceIndex, 0u, pulseDurationUs)
                            
                    // according to docu: no impulse possible in the next 5 ms
                    do! Async.Sleep sleepTimeMs
                ()
            } 
            
        Async.Start(worker,cts.Token)

        l, cts, semaphore

    let stopVibration(deviceIndex : uint32) =
        let l,_,semaphore = threads.GetOrAdd(deviceIndex, vibrationThread)
        
        lock l (fun () -> 
            while semaphore.Wait(0) do
                if not (l.IsEmpty()) then
                        l.RemoveAt 0
        )

//        match threads.TryRemove(deviceIndex) with
//            | (true,(c,cts)) ->
//                cts.Cancel()
//            | _ -> failwith "cannot stop without start"

    let vibrate(deviceIndex : uint32, durationMs : int, strength : float) =
    
        let l,_,semaphore = threads.GetOrAdd(deviceIndex, vibrationThread)

        lock l (fun () -> 
            let newEvent = (durationMs * 1000, strength)
            l.Add newEvent
            semaphore.Release() |> ignore
        )
        
        printfn "sem: %A, list count: %A" (semaphore.CurrentCount) (l.Count)