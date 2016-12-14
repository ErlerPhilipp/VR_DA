namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

open System.Collections.Generic

module Vibration = 
    open System.Threading
    open System.Collections.Concurrent
    
    type VibrationEventType =
        | All
        | HitObject
        | OverlappingObject
        | Score

    type private VibrationEvent = VibrationEventType * int * float // duration in microsec, strength

    let private threads = ConcurrentDictionary<uint32, List<VibrationEvent>*CancellationTokenSource*ManualResetEventSlim>()

    let private triggerHapticPulse(deviceIndex : uint32, axis : uint32, durationUs : uint16) = 
        OpenVR.System.TriggerHapticPulse(deviceIndex, axis, char durationUs)
        
    let private vibrationThread (deviceIndex : uint32) =
        let l = List<VibrationEvent>()
        let nonEmpty = new ManualResetEventSlim(false)
        let cts = new System.Threading.CancellationTokenSource()
        let worker =
            async {
                do! Async.SwitchToNewThread()
                let stopwatch = System.Diagnostics.Stopwatch()
                while not cts.IsCancellationRequested do
                    stopwatch.Restart()
                    let sleepTimeMs = 5
                    nonEmpty.Wait(cts.Token)
                    let strength = lock l (fun () -> 
                        if not (l.IsEmpty()) then
                            let vibroType, durationUs, strength = l.[0]
                            let newDurationUs = durationUs - sleepTimeMs * 1000
                            if newDurationUs > 0 then
                                l.[0] <- (vibroType, newDurationUs, strength)
                            else
                                l.RemoveAt(0)
                                if l.Count = 0 then nonEmpty.Reset()
                                    
                            strength
                        else
                            printfn "List empty although semaphore released"
                            0.0
                    )
     
                    let minDurationUs = 0us
                    let maxDurationUs = 250us // empirical max strength
                    let pulseDurationUs = Aardvark.Base.Fun.Lerp(strength, minDurationUs, maxDurationUs)
                    
                    // according to docu: no impulse possible in the next 5 ms
//                    printfn "%A: start sleep" stopwatch.Elapsed.TotalSeconds
                    // do! Async.Sleep sleepTimeMs // sleep too inaccurate, do busy waiting
                    while stopwatch.Elapsed.TotalSeconds < float sleepTimeMs / 1000.0 do
                        ()
                        
                    triggerHapticPulse(deviceIndex, 0u, pulseDurationUs)
//                    printfn "%A: triggered pulse" stopwatch.Elapsed.TotalSeconds
                ()
            } 
            
        Async.Start(worker,cts.Token)

        l, cts, nonEmpty

    let stopVibration(vibroTypeToStop : VibrationEventType, deviceIndex : uint32) =
        let l,_,nonEmpty = threads.GetOrAdd(deviceIndex, vibrationThread)
        
        lock l (fun () -> 
            let mutable index = 0
            while index < l.Count do
                let vibroType, durationUs, strength = l.[index]
                if vibroTypeToStop = VibrationEventType.All || vibroType = vibroTypeToStop then
                    l.RemoveAt index
                else
                    index <- index + 1
            if l.Count = 0 then nonEmpty.Reset()
        )

//        match threads.TryRemove(deviceIndex) with
//            | (true,(c,cts)) ->
//                cts.Cancel()
//            | _ -> failwith "cannot stop without start"

    let vibrate(vibroType : VibrationEventType, deviceIndex : uint32, durationMs : int, strength : float) =
    
        let l,_,nonEmpty = threads.GetOrAdd(deviceIndex, vibrationThread)

        lock l (fun () -> 
            let newEvent = (vibroType, durationMs * 1000, strength)
            l.Add newEvent
            nonEmpty.Set()
        )
        
//        printfn "dev: %A, sem: %A, list count: %A" deviceIndex (semaphore.CurrentCount) (l.Count)

    let periodicPulses(numPeriods : int, impulsesPerPeriod : int, periodDurationMs : int, 
                       vibroType : VibrationEventType, deviceIndex : uint32, strength : float, strengthFunction : (float -> float)) = 
        for period in 0..numPeriods-1 do
            for impuls in 0..impulsesPerPeriod-1 do
                let impulseDuration = periodDurationMs / impulsesPerPeriod
                let currentStrength = strengthFunction(float impuls / float impulsesPerPeriod)
                vibrate(vibroType, deviceIndex, impulseDuration, currentStrength)

    let triangularFunctionPulses(numPeriods : int, impulsesPerPeriod : int, periodDurationMs : int, 
                                 vibroType : VibrationEventType, deviceIndex : uint32, strength : float) = 
        let triFunction (t : float) = 
            if t < 0.5 then
                strength * 2.0 * t
            else
                strength * 2.0 - strength * 2.0 * t
        periodicPulses(numPeriods, impulsesPerPeriod, periodDurationMs, vibroType, deviceIndex, strength, triFunction)

    let sawFunctionPulses(numPeriods : int, impulsesPerPeriod : int, periodDurationMs : int, 
                          vibroType : VibrationEventType, deviceIndex : uint32, strength : float) = 
        let triFunction (t : float) = 
                strength * t
        periodicPulses(numPeriods, impulsesPerPeriod, periodDurationMs, vibroType, deviceIndex, strength, triFunction)

    let sinusiodFunctionPulses(numPeriods : int, impulsesPerPeriod : int, periodDurationMs : int, 
                               vibroType : VibrationEventType, deviceIndex : uint32, strength : float) = 
        let sinFunction (t : float) = 
            0.5 + (Fun.Cos(t * Constant.PiTimesTwo) / 2.0) 
        periodicPulses(numPeriods, impulsesPerPeriod, periodDurationMs, vibroType, deviceIndex, strength, sinFunction)
        