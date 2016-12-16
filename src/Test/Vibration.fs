namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

module Vibration = 
    open System.Threading
    open System.Collections.Concurrent
    
    type VibrationEventType =
        | All
        | HitObject
        | OverlappingObject
        | Score

    type private VibrationEvent = VibrationEventType * float * float // duration, strength

    let private eventLists = Dict.empty
    let private getOrCreateEventList(key) = 
        let value = Dict.tryFind key eventLists
        match value with
            | Some value -> value
            | None -> 
                let newList = []
                Dict.add(key) newList eventLists
                newList

    let private triggerHapticPulse(deviceIndex : uint32, axis : uint32, durationUs : uint16) = 
        OpenVR.System.TriggerHapticPulse(deviceIndex, axis, durationUs)
//        printfn "triggered pulse on dev %A with length %A" deviceIndex durationUs
        // according to docu: no impulse possible in the next 5 ms
        
    let updateVibration (deviceIndex : uint32, dt : float) =
        let l = getOrCreateEventList(deviceIndex)
        let newEventList = 
            match l with
                | [] -> []
                | head :: tail -> 
                    let vibroType, durationUs, strength = head
                    
                    if strength > 0.0 then
                        let minDurationUs = 0us
                        let maxDurationUs = 1999us // empirical max strength
                        let pulseDurationUs = Aardvark.Base.Fun.Lerp(strength, minDurationUs, maxDurationUs)
                        triggerHapticPulse(deviceIndex, 0u, pulseDurationUs)

                    let newDurationUs = durationUs - dt
                    let newHead = 
                        if newDurationUs > 0.0 then
                            [(vibroType, newDurationUs, strength)]
                        else
                            []

                    newHead @ tail
        Dict.set deviceIndex newEventList eventLists

    let vibrate(vibroType : VibrationEventType, deviceIndex : uint32, duration : float, strength : float) =
        let l = getOrCreateEventList(deviceIndex)
        let newEvent = (vibroType, duration, strength)
        Dict.set deviceIndex (l @ [newEvent]) eventLists

    let stopVibration(vibroTypeToStop : VibrationEventType, deviceIndex : uint32) =
        let l = getOrCreateEventList(deviceIndex)
        let newEventList = l |> List.filter (fun e -> let (vibroType, newDurationUs, strength) = e
                                                      (vibroTypeToStop <> VibrationEventType.All && vibroType <> vibroTypeToStop) 
                                              )
        Dict.set deviceIndex newEventList eventLists

    let periodicPulses(numPeriods : int, impulsesPerPeriod : int, periodDuration : float, 
                       vibroType : VibrationEventType, deviceIndex : uint32, strength : float, strengthFunction : (float -> float)) = 
        for period in 0..numPeriods-1 do
            for impuls in 0..impulsesPerPeriod-1 do
                let impulseDuration = periodDuration / float impulsesPerPeriod
                let currentStrength = strengthFunction(float impuls / float impulsesPerPeriod)
                vibrate(vibroType, deviceIndex, impulseDuration, currentStrength)

    let triangularFunctionPulses(numPeriods : int, impulsesPerPeriod : int, periodDuration : float, 
                                 vibroType : VibrationEventType, deviceIndex : uint32, strength : float) = 
        let triFunction (t : float) = 
            if t < 0.5 then
                strength * 2.0 * t
            else
                strength * 2.0 - strength * 2.0 * t
        periodicPulses(numPeriods, impulsesPerPeriod, periodDuration, vibroType, deviceIndex, strength, triFunction)

    let sawFunctionPulses(numPeriods : int, impulsesPerPeriod : int, periodDuration : float, 
                          vibroType : VibrationEventType, deviceIndex : uint32, strength : float) = 
        let triFunction (t : float) = 
                strength * t
        periodicPulses(numPeriods, impulsesPerPeriod, periodDuration, vibroType, deviceIndex, strength, triFunction)

    let sinusiodFunctionPulses(numPeriods : int, impulsesPerPeriod : int, periodDuration : float, 
                               vibroType : VibrationEventType, deviceIndex : uint32, strength : float) = 
        let sinFunction (t : float) = 
            0.5 + (Fun.Cos(t * Constant.PiTimesTwo) / 2.0) 
        periodicPulses(numPeriods, impulsesPerPeriod, periodDuration, vibroType, deviceIndex, strength, sinFunction)
        