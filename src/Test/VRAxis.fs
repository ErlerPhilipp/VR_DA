namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental

module VrAxis =
    open VrTypes

    type VrAxis(system : CVRSystem, axisType : EVRControllerAxisType, deviceIndex : int, index : int) =
    
        let touched = Mod.init false
        let pressed = Mod.init false
        let position = Mod.init None
        let down = new System.Reactive.Subjects.Subject<unit>()
        let up = new System.Reactive.Subjects.Subject<unit>()

  
        member x.Touched = touched :> IMod<_>
        member x.Pressed = pressed :> IMod<_>
        member x.Position = position :> IMod<_>
        member x.Down = down :> System.IObservable<_>
        member x.Up = up :> System.IObservable<_>

        member x.Update(e : VREvent_t, state : VRControllerState_t) =

            if int e.trackedDeviceIndex = deviceIndex then
                let buttonIndex = int e.data.controller.button |> unbox<EVRButtonId>
                if buttonIndex >= EVRButtonId.k_EButton_Axis0 && buttonIndex <= EVRButtonId.k_EButton_Axis4 then
                    let axis = buttonIndex - EVRButtonId.k_EButton_Axis0 |> int

                    if axis  = index then
                        let eventType = e.eventType |> int |> unbox<EVREventType>
                        transact (fun () ->
                            match eventType with
                                | EVREventType.VREvent_ButtonTouch -> touched.Value <- true
                                | EVREventType.VREvent_ButtonUntouch -> touched.Value <- false
                                | EVREventType.VREvent_ButtonPress -> 
                                    down.OnNext()
                                    pressed.Value <- true
                                | EVREventType.VREvent_ButtonUnpress -> 
                                    up.OnNext()
                                    pressed.Value <- false
                                | _ -> ()
                        )

            if touched.Value then
                let pos = state.[index]
                transact (fun () ->
                    position.Value <- Some (V2d(pos.x, pos.y))
                )
            else
                match position.Value with
                    | Some _ -> transact (fun () -> position.Value <- None)
                    | _ -> ()

