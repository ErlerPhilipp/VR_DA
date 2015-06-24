// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Valve.VR


[<EntryPoint>]
let main argv =

    let mutable err = EVRInitError.None
    let system = OpenVR.Init(&err, EVRApplicationType.VRApplication_Scene)
 
    let hasDevice = OpenVR.IsHmdPresent()
    let compositor = OpenVR.Compositor
    
    let start = OpenVR.k_unTrackedDeviceIndex_Hmd + 1u
    let cnt = OpenVR.k_unMaxTrackedDeviceCount
    
//    let deviceId =
//        { start .. cnt - 1u }
//            |> Seq.tryPick (fun i ->
//                let isConnected = system.IsTrackedDeviceConnected(i)
//                if isConnected then Some i
//                else None
//            )
//
//
//    for i in start .. cnt - 1u do
//        let isConnected = system.IsTrackedDeviceConnected(i)
//        ()


    let mutable leftTex = Texture_t()
    let mutable leftBounds = Unchecked.defaultof<_>

    leftTex.eColorSpace <- EColorSpace.Auto
    leftTex.eType <- EGraphicsAPIConvention.API_OpenGL
    leftTex.handle <- nativeint 0



    let errLeft = compositor.Submit(EVREye.Eye_Left, &leftTex, &leftBounds, EVRSubmitFlags.Submit_Default)

    //system.GetStringTrackedDeviceProperty(0u, ETrackedDeviceProperty.Prop_TrackingSystemName_String, )


    printfn "%A" hasDevice
    0 // return an integer exit code
