namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

open BulletSharp.Math

module VrInteractions =
    open VrTypes

    let getAxisValueWithDeathZone (value : float) = 
        let deathZone = 0.1
        let axisWithDeathZone = clamp 0.0 1.0 (value * (1.0 + deathZone) - deathZone)
        axisWithDeathZone