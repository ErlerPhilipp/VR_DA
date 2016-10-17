namespace Aardvark.VR

open Valve.VR

open Aardvark.Base

[<AutoOpen>]
module Conversions =
    type HmdMatrix44_t with
        member x.Trafo =
            let t = M44f(x.m0,x.m1,x.m2,x.m3,x.m4,x.m5,x.m6,x.m7,x.m8,x.m9,x.m10,x.m11,x.m12,x.m13,x.m14,x.m15) 
            let t = M44d.op_Explicit(t)
            Trafo3d(t,t.Inverse)

    type HmdMatrix34_t with
        member x.Trafo =
            let m44f = 
                M44f(
                    x.m0,x.m4,x.m8, 0.0f,
                    x.m1,x.m5,x.m9, 0.0f,
                    x.m2,x.m6,x.m10, 0.0f,
                    x.m3,x.m7,x.m11, 1.0f
                ) 
            let t = M44d.op_Explicit(m44f.Transposed)
            Trafo3d(t,t.Inverse)

