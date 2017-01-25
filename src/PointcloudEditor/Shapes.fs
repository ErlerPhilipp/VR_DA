namespace Aardvark.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

open System

module Shapes =
    type Shape =
        | Box            of size :  V3d
        | Sphere         of radius : float
        | CylinderX      of radius : float * height : float
        | CylinderY      of radius : float * height : float
        | CylinderZ      of radius : float * height : float
        | Plane          of Plane3d
        | Mesh           of IndexedGeometry
        | TriangleMesh   of Triangle3d[]
        | Compound       of list<Trafo3d * Shape>
