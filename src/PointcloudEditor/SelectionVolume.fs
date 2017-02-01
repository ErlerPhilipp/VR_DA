﻿namespace Aardvark.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

open FShade

open System
open SGHelper

module Renderpasses =
    let PointcloudPass = RenderPass.main
    let SelectionPass = RenderPass.after "sel" RenderPassOrder.Arbitrary PointcloudPass
    let HighlightPass = RenderPass.after "high" RenderPassOrder.Arbitrary SelectionPass
    let GeometryPass = RenderPass.after "Geo" RenderPassOrder.Arbitrary SelectionPass
    let SelectionVolumePass = RenderPass.after "selvol" RenderPassOrder.Arbitrary GeometryPass

module SelectionVolume =
    type InstanceVertex = { 
            [<Position>] pos : V4d 
            [<Color>] col : V4d
            //[<InstanceColor>] col : V4d
            [<InstanceTrafo>] trafo : M44d
        }

//    type Shape =
//        | Box            of size :  V3d
//        | Sphere         of radius : float
//        | CylinderX      of radius : float * height : float
//        | CylinderY      of radius : float * height : float
//        | CylinderZ      of radius : float * height : float
//        | Plane          of Plane3d
//        | Mesh           of IndexedGeometry
//        | TriangleMesh   of Triangle3d[]
//        | Compound       of list<Trafo3d * Shape>
        
    let selectionVolumeRadius = 0.065
    let private tesselationLevel = 5

    let makeSelectionVolumeGeometry() = 
        Sphere.get(tesselationLevel)
        
    type UniformScope with
        member x.PointcloudToWorld : M44d = uniform?PointcloudToWorld
        
    let makeSelectionVolumeSurfaceInstance(vrWin : VrWindow.VrWindow) = 
        let instancedTrafo (v : InstanceVertex) =
            vertex {
                let pointcloudToWorld = uniform.PointcloudToWorld
                return {
                    pos     = uniform.ViewProjTrafo * pointcloudToWorld * v.trafo * v.pos
                    col     = v.col
                    trafo   = v.trafo
                }
            }
        let vertexColor (v:InstanceVertex) = fragment {return v.col}

        let defaultTrafoEffect = DefaultSurfaces.instanceTrafo |> toEffect
//        let defaultSimpleLightingEffect = DefaultSurfaces.simpleLighting |> toEffect
        let selectionVolumeEffect = [
//                                        defaultTrafoEffect
                                        instancedTrafo |> toEffect
                                        DefaultSurfaces.constantColor (C4f(0.3f, 0.3f, 0.9f, 0.3f)) |> toEffect
//                                        defaultSimpleLightingEffect
                                    ]
        vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, selectionVolumeEffect) :> ISurface

    let makeSelectionVolumeSurface(vrWin : VrWindow.VrWindow) = 
        let defaultTrafoEffect = DefaultSurfaces.trafo |> toEffect
//        let defaultSimpleLightingEffect = DefaultSurfaces.simpleLighting |> toEffect
        let selectionVolumeEffect = [
                                        defaultTrafoEffect
                                        DefaultSurfaces.constantColor (C4f(0.3f, 0.3f, 0.9f, 0.3f)) |> toEffect
//                                        defaultSimpleLightingEffect
                                    ]
        vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, selectionVolumeEffect) :> ISurface

    let makeSelectionVolumeSg(vrWin : VrWindow.VrWindow) =
        let controllerRingCenter = V3d(0.0, -0.03, -0.02)
        
        let selectionVolumeSg = Sg.sphere tesselationLevel (Mod.constant C4b.Green) (Mod.constant selectionVolumeRadius)
        let selectionVolumeSg = 
                selectionVolumeSg 
                |> Sg.surface (Mod.constant (makeSelectionVolumeSurface(vrWin))) 
                |> Sg.blendMode(Mod.constant (BlendMode(true))) 
                |> Sg.trafo(Mod.constant(Trafo3d.Translation(controllerRingCenter)))
                |> Sg.pass (Renderpasses.SelectionVolumePass)
        selectionVolumeSg