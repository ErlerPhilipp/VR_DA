namespace Aardvark.VR

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
    let GeometryPass = RenderPass.after "Geo" RenderPassOrder.Arbitrary HighlightPass
    let SelectionVolumePass = RenderPass.after "selvol" RenderPassOrder.Arbitrary GeometryPass
    let ThumbPass = RenderPass.after "thumb" RenderPassOrder.Arbitrary SelectionVolumePass

module SelectionVolume =
    type InstanceVertex = { 
            [<Position>] pos : V4d 
            [<Color>] col : V4d
            [<InstanceTrafo>] trafo : M44d
        }
        
    let selectionVolumeRadius = 0.065
    let private tesselationLevel = 5
    let controllerToRingCenter = V3d(0.0, -0.03, -0.02)

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

        let selectionVolumeEffect = [
                                        instancedTrafo |> toEffect
                                        DefaultSurfaces.constantColor (C4f(0.3f, 0.3f, 0.9f, 0.3f)) |> toEffect
                                    ]
        vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, selectionVolumeEffect) :> ISurface

    let makeSelectionVolumeSurface(vrWin : VrWindow.VrWindow) = 
        let defaultTrafoEffect = DefaultSurfaces.trafo |> toEffect
        let selectionVolumeEffect = [
                                        defaultTrafoEffect
                                        DefaultSurfaces.constantColor (C4f(0.3f, 0.3f, 0.9f, 0.3f)) |> toEffect
                                    ]
        vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, selectionVolumeEffect) :> ISurface

    let selectionVolumeSg =
        let selectionVolumeSg = Sg.sphere tesselationLevel (Mod.constant C4b.Green) (Mod.constant selectionVolumeRadius)
        let selectionVolumeSg = 
                selectionVolumeSg 
                |> Sg.blendMode(Mod.constant (BlendMode(true))) 
                |> Sg.writeBuffers (Some (Set.singleton DefaultSemantic.Colors))
                |> Sg.pass (Renderpasses.SelectionVolumePass)
        selectionVolumeSg