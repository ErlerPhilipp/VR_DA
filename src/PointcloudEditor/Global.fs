namespace Aardvark.VR

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph.Semantics
open System.IO
open System.Threading

open Aardvark.Git
open Aardvark.Database
open Aardvark.Base.Native

open InteractiveSegmentation

module Global = 
    
    let mutable cameraView  : IMod<CameraView>  = null
    let mutable frustum     : IMod<Frustum>     = null
    let mutable windowSizes : IMod<V2i>         = null
    let mutable viewTrafo   : IMod<Trafo3d>     = null
    let mutable projTrafo   : IMod<Trafo3d>     = null 
    let mutable octree      : IMod<Octree>      = null 


module Interaction = 

    type State =    
        | Picking 
        | Segmenting
        | NoInteraction
    

    let CurrentState : ModRef<State> = Mod.init Picking


    let NextState() = 
        match (CurrentState |> Mod.force) with
            | Picking       -> Segmenting
            | Segmenting    -> NoInteraction
            | NoInteraction -> Picking


    let ChangeState(state : State) =         
        transact (fun () -> Mod.change CurrentState state) 
   



module Input = 
   
    let mutable mouse : IMouse              = Unchecked.defaultof<IMouse>
    let mutable keyboard : IKeyboard        = Unchecked.defaultof<IKeyboard>


module RenderPasses = 
    let mutable overlay = RenderPass.main  |> RenderPass.after "" RenderPassOrder.Arbitrary 