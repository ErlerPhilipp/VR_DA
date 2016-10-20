namespace Aardvark.VR

open Aardvark.Base
open Aardvark.SceneGraph

[<AutoOpen>]
module Types =

    type VrDeviceType =
        | Other = 0
        | Hmd = 1
        | Controller = 2
        | TrackingReference = 3

    type VrEye =
        | Left = 1
        | Right = 2
        
    exception VrException of string
    
    type pset<'a> = PersistentHashSet<'a>
    type Object =
        {
            id              : int
            canMove         : bool
            boundingBox     : Box3d
            trafo           : Trafo3d
            model           : ISg
        }

    type Scene =
        {
            activeObjects     : pset<Object>
            things            : pset<Object>
            viewTrafo         : Trafo3d
            lastTrafo         : Trafo3d
            controllerObjects : list<Option<Object>>

        }
        
    let mutable currentId = 0
    let newId() = 
        currentId <- currentId + 1
        currentId
