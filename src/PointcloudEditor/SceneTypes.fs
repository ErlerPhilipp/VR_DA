namespace Aardvark.VR

open Audio
open InteractiveSegmentation
open PointCloudHelper.Rendering.LodData

module LogicalSceneTypes = 
    open Aardvark.Base
    open Aardvark.SceneGraph
    open Aardvark.Base.Rendering

    open FShade

    let mutable currentId = 0
    let newId() = 
        currentId <- currentId + 1
        currentId
            
    type Message =
        | DevicePress of int * int * Trafo3d
        | DeviceRelease of int * int * Trafo3d
        | DeviceTouch of int * int * Trafo3d
        | DeviceUntouch of int * int * Trafo3d
        | DeviceMove of int * Trafo3d
        | StartFrame
        | TimeElapsed of System.TimeSpan
        | EndFrame
        | UpdateViewTrafo of Trafo3d

    type TrackpadActionType =
        | Nop
        | Select
        | Deselect
        | Upscale
        | Downscale
        
    type OperationType =
        | Select
        | Deselect

    type Operation =
        {
            opType                  : OperationType
            selectionVolumeTrafos   : Trafo3d[]
            worldToPointcloud       : Trafo3d
            selectionVolumeRadiusPC : float
        }

    [<ReferenceEquality;NoComparison>]
    type Object =
        {
            id                  : int
                                
            visible             : bool
            castsShadow         : bool
                                
            trafo               : Trafo3d
            model               : Option<ISg>
            surface             : Option<ISurface>
            tilingFactor        : V2d
        }

    let defaultObject = 
        {
            id = newId()
            
            visible             = true
            castsShadow         = true

            trafo               = Trafo3d.Identity
            model               = None
            surface             = None
            tilingFactor        = V2d(1.0, 1.0)
        }

    type SpecialObjectIds =
        {
            cam1ObjectId        : int
            cam2ObjectId        : int
            controller1ObjectId : int
            controller2ObjectId : int
            lightId             : int
            centroidId          : int
            thumbPos1Id         : int
            thumbPos2Id         : int
            selectionVolume1Id  : int
            selectionVolume2Id  : int
        }

    type InteractionInfo =
        {
            lastContrTrafo      : Trafo3d
            vibrationStrength   : float
            vibStrLastFrame     : float
            triggerPressed      : bool
            trackpadPressed     : bool
            selectionVolumePath : Trafo3d[]
            numPointsInSelVol   : int
            currActionType      : TrackpadActionType
            currSelVolScale     : float
        }

    let DefaultInteractionInfo = 
        {
            lastContrTrafo      = Trafo3d.Identity
            vibrationStrength   = 0.0
            vibStrLastFrame     = 0.0
            triggerPressed      = false
            trackpadPressed     = false
            selectionVolumePath = [| |]
            numPointsInSelVol   = 0
            currActionType      = TrackpadActionType.Nop
            currSelVolScale     = 1.0
        }

    type Scene =
        {
            objects             : PersistentHashSet<Object>
            viewTrafo           : Trafo3d
            trackingToWorld     : Trafo3d
            deltaTime           : float
            
            lightColor          : V3d
            scoreTrafo          : Trafo3d
            scoreText           : string
            pointCloudTrafo     : Trafo3d
            initialOctree       : Octree
            currentOctree       : Octree
            contrToTrackpad     : Trafo3d
            allOperations       : Operation[]
            refOperationsFile   : string
            referenceOperations : Operation[]
            timeSinceLastComp   : float
            loadGroundTruth     : bool
            autoCompareInSec   : float
            
            specialObjectIds    : SpecialObjectIds
            interactionInfo1    : InteractionInfo
            interactionInfo2    : InteractionInfo
        }
        
    let setTrafoOfObjectsWithId(id : int, t : Trafo3d, objects : PersistentHashSet<Object>) = 
        let newObjects = objects |> PersistentHashSet.map (fun o -> 
            if o.id = id then
                let newTrafo = t
                {o with trafo = newTrafo}
            else
                o
            ) 
        newObjects

    let transformTrafoOfObjectsWithId(id : int, t : Trafo3d, objects : PersistentHashSet<Object>) = 
        let newObjects = objects |> PersistentHashSet.map (fun o -> 
            if o.id = id then
                let newTrafo = o.trafo * t
                {o with trafo = newTrafo}
            else
                o
            ) 
        newObjects

    let getTrafoOfFirstObjectWithId(id : int, objects : PersistentHashSet<Object>) = 
        let filteredObjects = objects |> PersistentHashSet.filter (fun o -> 
                o.id = id
            )
        if PersistentHashSet.isEmpty filteredObjects then
            failwith "Id not found!"
        else
            (PersistentHashSet.toList filteredObjects).[0].trafo

    let getObjectWithId(id : int, objects : PersistentHashSet<Object>) =
        let objectWithId = 
            objects |> PersistentHashSet.fold ( fun found current -> if current.id = id then Some current else found) None
        
        match objectWithId with
            | Some foundObject -> foundObject
            | None -> failwith(sprintf "Object with id %A not found!" id) 
        
module GraphicsSceneTypes = 
    open LogicalSceneTypes
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph

    type GraphicsObject =
        {
            mutable original    : Object
            trafo               : ModRef<Trafo3d>
            model               : ModRef<Option<ISg>>
            tilingFactor        : ModRef<V2d>
            visible             : ModRef<bool>
        }

    type GraphicsScene =
        {
            mutable original    : Scene
            graphicsObjects     : cset<GraphicsObject>
            viewTrafo           : ModRef<Trafo3d>
            lightPos            : ModRef<V3d>
            lightColor          : ModRef<V3d>
            pointCloudTrafo     : ModRef<Trafo3d>
            selVolPath          : ModRef<Trafo3d[]>
            octree              : ModRef<Octree>

            scoreTrafo          : ModRef<Trafo3d>
            scoreText           : ModRef<string>
        }
            