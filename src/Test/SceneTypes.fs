﻿namespace Aardvark.VR

open Audio

module LogicalSceneTypes = 
    open Aardvark.Base
    open Aardvark.SceneGraph
    open Aardvark.Base.Rendering

    open FShade

    let mutable currentId = 0
    let newId() = 
        currentId <- currentId + 1
        currentId

    type ObjectTypes = 
        | Static        // static collider, never moving
        | Dynamic       // moved by physics
        | Ghost         // tracks its collisions
        | Kinematic     // not moved by physics but by game logic

//    type pset<'a> = PersistentHashSet<'a>

    type GrabbedOptions = 
        | NoGrab
        | Controller1
        | Controller2

    type GrabbableOptions = 
        | NoGrab
        | Controller1
        | Controller2
        | BothControllers
    
    [<ReferenceEquality;NoComparison>]
    type Object =
        {
            id                : int
            
            castsShadow       : bool
            objectType        : ObjectTypes
            isColliding       : bool
            isManipulable     : bool
            isGrabbable       : GrabbableOptions
            isGrabbed         : GrabbedOptions
            wasGrabbed        : GrabbedOptions
            hitUpperTrigger   : bool
            hitLowerTrigger   : bool
            hasScored         : bool
            willReset         : bool
            timeToReset       : float
            collisionCallback : bool

            trafo             : Trafo3d
            model             : Option<ISg>
            surface           : Option<ISurface>
            tilingFactor      : V2d

            linearVelocity    : V3d
            angularVelocity   : V3d
            mass              : float32
            restitution       : float32
            friction          : float32
            ccdSpeedThreshold : float32
            ccdSphereRadius   : float32
            rollingFriction   : float32
            collisionShape    : Option<BulletHelper.Shape> 
            collisionGroup    : int16
            collisionMask     : int16
        }

    let defaultObject = 
        {
            id = newId()

            castsShadow         = true
            objectType          = ObjectTypes.Static
            isColliding         = true
            isManipulable       = false
            isGrabbable         = GrabbableOptions.NoGrab
            isGrabbed           = GrabbedOptions.NoGrab
            wasGrabbed          = GrabbedOptions.NoGrab
            hitUpperTrigger     = false
            hitLowerTrigger     = false
            hasScored           = false
            willReset           = false
            timeToReset         = 0.0
            collisionCallback   = false

            trafo               = Trafo3d.Identity
            model               = None
            surface             = None
            tilingFactor        = V2d(1.0, 1.0)
            
            linearVelocity      = V3d()
            angularVelocity     = V3d()
            mass                = 0.0f
            restitution         = 0.0f
            friction            = 1.0f
            ccdSpeedThreshold   = 0.0f // ccd disabled
            ccdSphereRadius     = 0.0f
            rollingFriction     = 0.0f
            collisionShape      = None
            collisionGroup      = 0xFFFFs
            collisionMask       = 0xFFFFs
        }

    type SpecialObjectIds =
        {
            cam1ObjectId        : int
            cam2ObjectId        : int
            controller1ObjectId : int
            controller2ObjectId : int
            grabTrigger1Id      : int
            grabTrigger2Id      : int
            headId              : int
            lowerHoopTriggerId  : int
            upperHoopTriggerId  : int
            lightId             : int
            groundTriggerId     : int
            hoopObjectId        : int
        }

    type RaycastInfo = 
        {
            wantsRayCast        : bool
            rayCastStart        : V3d
            rayCastEnd          : V3d
            rayCastHasHit       : bool
            rayCastHitPoint     : V3d
            rayCastHitNormal    : V3d
        }

    let DefaultRaycastInfo = 
        {
            wantsRayCast        = false
            rayCastStart        = V3d()
            rayCastEnd          = V3d()
            rayCastHasHit       = false
            rayCastHitPoint     = V3d()
            rayCastHitNormal    = V3d()
        }

    type InteractionInfo =
        {
            interactionType     : VrInteractions.VrInteractionTechnique
            armExtensionFactor  : float
            moveDirection       : V3d
            lastContrTrafo      : Trafo3d
            raycastInfo         : RaycastInfo
            vibrationStrength   : float
            vibStrLastFrame     : float
            triggerPressed      : bool
        }

    let DefaultInteractionInfo = 
        {
            interactionType     = VrInteractions.VrInteractionTechnique.VirtualHand
            armExtensionFactor  = 1.0
            moveDirection       = V3d.Zero
            lastContrTrafo      = Trafo3d.Identity
            raycastInfo         = DefaultRaycastInfo
            vibrationStrength   = 0.0
            vibStrLastFrame     = 0.0
            triggerPressed      = false
        }

    type GameInfo = 
        {
            score               : int
            warmupScore         : int
            timeSinceStart      : float
            scoreTrafo          : Trafo3d
            scoreStartTrafo     : Trafo3d
            scoreText           : string
            running             : bool
            numRounds           : int
            hoopStartTrafo      : Trafo3d
            upperTriggerTrafo   : Trafo3d
            lowerTriggerTrafo   : Trafo3d
            goalAreaSize        : float
            goalMovementPhase   : float
        }

    let DefaultGameInfo = 
        {
            score               = 0
            warmupScore         = 0
            timeSinceStart      = 0.0
            scoreTrafo          = Trafo3d()
            scoreStartTrafo     = Trafo3d()
            scoreText           = "Score: 000\r\nTime: 000.00"
            running             = false
            numRounds           = 0
            hoopStartTrafo      = Trafo3d()
            upperTriggerTrafo   = Trafo3d()
            lowerTriggerTrafo   = Trafo3d()
            goalAreaSize        = 0.0
            goalMovementPhase   = 0.0
        }

    type PhysicsInfo = 
        {
            deltaTime           : float
            enablePhysics       : bool
            physicsDebugDraw    : bool
            gravity             : V3d
            numSubSteps         : int
            subStepTime         : float
            
            raycastCollGroup    : int16
            raycastCollMask     : int16
        }

    let DefaultPhysicsInfo = 
        {
            deltaTime           = 0.0
            enablePhysics       = true
            gravity             = V3d(0.0, -9.81, 0.0)
            physicsDebugDraw    = false
            numSubSteps         = 6
            subStepTime         = 1.0 / 180.0
            raycastCollGroup    = 0xFFFFs
            raycastCollMask     = 0xFFFFs
        }

    type Scene =
        {
            objects             : PersistentHashSet<Object>
            viewTrafo           : Trafo3d
            trackingToWorld     : Trafo3d
            ballResetPos        : V3d
            lightColor          : V3d
            
            bounceSoundSources  : list<Audio.Sound>
            sireneSoundSource   : Audio.Sound
            popSoundSource      : Audio.Sound
            
            rayCastDirSg        : ISg
            rayCastHitPointSg   : ISg
            rayCastHitAreaSg    : ISg
            rayCastCamSg        : ISg
            
            specialObjectIds    : SpecialObjectIds
            interactionInfo1    : InteractionInfo
            interactionInfo2    : InteractionInfo
            gameInfo            : GameInfo
            physicsInfo         : PhysicsInfo

            enableExperimental  : bool
            shortVersion        : bool
        }
        
    let setTrafoOfObjectsWithId(id : int, t : Trafo3d, objects : PersistentHashSet<Object>, dt : float) = 
        let newObjects = objects |> PersistentHashSet.map (fun o -> 
            if o.id = id then
                let newTrafo = t
                if o.objectType <> ObjectTypes.Dynamic then
                    let linVel = (newTrafo.Forward.TransformPos(V3d()) - o.trafo.Forward.TransformPos(V3d())) / dt
                    {o with 
                        trafo = newTrafo
                        linearVelocity = linVel
                    }
                else
                    {o with trafo = newTrafo}
            else
                o
            ) 
        newObjects

    let transformTrafoOfObjectsWithId(id : int, t : Trafo3d, objects : PersistentHashSet<Object>, dt : float) = 
        let newObjects = objects |> PersistentHashSet.map (fun o -> 
            if o.id = id then
                let newTrafo = o.trafo * t
                if o.objectType <> ObjectTypes.Dynamic then
                    let linVel = (newTrafo.Forward.TransformPos(V3d()) - o.trafo.Forward.TransformPos(V3d())) / dt
                    {o with 
                        trafo = newTrafo
                        linearVelocity = linVel
                    }
                else
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
        | Collision of int * int
        | CollisionAdded of int * int * V3d * V3d
//        | CollisionProcessed of int * int * float * V3d
        | RayCastResult of int * bool * V3d * V3d
        
module PhysicsSceneTypes = 
    open LogicalSceneTypes
    open BulletSharp
    open BulletSharp.Math

    type CollisionObject =
        | StaticBody     of collObj     : BulletSharp.CollisionObject
        | RigidBody      of rb          : BulletSharp.RigidBody
        | Ghost          of ghost       : BulletSharp.PairCachingGhostObject
        | NoObject
    
    [<ReferenceEquality;NoComparison>]
    type PhysicsBody = 
        { 
            mutable original    : Object
            collisionObject     : CollisionObject
            inertia             : Vector3
            mutable trafo       : Matrix
        }
        
    type MyMotionState() =
        inherit MotionState()

        let mutable body = None
        member this.init(newBody : PhysicsBody) =
            body <- Some newBody

        override this.GetWorldTransform(worldTrans : Matrix byref) = 
            match body with
                | Some body -> worldTrans <- body.trafo
                | None -> worldTrans <- Matrix()

        override this.SetWorldTransform(worldTrans : Matrix byref) = 
            match body with
                | Some body -> body.trafo <- worldTrans
                | None -> ()
            
    
    [<ReferenceEquality;NoComparison>]
    type PhysicsWorld =
        {
            mutable original : Scene
            
            collisionConf    : DefaultCollisionConfiguration
            collisionDisp    : CollisionDispatcher
            broadPhase       : DbvtBroadphase
            dynamicsWorld    : DiscreteDynamicsWorld

            mutable bodies   : System.Collections.Generic.HashSet<PhysicsBody>
        }
        
    let getBodyWithId(id : int, bodies : System.Collections.Generic.HashSet<PhysicsBody>) =
        let objectWithId = 
            bodies |> Seq.fold ( fun found current -> if current.original.id = id then Some current else found) None
        
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
            mtrafo              : ModRef<Trafo3d>
            mmodel              : ModRef<Option<ISg>>
            mhasHighlight       : ModRef<bool>
            mscoredState        : ModRef<int>
            mtilingFactor       : ModRef<V2d>
        }

    type RaycastMods = 
        {
            hasRayCastHit       : ModRef<bool>
            drawRayCastDir      : ModRef<bool>
            drawHitPoint        : ModRef<bool>
            drawHitArea         : ModRef<bool>
            hasRayCastDir       : ModRef<Trafo3d>
            rayCastHitTrafo     : ModRef<Trafo3d>
            rayCastCam          : ModRef<Trafo3d>
        }

    type GraphicsScene =
        {
            mutable original    : Scene
            graphicsObjects     : cset<GraphicsObject>
            viewTrafo           : ModRef<Trafo3d>
            lightPos            : ModRef<V3d>
            lightColor          : ModRef<V3d>

            scoreTrafo          : ModRef<Trafo3d>
            scoreText           : ModRef<string>
            
            raycastMods1        : RaycastMods
            raycastMods2        : RaycastMods
        }
            