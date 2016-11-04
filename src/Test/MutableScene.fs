﻿namespace Aardvark.VR

open Valve.VR

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph


[<AutoOpen>]
module ImmutableScene =

    type pset<'a> = PersistentHashSet<'a>
    type Object =
        {
            id              : int
            isManipulable   : bool
            isGrabbed         : bool
            wasGrabbed      : bool
            boundingBox     : Box3d
            trafo           : Trafo3d
            model           : ISg
            mass            : Mass
            collisionShape  : Option<Shape> 
        }

    type Scene =
        {
            things            : pset<Object>
            viewTrafo         : Trafo3d
            lastTrafo         : Trafo3d
            
            cam1Object        : Object
            cam2Object        : Object
            controller1Object : Object
            controller2Object : Object

            interactionType   : VrInteractionTechnique

            moveDirection     : V3d
        }

module Physics =

    open BulletSharp
    open BulletSharp.Math
    
    [<ReferenceEquality;NoComparison>]
    type PhysicsBody = 
        { 
            mutable original : Object
            body : Option<BulletSharp.RigidBody>
            inertia : Vector3
        }

    
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

    let mutable currentWorld = None


    // Replay changes into physics world....
    type Conversion private() =
        static member Create(o : Object, scene : PhysicsWorld) : PhysicsBody =
            // aha we have new object, tell bullet we have a new object
            match o.collisionShape with
                | Some (collisionShape) -> 
                    let cshape = toCollisionShape collisionShape
                    //let inner = Trafo3d.Scale(1.0, 4.0, 1.0) // Trafo3d.Identity
                    let state = new BulletSharp.DefaultMotionState(toMatrix o.trafo)

                    match o.mass with
                        | Infinite ->
                            let info = new BulletSharp.RigidBodyConstructionInfo(0.0f, state, cshape)
                            let rigidBody = new BulletSharp.RigidBody(info)
                            scene.dynamicsWorld.AddCollisionObject(rigidBody)
                            { 
                                original = o
                                body = Some rigidBody 
                                inertia = Vector3.Zero
                            }
                        | Mass m -> 
                            let inertia = cshape.CalculateLocalInertia(m)
                            let info = new BulletSharp.RigidBodyConstructionInfo(m, state, cshape, inertia)
                            let rigidBody = new BulletSharp.RigidBody(info)
                            scene.dynamicsWorld.AddRigidBody(rigidBody)
                            { 
                                original = o
                                body = Some rigidBody 
                                inertia = inertia
                            }

                | None -> { original = o; body = None; inertia = Vector3.Zero }

        static member Create(s : Scene) : PhysicsWorld =
            let collConf = new DefaultCollisionConfiguration()
            let dispatcher = new CollisionDispatcher(collConf)
            let broad = new DbvtBroadphase()
            let dynWorld = new DiscreteDynamicsWorld(dispatcher, broad, null, collConf)
            dynWorld.Gravity <- Vector3(0.0f, -9.81f, 0.0f)
            let scene ={ original = s; collisionConf = collConf; collisionDisp = dispatcher;  broadPhase = broad; dynamicsWorld = dynWorld; bodies = null }
            scene.bodies <- HashSet.ofSeq (PersistentHashSet.toSeq s.things |> Seq.map (fun o -> Conversion.Create(o,scene)))
            scene

        static member Update(m : PhysicsBody, o : Object) =
            if not (System.Object.ReferenceEquals(m.original, o)) then
                match o.collisionShape, m.body with
                    | Some (collisionShape), Some body -> 
                        //printfn "%A" (o.trafo.Forward.TransformPos(V3d.OOO))
                        //let invMass = if o.mass != Mass.Infinite then 1.0f / Mass.toFloat o.mass else 0.0f
                        if (o.wasGrabbed && not o.isGrabbed) then // || (body.InvMass <> invMass) then
                            body.SetMassProps(Mass.toFloat o.mass, m.inertia)

                            // move with hand velocity
                            let vel = VrDriver.inputDevices.controller2.Velocity
                            let handVelocity = Vector3(float32 vel.X, float32 vel.Y, float32 vel.Z)
                            body.LinearVelocity <- handVelocity

                            let angVel = VrDriver.inputDevices.controller2.AngularVelocity
                            let handAngVelocity = Vector3(float32 angVel.X, float32 angVel.Y, float32 angVel.Z)
                            body.AngularVelocity <- handAngVelocity

                            //body.ApplyCentralImpulse(handVelocity * Mass.toFloat o.mass)
                            if o.id = 5 then printfn "released, set velocity to %A" vel
                            body.Activate()
                        else if not o.wasGrabbed && o.isGrabbed then
                            body.SetMassProps(0.0f, Vector3(0.0f,0.0f,0.0f))
                            body.LinearVelocity <- Vector3()
                            body.AngularVelocity <- Vector3()

                        body.WorldTransform <- toMatrix o.trafo
                        m.original <- o
                    | _ -> failwith "not yet implemented...."

        static member Update(m : PhysicsWorld, s : Scene) =
            if not (System.Object.ReferenceEquals(m.original, s)) then
                m.original <- s
                
                let table = 
                    m.bodies |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                for t in s.things |> PersistentHashSet.toSeq do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t)
                        | _ ->
                            let mo = Conversion.Create(t,m)
                            m.bodies.Add mo |> ignore

                for pb in table.Values do
                   // remove from world 
                   match pb.body with
                    | Some rb ->
                        m.dynamicsWorld.RemoveRigidBody(rb)
                        m.bodies.Remove pb |> ignore
                    | None -> ()

                   m.bodies.Remove pb |> ignore

                // printfn "%A" (m.bodies)


    let stepSimulation (dt : System.TimeSpan) (s : Scene) : Scene =
        match currentWorld with
            | Some world -> 
                Conversion.Update(world,s)
                world.dynamicsWorld.StepSimulation(float32 dt.TotalSeconds) |> ignore

                let objects =
                    [
                        for b in world.bodies do
                            //if b.original.id = 5 then printfn "%A" (b.body.Value.LinearVelocity)
                            match b.body with
                                | Some body -> 
                                    match b.original.collisionShape with
                                        | Some (c) -> 
                                            let newTrafo = toTrafo body.WorldTransform

                                            yield { b.original with trafo = newTrafo }
                                        | None ->
                                            yield b.original
                                | None -> 
                                    yield b.original
                    ]

                { s with
                    things = PersistentHashSet.ofList objects
                }
            | None -> 
                currentWorld <- Conversion.Create s |> Some
                s

[<AutoOpen>]
module MutableScene =

    let mutable currentId = 0
    let newId() = 
        currentId <- currentId + 1
        currentId

    type MObject =
        {
            mutable original : Object
            mtrafo : ModRef<Trafo3d>
            mmodel : ModRef<ISg>
        }

    type MScene =
        {
            mutable original : Scene
            mthings          : cset<MObject>
            mviewTrafo       : ModRef<Trafo3d>
            
            mcam1Object        : ModRef<MObject>
            mcam2Object        : ModRef<MObject>
            mcontroller1Object : ModRef<MObject>
            mcontroller2Object : ModRef<MObject>
        }
        
    let virtualHandColor = Mod.init (C4f.White)

    type Conversion private() =
        static member Create(o : Object) =
            {
                original = o
                mtrafo = Mod.init o.trafo
                mmodel = Mod.init o.model
            }

        static member Create(s : Scene) =
            {
                original = s
                mthings = CSet.ofSeq (PersistentHashSet.toSeq s.things |> Seq.map Conversion.Create)
                mviewTrafo = Mod.init s.viewTrafo

                mcam1Object = s.cam1Object |> Conversion.Create |> Mod.init
                mcam2Object = s.cam2Object |> Conversion.Create |> Mod.init
                mcontroller1Object = s.controller1Object |> Conversion.Create |> Mod.init
                mcontroller2Object = s.controller2Object |> Conversion.Create |> Mod.init
            }

        static member Update(m : MObject, o : Object) =
            if not (System.Object.ReferenceEquals(m.original, o)) then
                m.original <- o
                m.mmodel.Value <- o.model
                m.mtrafo.Value <- o.trafo

        static member Update(m : MScene, s : Scene) =
            if not (System.Object.ReferenceEquals(m.original, s)) then
                m.original <- s

                m.mviewTrafo.Value <- s.viewTrafo
                
                Conversion.Update(m.mcam1Object.Value, s.cam1Object)
                Conversion.Update(m.mcam2Object.Value, s.cam2Object)
                Conversion.Update(m.mcontroller1Object.Value, s.controller1Object)
                Conversion.Update(m.mcontroller2Object.Value, s.controller2Object)

                let table = 
                    m.mthings |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                
                for t in PersistentHashSet.toSeq s.things do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t)
                        | _ ->
                            let mo = Conversion.Create(t)
                            m.mthings.Add mo |> ignore
                
                m.mthings.ExceptWith table.Values
            

    type Message =
        | Add of Object
        | Remove of Object
        | DevicePress of int * int * Trafo3d
        | DeviceRelease of int * int * Trafo3d
        | DeviceTouch of int * int * Trafo3d
        | DeviceUntouch of int * int * Trafo3d
        | DeviceMove of int * Trafo3d
        | TimeElapsed of System.TimeSpan
        | UpdateViewTrafo of Trafo3d

    let change (i : int) (f : 'a -> 'a) (l : list<'a>) =
        l |> List.mapi (fun ii v -> if ii = i then f v else v)

    let filterIndices (set : Set<int>) (l : list<'a>) =
        let rec filterIndices (i : int) (set : Set<int>) (l : list<'a>) =
            match l with
                | [] -> []
                | h :: rest ->
                    if Set.contains i set then h :: filterIndices (i+1) set rest
                    else filterIndices (i+1) set rest
        filterIndices 0 set l

    let update (scene : Scene) (message : Message) : Scene =
        match message with
            | TimeElapsed _ | UpdateViewTrafo _ | DeviceMove _ | DeviceTouch _ | DevicePress _ | DeviceUntouch _ | DeviceRelease _ -> ()
            | _ -> printfn "%A" message

        let getVirtualHandTrafo (t : Trafo3d) = 
            if scene.interactionType = VrInteractionTechnique.VirtualHand then
                t
            else
                let handPos = t.Forward.TransformPos V3d.Zero
                let headPos = scene.viewTrafo.Backward.TransformPos V3d.Zero
                let chestPos = headPos |> Trafo3d.Translation(0.0, -0.2, 0.0).Forward.TransformPos
                let chestToHand = handPos - chestPos
                //printfn "hand: %A, chest: %A" (chestPos) (handPos)
                let headToHandDist = chestToHand.Length
                    
                let linearExtensionLimit = 0.5

                if headToHandDist < linearExtensionLimit then
                    t
                else
                    let quadraticTermFactor = 200.0
                    let quadraticExtension = headToHandDist - linearExtensionLimit
                    let gogoAdditionalExtension = max 0.0 quadraticTermFactor * quadraticExtension * quadraticExtension // R_r + k(R_r - D)^2
                    let gogoHandPosOffset = chestToHand.Normalized * gogoAdditionalExtension
                    let gogoHandTrafo = t * Trafo3d.Translation(gogoHandPosOffset)
                    //printfn "arm length: %A gogo arm length: %A, pos: %A" headToHandDist (1.0+gogoAdditionalExtension) (gogoHandTrafo.GetViewPosition())
                    gogoHandTrafo

        let scene =
            match message with
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                    { scene with 
                        controller1Object = {scene.controller1Object with trafo = t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller2Id ->
                    { scene with 
                        controller2Object = {scene.controller2Object with trafo = getVirtualHandTrafo t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam1Id ->
                    { scene with 
                        cam1Object = {scene.cam1Object with trafo = t}
                    }
                | DeviceMove(deviceId, t) when deviceId = assignedInputs.cam2Id ->
                    { scene with 
                        cam2Object = {scene.cam2Object with trafo = t}
                    }
                | _ -> 
                    scene
        
        let colorForInteractionTechnique(it : VrInteractionTechnique) =
            match it with
                | VrInteractionTechnique.VirtualHand -> C4f.White
                | VrInteractionTechnique.GoGo -> C4f.Green
                | _ -> C4f.White
            

        match message with
            | DevicePress(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 0 ->
                let newInteractionTechnique = nextInteractionTechnique scene.interactionType
                transact ( fun _ -> Mod.change virtualHandColor (colorForInteractionTechnique newInteractionTechnique) )
                { scene with
                    interactionType = newInteractionTechnique
                }
            | DevicePress(deviceId, a, t) when deviceId = assignedInputs.controller2Id && a = 1 ->
                
                let trafo = 
                    if deviceId = assignedInputs.controller2Id then
                        scene.controller2Object.trafo
                    else
                        t
                
                let worldLocation = trafo.Forward.C3.XYZ

                let newThings = scene.things |> PersistentHashSet.map (fun o ->
                
                        if o.isManipulable then
                            let modelLocation = o.trafo.Backward.TransformPos worldLocation
                            if o.boundingBox.Contains modelLocation then
                                { o with isGrabbed = true}
                            else
                                o
                        else
                            o

                    ) 

                { scene with 
                    lastTrafo       = trafo
                    things          = newThings
                }
                    
            | DeviceMove(deviceId, t) when deviceId = assignedInputs.controller1Id ->
                let direction = t.Forward.TransformDir(V3d.OOI)
                { scene with moveDirection = direction }
            | DeviceMove(deviceId, t) ->
                let trafo = 
                    if deviceId = assignedInputs.controller2Id then
                        scene.controller2Object.trafo
                    else
                        t

                if PersistentHashSet.isEmpty scene.things then
                    scene
                else    
                    let deltaTrafo = scene.lastTrafo.Inverse * trafo
                    { scene with 
                        things =
                            scene.things |> PersistentHashSet.map (fun a ->
                                if a.isGrabbed then { a with trafo = a.trafo * deltaTrafo } else a
                            ) 
                        lastTrafo = trafo
                    }
                    
            | DeviceRelease(deviceId, a, _) when deviceId = assignedInputs.controller2Id && a = 1 ->
                let newThings = scene.things |> PersistentHashSet.map (fun a ->
                        if not a.isGrabbed then a else { a with isGrabbed = false }
                    ) 

                { scene with 
                    things = newThings 
                    lastTrafo = Trafo3d.Identity
                }

            | TimeElapsed(dt) ->
                let maxSpeed = 10.0
                    
                let mutable state = VRControllerState_t()
                let axisPosition =
                    if system.GetControllerState(uint32 assignedInputs.controller1Id, &state) then
                        Some (V2d(state.[1].x, state.[1].y))
                    else None

                let axisValue = if axisPosition.IsSome then axisPosition.Value.X else 0.0

                let deathZone = 0.1
                let axisWithDeathZone = clamp 0.0 1.0 (axisValue * (1.0 + deathZone) - deathZone)
                //printfn "axisWithDeathZone: %A" axisWithDeathZone

                let dp = Trafo3d.Translation(scene.moveDirection * dt.TotalSeconds * maxSpeed * axisWithDeathZone)
                { scene with
                    // only move static things, keep active things like controllers
                    things = scene.things |> PersistentHashSet.map (fun o -> 
                        let newTrafo = if o.isGrabbed then o.trafo else o.trafo * dp
                        { o with 
                            trafo = newTrafo; 
                            wasGrabbed = o.isGrabbed 
                        }
                    )
                }

            | UpdateViewTrafo trafo -> 
                { scene with viewTrafo = trafo }

            | _ -> scene


    let createScene (initialScene : Scene) (win : VrWindow) =
        let mutable scene = initialScene
        let mscene = Conversion.Create initialScene

        let perform (msg : Message) =
            transact (fun () ->
                scene <- update scene msg
                Conversion.Update(mscene, scene)
            )

        let deviceCount = VrDriver.devices.Length
        let oldTrafos = Array.zeroCreate deviceCount
        let update (dt : System.TimeSpan) (trafos : Trafo3d[]) (e : VREvent_t) =

            transact (fun () -> 
                scene <- Physics.stepSimulation dt scene 
                Conversion.Update(mscene,scene)
            )

            perform (TimeElapsed dt)

            VrDriver.updateInputDevices( )
            
            for i in 0 .. VrDriver.devices.Length-1 do
                let t = trafos.[i]
                if oldTrafos.[i] <> t then
                    oldTrafos.[i] <- t
                    if i = 0 then 
                        perform (UpdateViewTrafo(t.Inverse)) 
                    else 
                        perform (DeviceMove(i, t))
                
            if e.trackedDeviceIndex >= 0u && e.trackedDeviceIndex < uint32 deviceCount then
                let deviceId = e.trackedDeviceIndex |> int
                let button = int e.data.controller.button |> unbox<EVRButtonId>
                let axis = button - EVRButtonId.k_EButton_Axis0 |> int
                let trafo = trafos.[deviceId]



                match unbox<EVREventType> (int e.eventType) with
                    | EVREventType.VREvent_ButtonPress -> perform(DevicePress(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonUnpress -> perform(DeviceRelease(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonTouch -> perform(DeviceTouch(deviceId, axis, trafo))
                    | EVREventType.VREvent_ButtonUntouch -> perform(DeviceUntouch(deviceId, axis, trafo))
                    | _ -> () //printfn "%A" (e.eventType)
    
            ()

        win.Update <- update

        let toSg (t : MObject) =
            t.mmodel
                |> Sg.dynamic
                |> Sg.trafo t.mtrafo

        let visibleDevices = [mscene.mcontroller1Object.Value; mscene.mcontroller2Object.Value; mscene.mcam1Object.Value; mscene.mcam2Object.Value]
        let objects = 
            visibleDevices |> List.map toSg
                |> Sg.ofList
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.simpleLighting
                   }

        let sgs = 
            mscene.mthings
                |> ASet.map toSg
                |> Sg.set

        Sg.ofList [sgs; objects]
            |> Sg.viewTrafo mscene.mviewTrafo
