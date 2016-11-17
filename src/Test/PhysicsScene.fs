namespace Aardvark.VR

open Aardvark.Base

module PhysicsScene =
    open LogicalScene
    open BulletHelper

    open BulletSharp
    open BulletSharp.Math

    type CollisionObject =
        | StaticBody     of collObj : BulletSharp.CollisionObject
        | RigidBody      of rb      : BulletSharp.RigidBody
        | Ghost          of ghost   : BulletSharp.PairCachingGhostObject
        | NoObject
    
    [<ReferenceEquality;NoComparison>]
    type PhysicsBody = 
        { 
            mutable original    : Object
            collisionObject     : CollisionObject
            inertia             : Vector3
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
    let debugDrawer = BulletHelper.DebugDrawer()

    // Replay changes into physics world....
    type Conversion private() =
        static member Create(o : Object, scene : PhysicsWorld) : PhysicsBody =
            match o.collisionShape with
                | Some (collisionShape) -> 
                    let cshape = toCollisionShape collisionShape
                    let state = new BulletSharp.DefaultMotionState(toMatrix o.trafo)

                    let setProperties(collObj : BulletSharp.CollisionObject, o : Object) =
                        if not o.isColliding then collObj.CollisionFlags <- collObj.CollisionFlags ||| BulletSharp.CollisionFlags.NoContactResponse
                        collObj.UserObject <- o
                        collObj.UserIndex <- o.id
                        collObj.Friction <- float32 o.friction
                        collObj.RollingFriction <- float32 o.rollingFriction
                        collObj.CollisionShape <- cshape
                        collObj.WorldTransform <- toMatrix o.trafo
                        collObj.Restitution <- float32 o.restitution
                        // continuous collision detection
                        collObj.CcdMotionThreshold <- float32 o.ccdSpeedThreshold
                        collObj.CcdSweptSphereRadius <- float32 o.ccdSphereRadius

                    match o.objectType with
                        | ObjectTypes.Static ->
                            let collObj = new BulletSharp.CollisionObject()
                            setProperties(collObj, o)
                            scene.dynamicsWorld.AddCollisionObject(collObj)
                            { 
                                original = o
                                collisionObject = StaticBody collObj 
                                inertia = Vector3.Zero
                            }
                        | ObjectTypes.Dynamic -> 
                            let inertia = cshape.CalculateLocalInertia(o.mass)
                            let info = new BulletSharp.RigidBodyConstructionInfo(o.mass, state, cshape, inertia)
                            
                            let rigidBody = new BulletSharp.RigidBody(info)
                            // keep all rb active to enable collisions with grabbed (static) objects
                            // TODO: add collisiondispatcher etc, ghost object of grabbed object, activate colliding object if necessary
                            rigidBody.Activate()
                            rigidBody.ForceActivationState(ActivationState.DisableDeactivation)
                            rigidBody.SetDamping(0.1f, 0.1f)
                            setProperties(rigidBody, o)

                            scene.dynamicsWorld.AddRigidBody(rigidBody)
                            { 
                                original = o
                                collisionObject = RigidBody rigidBody 
                                inertia = inertia
                            }
                        | ObjectTypes.Ghost ->
                            let ghost = new BulletSharp.PairCachingGhostObject()
                            setProperties(ghost, o)
                            scene.dynamicsWorld.AddCollisionObject(ghost)
                            { 
                                original = o
                                collisionObject = Ghost ghost 
                                inertia = Vector3.Zero
                            }

                | None -> { original = o; collisionObject = CollisionObject.NoObject; inertia = Vector3.Zero }

        static member Create(s : Scene) : PhysicsWorld =
            let collConf = new DefaultCollisionConfiguration()
            let dispatcher = new CollisionDispatcher(collConf)
            GImpactCollisionAlgorithm.RegisterAlgorithm(dispatcher)
            let broad = new DbvtBroadphase()
            let solver = new SequentialImpulseConstraintSolver()
            let dynWorld = new DiscreteDynamicsWorld(dispatcher, broad, solver, collConf)
            dynWorld.Gravity <- toVector3(s.gravity)
            dynWorld.DebugDrawer <- debugDrawer

            // make bounce reliable, no added impulse from penetration
            dynWorld.SolverInfo.SplitImpulse <- 1
            dynWorld.SolverInfo.SplitImpulsePenetrationThreshold <- 0.02f

            let ghostCB = new BulletSharp.GhostPairCallback();
            broad.OverlappingPairCache.SetInternalGhostPairCallback(ghostCB)

            let scene = { original = s; collisionConf = collConf; collisionDisp = dispatcher; broadPhase = broad; dynamicsWorld = dynWorld; bodies = null;  }
            scene.bodies <- HashSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map (fun o -> Conversion.Create(o,scene)))
            scene

        static member Update(m : PhysicsBody, o : Object, s : Scene) =
            if not (System.Object.ReferenceEquals(m.original, o)) then
                
                match m.collisionObject with
                    | CollisionObject.RigidBody collisionObject -> 
                        if collisionObject.Friction <> o.friction then collisionObject.Friction <- o.friction
                        if collisionObject.Restitution <> o.restitution then collisionObject.Restitution <- o.restitution
                        if collisionObject.CcdMotionThreshold <> o.ccdSpeedThreshold then collisionObject.CcdMotionThreshold <- o.ccdSpeedThreshold
                        if collisionObject.CcdSweptSphereRadius <> o.ccdSphereRadius then collisionObject.CcdSweptSphereRadius <- o.ccdSphereRadius
                        if collisionObject.RollingFriction <> o.rollingFriction then collisionObject.RollingFriction <- o.rollingFriction

                        let newWorldTransform = toMatrix o.trafo
                        let worldTransformChanged = collisionObject.WorldTransform <> newWorldTransform
                        if worldTransformChanged then
                            collisionObject.WorldTransform <- newWorldTransform

                        if (o.wasGrabbed && not o.isGrabbed) then
                            collisionObject.ClearForces()
                            // activate by setting normal mass
                            collisionObject.SetMassProps(o.mass, m.inertia)
//                            collisionObject.CollisionFlags <- BulletSharp.CollisionFlags.None
                            
                        if (o.wasGrabbed && not o.isGrabbed) || worldTransformChanged then
                            // set object velocity to hand velocity
                            let vel = VrDriver.inputDevices.controller2.Velocity
                            let handVelocity = 
                                match s.interactionType with
                                    | VrInteractions.VrInteractionTechnique.VirtualHand -> toVector3(vel)
                                    | VrInteractions.VrInteractionTechnique.GoGo -> toVector3(vel * s.armExtensionFactor)
                                    | _ -> failwith "not implemented"

                            collisionObject.LinearVelocity <- handVelocity

                            let angVel = VrDriver.inputDevices.controller2.AngularVelocity
                            let handAngVelocity = toVector3(angVel)
                            collisionObject.AngularVelocity <- handAngVelocity
                            
                            // reset to normal state
//                            collisionObject.Activate()
//                            collisionObject.ForceActivationState(ActivationState.ActiveTag)

                        else if not o.wasGrabbed && o.isGrabbed then
                            collisionObject.ClearForces()
                            // deactivate by setting infinite mass
                            collisionObject.SetMassProps(0.0f, Vector3(0.0f,0.0f,0.0f))
//                            collisionObject.CollisionFlags <- BulletSharp.CollisionFlags.StaticObject ||| BulletSharp.CollisionFlags.KinematicObject

                            collisionObject.LinearVelocity <- Vector3()
                            collisionObject.AngularVelocity <- Vector3()

                            // keep always active
//                            collisionObject.Activate()
//                            collisionObject.ForceActivationState(ActivationState.DisableDeactivation)

                        m.original <- o
                    | CollisionObject.StaticBody collisionObject -> 
                        if collisionObject.Friction <> o.friction then collisionObject.Friction <- o.friction
                        if collisionObject.Restitution <> o.restitution then collisionObject.Restitution <- o.restitution
                        if collisionObject.CcdMotionThreshold <> o.ccdSpeedThreshold then collisionObject.CcdMotionThreshold <- o.ccdSpeedThreshold
                        if collisionObject.CcdSweptSphereRadius <> o.ccdSphereRadius then collisionObject.CcdSweptSphereRadius <- o.ccdSphereRadius
                        if collisionObject.RollingFriction <> o.rollingFriction then collisionObject.RollingFriction <- o.rollingFriction

                        let newWorldTransform = toMatrix o.trafo
                        let worldTransformChanged = collisionObject.WorldTransform <> newWorldTransform
                        if worldTransformChanged then
                            collisionObject.WorldTransform <- newWorldTransform
                        // TODO: make static objects grabbable?
                    | CollisionObject.Ghost collisionObject -> 
                        let newWorldTransform = toMatrix o.trafo
                        let worldTransformChanged = collisionObject.WorldTransform <> newWorldTransform
                        if worldTransformChanged then
                            collisionObject.WorldTransform <- newWorldTransform
                    | CollisionObject.NoObject -> 
                        ()

        static member Update(m : PhysicsWorld, s : Scene) =
            if not (System.Object.ReferenceEquals(m.original, s)) then
                m.original <- s
                
                let table = 
                    m.bodies |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                for t in s.objects |> PersistentHashSet.toSeq do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t, s)
                        | _ ->
                            let mo = Conversion.Create(t,m)
                            m.bodies.Add mo |> ignore

                for pb in table.Values do
                   // remove from world 
                   match pb.collisionObject with
                    | CollisionObject.RigidBody collisionObject -> 
                        m.dynamicsWorld.RemoveRigidBody(collisionObject)
                        m.bodies.Remove pb |> ignore
                    | CollisionObject.StaticBody collisionObject -> 
                        m.dynamicsWorld.RemoveCollisionObject(collisionObject)
                        m.bodies.Remove pb |> ignore
                    | CollisionObject.Ghost collisionObject -> 
                        m.dynamicsWorld.RemoveCollisionObject(collisionObject)
                        m.bodies.Remove pb |> ignore
                    | CollisionObject.NoObject -> ()

                   m.bodies.Remove pb |> ignore

                let mutable oldGravity = Vector3.Zero
                m.dynamicsWorld.GetGravity(&oldGravity)
                let newGravity = toVector3 s.gravity
                if oldGravity <> newGravity then m.dynamicsWorld.SetGravity(ref newGravity)

                if s.physicsDebugDraw && debugDrawer.DebugMode <> BulletSharp.DebugDrawModes.DrawWireframe then 
                    debugDrawer.DebugMode <- BulletSharp.DebugDrawModes.DrawWireframe
                else if not s.physicsDebugDraw && debugDrawer.DebugMode <> BulletSharp.DebugDrawModes.None then 
                    debugDrawer.DebugMode <- BulletSharp.DebugDrawModes.None


    let simulationSw = System.Diagnostics.Stopwatch()
    let stepSimulation (dt : System.TimeSpan) (s : Scene) : Scene =
        match currentWorld with
            | Some world -> 
                Conversion.Update(world,s)
                simulationSw.Restart()
                world.dynamicsWorld.StepSimulation(float32 dt.TotalSeconds, s.numSubSteps, float32 s.subStepTime) |> ignore
                simulationSw.Stop()
//                System.Console.WriteLine(simulationSw.MicroTime.ToString())
                
                let mutable messages : list<Message> = []

                let objects =
                    [
                        for b in world.bodies do
                            match b.collisionObject with
                                | CollisionObject.RigidBody rigidBody -> 
                                    match b.original.collisionShape with
                                        | Some (c) -> 
                                            let newTrafo = toTrafo rigidBody.WorldTransform
                                            //printfn "rigidBody %A pos: %A" (b.original.id) (newTrafo.Forward.TransformPos(V3d()))

                                            yield { b.original with trafo = newTrafo }
                                        | None ->
                                            yield b.original

                                // those shouldn't be moved by the physics
                                | CollisionObject.StaticBody collisionObject -> 
                                    yield b.original
                                | CollisionObject.Ghost ghostObject -> 
                                    // see Bullet3\src\BulletDynamics\Character\btCharacterController.cpp line ~220
                                    let overlappingPairs = ghostObject.OverlappingPairCache.OverlappingPairArray
                                    for pairIndex in 0..(overlappingPairs.Count - 1) do
                                        let broadphasePair = overlappingPairs.[pairIndex]
                                        let collisionPair = world.dynamicsWorld.PairCache.FindPair(broadphasePair.Proxy0, broadphasePair.Proxy1)

                                        let obj0 = collisionPair.Proxy0.ClientObject :?> BulletSharp.CollisionObject
                                        let obj1 = collisionPair.Proxy1.ClientObject :?> BulletSharp.CollisionObject

                                        let firstBodyIsGhost = obj0 = (ghostObject :> BulletSharp.CollisionObject)
                                        let collidingObject = (if firstBodyIsGhost then obj1.UserObject else obj0.UserObject) :?> Object
                                        
                                        let mutable hasContact = false

                                        let bpPairAlgorithm = collisionPair.Algorithm
                                        if not (isNull bpPairAlgorithm) then
                                            let contactManifoldArray = new BulletSharp.AlignedManifoldArray()
                                            bpPairAlgorithm.GetAllContactManifolds(contactManifoldArray)

                                            for manifoldIndex in 0..(contactManifoldArray.Count - 1) do
                                                if not hasContact then
                                                    let manifold = contactManifoldArray.[manifoldIndex]
                                                    let firstBodyIsGhost = manifold.Body0 = (ghostObject :> BulletSharp.CollisionObject)
                                                    let directionSign = if firstBodyIsGhost then -1.0f else 1.0f

                                                    let numContactPoints = manifold.NumContacts
                                                    for contactPointIndex in 0..(numContactPoints - 1) do
                                                        if not hasContact then
                                                            let contactPoint = manifold.GetContactPoint(contactPointIndex)
                                                            let dist = contactPoint.Distance
                                                            let maxPenetrationDepth = 0.0f
                                                            if dist < -maxPenetrationDepth then
                                                                hasContact <- true

                                            if hasContact then
                                                messages <- messages @ [Collision(b.original, collidingObject)]
                                    
//                                    // from BulletSharp.GhostObject
//                                    let numOverlappingObjects = ghostObject.NumOverlappingObjects
//                                    for i in 0..(numOverlappingObjects - 1) do
//                                        let userObject = ghostObject.GetOverlappingObject(i).UserObject :?> Object
//                                        if userObject.objectType = ObjectTypes.Dynamic then
//                                            //printfn "collision with ghost: %A" userObject.id
//                                            messages <- messages @ [Collision(b.original, userObject)]
                                    yield b.original
                                | CollisionObject.NoObject -> 
                                    yield b.original
                    ]

                world.dynamicsWorld.DebugDrawWorld()

                let mutable newScene = { s with objects = PersistentHashSet.ofList objects }

                for message in messages do
                    newScene <- LogicalScene.update newScene message

                newScene
            | None -> 
                currentWorld <- Conversion.Create s |> Some
                s
