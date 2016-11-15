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
        | Ghost          of ghost   : BulletSharp.GhostObject
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

                    let createRB (info : RigidBodyConstructionInfo) : RigidBody = 
                        let rigidBody = new BulletSharp.RigidBody(info)
                        rigidBody.Friction <- float32 o.friction
                        rigidBody.Restitution <- float32 o.restitution
                        // keep all rb active to enable collisions with grabbed (static) objects
                        // TODO: add collisiondispatcher etc, ghost object of grabbed object, activate colliding object if necessary
                        rigidBody.Activate()
                        rigidBody.ForceActivationState(ActivationState.DisableDeactivation)
                        // continuous collision detection
                        rigidBody.CcdMotionThreshold <- float32 o.ccdSpeedThreshold
                        rigidBody.CcdSweptSphereRadius <- float32 o.ccdSphereRadius
                        rigidBody.RollingFriction <- float32 o.rollingFriction
                        rigidBody.SetDamping(0.1f, 0.1f)
                        rigidBody.UserObject <- o
                        rigidBody

                    match o.objectType with
                        | ObjectTypes.Static ->
                            let collObj = new BulletSharp.CollisionObject()
                            collObj.CollisionShape <- cshape
                            collObj.UserObject <- o
                            collObj.WorldTransform <- toMatrix o.trafo
                            //collObj.CollisionFlags <- collObj.CollisionFlags ||| BulletSharp.CollisionFlags.KinematicObject
                            scene.dynamicsWorld.AddCollisionObject(collObj)
                            { 
                                original = o
                                collisionObject = StaticBody collObj 
                                inertia = Vector3.Zero
                            }
                        | ObjectTypes.Dynamic -> 
                            let inertia = cshape.CalculateLocalInertia(o.mass)
                            let info = new BulletSharp.RigidBodyConstructionInfo(o.mass, state, cshape, inertia)
                            let rigidBody = createRB(info)
                            scene.dynamicsWorld.AddRigidBody(rigidBody)
                            { 
                                original = o
                                collisionObject = RigidBody rigidBody 
                                inertia = inertia
                            }
                        | ObjectTypes.Ghost ->
                            let ghost = new BulletSharp.GhostObject()
                            ghost.UserObject <- o
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
            let broad = new DbvtBroadphase()
            let dynWorld = new DiscreteDynamicsWorld(dispatcher, broad, null, collConf)
            dynWorld.Gravity <- toVector3(s.gravity)
            dynWorld.DebugDrawer <- debugDrawer

            // make bounce reliable, no added impulse from penetration
            dynWorld.SolverInfo.SplitImpulse <- 1
            dynWorld.SolverInfo.SplitImpulsePenetrationThreshold <- 0.02f

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
                            let handVelocity = toVector3(vel)
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
                        // TODO: make static objects grabbable?
                    | CollisionObject.Ghost collisionObject -> 
                        ()
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
                System.Console.WriteLine(simulationSw.MicroTime.ToString())
                let objects =
                    [
                        for b in world.bodies do
                            match b.collisionObject with
                                | CollisionObject.RigidBody collisionObject -> 
                                    match b.original.collisionShape with
                                        | Some (c) -> 
                                            let newTrafo = toTrafo collisionObject.WorldTransform
                                            //printfn "collisionObject %A pos: %A" (b.original.id) (newTrafo.Forward.TransformPos(V3d()))

                                            yield { b.original with trafo = newTrafo }
                                        | None ->
                                            yield b.original

                                // those shouldn't be moved by the physics
                                | CollisionObject.StaticBody collisionObject -> 
                                    yield b.original
                                | CollisionObject.Ghost collisionObject -> 
                                    yield b.original
                                | CollisionObject.NoObject -> 
                                    yield b.original
                    ]

                world.dynamicsWorld.DebugDrawWorld()

                { s with
                    objects = PersistentHashSet.ofList objects
                }
            | None -> 
                currentWorld <- Conversion.Create s |> Some
                s
