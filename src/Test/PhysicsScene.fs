namespace Aardvark.VR

open Aardvark.Base

module PhysicsScene =
    open LogicalScene
    open BulletHelper

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
    let debugDrawer = BulletHelper.DebugDrawer()

    // Replay changes into physics world....
    type Conversion private() =
        static member Create(o : Object, scene : PhysicsWorld) : PhysicsBody =
            match o.collisionShape with
                | Some (collisionShape) -> 
                    let cshape = toCollisionShape collisionShape
                    let state = new BulletSharp.DefaultMotionState(toMatrix o.trafo)

                    match o.mass with
                        | Infinite ->
                            let info = new BulletSharp.RigidBodyConstructionInfo(0.0f, state, cshape)
                            let rigidBody = new BulletSharp.RigidBody(info)
                            rigidBody.Friction <- 0.25f
                            rigidBody.Restitution <- float32 o.restitution
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
                            
                            // keep all rb active to enable collisions with grabbed (static) objects
                            // TODO: add collisiondispatcher etc, ghost object of grabbed object, activate colliding object if necessary
                            rigidBody.Activate()
                            rigidBody.ForceActivationState(ActivationState.DisableDeactivation)
                            rigidBody.Restitution <- float32 o.restitution
                            rigidBody.Friction <- 0.25f

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
            dynWorld.Gravity <- toVector3(s.gravity)
            dynWorld.DebugDrawer <- debugDrawer
            let scene = { original = s; collisionConf = collConf; collisionDisp = dispatcher;  broadPhase = broad; dynamicsWorld = dynWorld; bodies = null }
            scene.bodies <- HashSet.ofSeq (PersistentHashSet.toSeq s.objects |> Seq.map (fun o -> Conversion.Create(o,scene)))
            scene

        static member Update(m : PhysicsBody, o : Object) =
            if not (System.Object.ReferenceEquals(m.original, o)) then
                match o.collisionShape, m.body with
                    | Some (collisionShape), Some body -> 
                        if (o.wasGrabbed && not o.isGrabbed) then
                            // activate by setting normal mass
                            body.SetMassProps(Mass.toFloat o.mass, m.inertia)

                            // set object velocity to hand velocity
                            let vel = VrDriver.inputDevices.controller2.Velocity
                            let handVelocity = toVector3(vel)
                            body.LinearVelocity <- handVelocity

                            let angVel = VrDriver.inputDevices.controller2.AngularVelocity
                            let handAngVelocity = toVector3(angVel)
                            body.AngularVelocity <- handAngVelocity
                            
                            // reset to normal state
//                            body.Activate()
//                            body.ForceActivationState(ActivationState.ActiveTag)

                        else if not o.wasGrabbed && o.isGrabbed then
                            // deactivate by setting infinite mass
                            body.SetMassProps(0.0f, Vector3(0.0f,0.0f,0.0f))
                            body.LinearVelocity <- Vector3()
                            body.AngularVelocity <- Vector3()

                            // keep always active
//                            body.Activate()
//                            body.ForceActivationState(ActivationState.DisableDeactivation)

                        let newWorldTransform = toMatrix o.trafo
                        if body.WorldTransform <> newWorldTransform then
                            body.WorldTransform <- newWorldTransform
                            
//                            body.Activate()

                        m.original <- o
                    | _ -> failwith "not yet implemented...."

        static member Update(m : PhysicsWorld, s : Scene) =
            if not (System.Object.ReferenceEquals(m.original, s)) then
                m.original <- s
                
                let table = 
                    m.bodies |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                for t in s.objects |> PersistentHashSet.toSeq do
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

                let mutable oldGravity = Vector3.Zero
                m.dynamicsWorld.GetGravity(&oldGravity)
                let newGravity = toVector3 s.gravity
                if oldGravity <> newGravity then m.dynamicsWorld.SetGravity(ref newGravity)

                if s.physicsDebugDraw && debugDrawer.DebugMode <> BulletSharp.DebugDrawModes.DrawWireframe then 
                    debugDrawer.DebugMode <- BulletSharp.DebugDrawModes.DrawWireframe
                else if not s.physicsDebugDraw && debugDrawer.DebugMode <> BulletSharp.DebugDrawModes.None then 
                    debugDrawer.DebugMode <- BulletSharp.DebugDrawModes.None


    let stepSimulation (dt : System.TimeSpan) (s : Scene) : Scene =
        match currentWorld with
            | Some world -> 
                Conversion.Update(world,s)
                //world.dynamicsWorld.StepSimulation(float32 dt.TotalSeconds) |> ignore
                world.dynamicsWorld.StepSimulation(float32 dt.TotalSeconds, 2, 1.0f / 180.0f) |> ignore

                let objects =
                    [
                        for b in world.bodies do
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

                world.dynamicsWorld.DebugDrawWorld()

                { s with
                    objects = PersistentHashSet.ofList objects
                }
            | None -> 
                currentWorld <- Conversion.Create s |> Some
                s
