namespace Aardvark.VR

open Aardvark.Base
open Aardvark.Base.Incremental

module PhysicsScene =
    open LogicalScene
    open BulletConversions

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
