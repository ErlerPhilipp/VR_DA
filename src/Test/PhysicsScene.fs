namespace Aardvark.VR

open Aardvark.Base

module PhysicsScene =
    open LogicalScene
    open BulletHelper

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

    let mutable currentWorld = None
    let debugDrawer = BulletHelper.DebugDrawer()

    // Replay changes into physics world....
    type Conversion private() =
        static member Create(o : Object, scene : PhysicsWorld) : PhysicsBody =
            let initialTrafo = o.trafo |> toMatrix

            match o.collisionShape with
                | Some (collisionShape) -> 
                    let cshape = toCollisionShape collisionShape

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
                                trafo = initialTrafo
                            }
                        | ObjectTypes.Dynamic -> 
                            let inertia = cshape.CalculateLocalInertia(o.mass)
                            let state = new MyMotionState()
                            let info = new BulletSharp.RigidBodyConstructionInfo(o.mass, state, cshape, inertia)
                            
                            let rigidBody = new BulletSharp.RigidBody(info)
                            rigidBody.SetDamping(0.1f, 0.1f)
                            setProperties(rigidBody, o)
                            rigidBody.LinearVelocity <- (o.linearVelocity |> toVector3)
                            rigidBody.AngularVelocity <- (o.angularVelocity |> toVector3)
                                
                            scene.dynamicsWorld.AddRigidBody(rigidBody)
                            let body =  { 
                                            original = o
                                            collisionObject = RigidBody rigidBody 
                                            inertia = inertia
                                            trafo = initialTrafo
                                        } 
                            state.init body
                            body
                        | ObjectTypes.Ghost ->
                            let ghost = new BulletSharp.PairCachingGhostObject()
                            setProperties(ghost, o)
                            scene.dynamicsWorld.AddCollisionObject(ghost)
                            { 
                                original = o
                                collisionObject = Ghost ghost 
                                inertia = Vector3.Zero
                                trafo = initialTrafo
                            }
                        | ObjectTypes.Kinematic ->
                            let inertia = cshape.CalculateLocalInertia(o.mass)
                            let state = new MyMotionState()
                            let info = new BulletSharp.RigidBodyConstructionInfo(o.mass, state, cshape, inertia)
                            
                            let rigidBody = new BulletSharp.RigidBody(info)
                            rigidBody.SetDamping(0.1f, 0.1f)
                            setProperties(rigidBody, o)
                            rigidBody.CollisionFlags <- rigidBody.CollisionFlags ||| BulletSharp.CollisionFlags.StaticObject ||| BulletSharp.CollisionFlags.KinematicObject
                            rigidBody.ForceActivationState(ActivationState.DisableDeactivation)
                                
                            scene.dynamicsWorld.AddRigidBody(rigidBody)
                            let body =  { 
                                            original = o
                                            collisionObject = RigidBody rigidBody 
                                            inertia = inertia
                                            trafo = initialTrafo
                                        } 
                            state.init body
                            body
                | None -> { original = o; collisionObject = CollisionObject.NoObject; inertia = Vector3.Zero; trafo = initialTrafo }


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

        static member Update(pb : PhysicsBody, o : Object, s : Scene) =
            if not (System.Object.ReferenceEquals(pb.original, o)) then
                pb.original <- o

                let newWorldTransform = toMatrix o.trafo
                if pb.trafo <> newWorldTransform then
                    pb.trafo <- newWorldTransform

                let updateCollisionObjectSettings(co : BulletSharp.CollisionObject, o : Object) =
                    if co.Friction <> o.friction then co.Friction <- o.friction
                    if co.Restitution <> o.restitution then co.Restitution <- o.restitution
                    if co.CcdMotionThreshold <> o.ccdSpeedThreshold then co.CcdMotionThreshold <- o.ccdSpeedThreshold
                    if co.CcdSweptSphereRadius <> o.ccdSphereRadius then co.CcdSweptSphereRadius <- o.ccdSphereRadius
                    if co.RollingFriction <> o.rollingFriction then co.RollingFriction <- o.rollingFriction
                    
                    let newWorldTransform = toMatrix o.trafo
                    if co.WorldTransform <> newWorldTransform then
                        co.WorldTransform <- newWorldTransform
                        co.Activate()
                
                match pb.collisionObject with
                    | CollisionObject.RigidBody collisionObject -> 
                        updateCollisionObjectSettings(collisionObject :> BulletSharp.CollisionObject, o)
                        let newLinearVelocity = o.linearVelocity |> toVector3
                        if collisionObject.LinearVelocity <> newLinearVelocity then 
                            collisionObject.LinearVelocity <- newLinearVelocity
                            collisionObject.Activate()
                        let newAngularVelocity = o.angularVelocity |> toVector3
                        if collisionObject.AngularVelocity <> newAngularVelocity then 
                            collisionObject.AngularVelocity <- newAngularVelocity
                            collisionObject.Activate()

                        // release grab
                        if (o.wasGrabbed && not o.isGrabbed) then
                            collisionObject.ClearForces()
                            // activate by setting normal mass
                            collisionObject.SetMassProps(o.mass, pb.inertia)
                            collisionObject.CollisionFlags <- collisionObject.CollisionFlags &&& ~~~(BulletSharp.CollisionFlags.StaticObject ||| BulletSharp.CollisionFlags.KinematicObject)

                            // set object velocity to hand velocity
                            let vel = VrDriver.inputDevices.controller2.Velocity
                            let vel = s.deviceOffset.Forward.TransformDir(vel)
                            let handVelocity = 
                                match s.interactionType with
                                    | VrInteractions.VrInteractionTechnique.VirtualHand -> toVector3(vel)
                                    | VrInteractions.VrInteractionTechnique.GoGo -> 
                                        // TODO: make more accurate, function of difference with last step
                                        //printfn "release object vel %A -> vel %A" vel (vel * s.armExtensionFactor)
                                        toVector3(vel * s.armExtensionFactor)
                                    | _ -> failwith "not implemented"

                            collisionObject.LinearVelocity <- handVelocity

                            let angVel = VrDriver.inputDevices.controller2.AngularVelocity
                            let handAngVelocity = toVector3(angVel)
                            collisionObject.AngularVelocity <- handAngVelocity
                            
                            // reset to normal state
                            collisionObject.Activate()
                            collisionObject.ForceActivationState(ActivationState.ActiveTag)
                        
                        // grab
                        else if not o.wasGrabbed && o.isGrabbed then
                            collisionObject.ClearForces()
                            // deactivate by setting infinite mass
                            collisionObject.SetMassProps(0.0f, Vector3(0.0f,0.0f,0.0f))
                            collisionObject.CollisionFlags <- collisionObject.CollisionFlags ||| BulletSharp.CollisionFlags.StaticObject ||| BulletSharp.CollisionFlags.KinematicObject

                            collisionObject.LinearVelocity <- Vector3()
                            collisionObject.AngularVelocity <- Vector3()

                            // keep always active
                            collisionObject.Activate()
                            collisionObject.ForceActivationState(ActivationState.DisableDeactivation)
                    | CollisionObject.StaticBody collisionObject -> 
                        updateCollisionObjectSettings(collisionObject, o)
                        // TODO: make static objects grabbable?
                    | CollisionObject.Ghost collisionObject -> 
                        updateCollisionObjectSettings(collisionObject :> BulletSharp.CollisionObject, o)
                    | CollisionObject.NoObject -> ()

        static member Update(pw : PhysicsWorld, s : Scene) =
            if not (System.Object.ReferenceEquals(pw.original, s)) then
                pw.original <- s
                
                let table = 
                    pw.bodies |> Seq.map (fun mm -> mm.original.id, mm) |> Dict.ofSeq
                
                for t in s.objects |> PersistentHashSet.toSeq do
                    match table.TryRemove t.id with
                        | (true, mo) -> 
                            Conversion.Update(mo, t, s)
                        | _ ->
                            let mo = Conversion.Create(t,pw)
                            pw.bodies.Add mo |> ignore

                for pb in table.Values do
                   // remove from world 
                   match pb.collisionObject with
                    | CollisionObject.RigidBody collisionObject -> 
                        pw.dynamicsWorld.RemoveRigidBody(collisionObject)
                        pw.bodies.Remove pb |> ignore
                    | CollisionObject.StaticBody collisionObject -> 
                        pw.dynamicsWorld.RemoveCollisionObject(collisionObject)
                        pw.bodies.Remove pb |> ignore
                    | CollisionObject.Ghost collisionObject -> 
                        pw.dynamicsWorld.RemoveCollisionObject(collisionObject)
                        pw.bodies.Remove pb |> ignore
                    | CollisionObject.NoObject -> ()

                   pw.bodies.Remove pb |> ignore

                let mutable oldGravity = Vector3.Zero
                pw.dynamicsWorld.GetGravity(&oldGravity)
                let newGravity = toVector3 s.gravity
                if oldGravity <> newGravity then pw.dynamicsWorld.SetGravity(ref newGravity)

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

                let mutable messages = System.Collections.Generic.List()

                let objects =
                    [
                        for b in world.bodies do
                            let newTrafo = b.trafo |> toTrafo
                            let transformedObject = 
                                if b.original.trafo <> newTrafo then
                                    { b.original with trafo = newTrafo }
                                else
                                    b.original
                            
                            match b.collisionObject with
                                | CollisionObject.Ghost ghostObject -> 
                                    // see Bullet3\src\BulletDynamics\Character\btCharacterController.cpp line ~220
                                    let overlappingPairs = ghostObject.OverlappingPairCache.OverlappingPairArray
                                    for pairIndex in 0..(overlappingPairs.Count - 1) do
                                        let broadphasePair = overlappingPairs.[pairIndex]
                                        let collisionPair = world.dynamicsWorld.PairCache.FindPair(broadphasePair.Proxy0, broadphasePair.Proxy1)

                                        let obj0 = collisionPair.Proxy0.ClientObject :?> BulletSharp.CollisionObject
                                        let obj1 = collisionPair.Proxy1.ClientObject :?> BulletSharp.CollisionObject

                                        let firstBodyIsGhost = obj0 = (ghostObject :> BulletSharp.CollisionObject)
                                        let collidingBody = if firstBodyIsGhost then obj1 else obj0
                                        let collidingObject = collidingBody.UserObject :?> Object
                                        
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
                                                            let maxPenetrationDepth = -0.001f
                                                            if dist < -maxPenetrationDepth then
                                                                hasContact <- true

                                            if hasContact then
                                                messages.Add (Collision(transformedObject, collidingObject, toV3d collidingBody.InterpolationLinearVelocity))
                                    
//                                    // from BulletSharp.GhostObject
//                                    let numOverlappingObjects = ghostObject.NumOverlappingObjects
//                                    for i in 0..(numOverlappingObjects - 1) do
//                                        let userObject = ghostObject.GetOverlappingObject(i).UserObject :?> Object
//                                        if userObject.objectType = ObjectTypes.Dynamic then
//                                            //printfn "collision with ghost: %A" userObject.id
//                                            messages <- messages @ [Collision(transformedObject, userObject)].v
                                    yield transformedObject
                                | CollisionObject.RigidBody rigidBody -> 
                                    // only rigidbodies should be moved by the physics
                                    let linearVelocity = rigidBody.LinearVelocity |> toV3d
                                    let angularVelocity = rigidBody.AngularVelocity |> toV3d
                                    if transformedObject.linearVelocity <> linearVelocity || transformedObject.angularVelocity <> angularVelocity then
                                        yield { transformedObject with
                                                    linearVelocity = linearVelocity
                                                    angularVelocity = angularVelocity
                                              }
                                    else
                                        yield transformedObject
                                | CollisionObject.StaticBody collisionObject -> 
                                    yield transformedObject
                                | CollisionObject.NoObject -> 
                                    yield transformedObject
                    ]

                world.dynamicsWorld.DebugDrawWorld()

                let mutable newScene = { s with objects = PersistentHashSet.ofList objects }

                for message in messages do
                    newScene <- LogicalScene.update newScene message
                   
                if s.wantsRayCast then
                    let (hasHit, hitPoint, hitNormal) = BulletHelper.rayCast(toVector3(s.rayCastStart), toVector3(s.rayCastEnd), world.dynamicsWorld)
                    let rayCastMsg = RayCastResult (hasHit, toV3d(hitPoint), toV3d(hitNormal))
                    newScene <- LogicalScene.update newScene rayCastMsg

                newScene
            | None -> 
                currentWorld <- Conversion.Create s |> Some
                s
