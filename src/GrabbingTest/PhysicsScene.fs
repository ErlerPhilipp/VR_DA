﻿namespace Aardvark.VR

open Aardvark.Base

module PhysicsScene =
    open LogicalSceneTypes
    open PhysicsSceneTypes
    open LogicalScene
    open BulletHelper

    open BulletSharp
    open BulletSharp.Math

    let mutable currentWorld = None
    let debugDrawer = BulletHelper.DebugDrawer()

    let mutable callbackMessages = System.Collections.Generic.List()

    let maxPenetrationDepth = -0.001f
    
    let release(firstController : bool, s : Scene, collisionObject : RigidBody, pb : PhysicsBody, o : Object) = 
        collisionObject.ClearForces()
        // activate by setting normal mass
        collisionObject.SetMassProps(o.mass, pb.inertia)
        collisionObject.CollisionFlags <- collisionObject.CollisionFlags &&& ~~~(BulletSharp.CollisionFlags.StaticObject ||| BulletSharp.CollisionFlags.KinematicObject)

        // set object velocity to hand velocity
        let controllerDevice = if firstController then VrDriver.inputDevices.controller1 else VrDriver.inputDevices.controller2
        let vel = s.trackingToWorld.Forward.TransformDir(controllerDevice.Velocity)
        let interactionInfo = if firstController then s.interactionInfo1 else s.interactionInfo2
        let handVelocity = toVector3(vel)

        collisionObject.LinearVelocity <- handVelocity
//        collisionObject.LinearVelocity <- Vector3()
//        let controllerId = if firstController then s.specialObjectIds.controller1ObjectId else s.specialObjectIds.controller2ObjectId
//        let controllerPos = LogicalSceneTypes.getTrafoOfFirstObjectWithId(controllerId, s.objects).Forward.TransformPos(V3d())
//        let colliderPos = LogicalSceneTypes.getTrafoOfFirstObjectWithId(o.id, s.objects).Forward.TransformPos(V3d())
//        let relativePos = (colliderPos - controllerPos) |> toVector3
//        collisionObject.ApplyImpulse(Vector3.Multiply(handVelocity, o.mass), relativePos)

        let angVel = controllerDevice.AngularVelocity
        let handAngVelocity = toVector3(angVel)
        collisionObject.AngularVelocity <- handAngVelocity
//        collisionObject.AngularVelocity <- Vector3()
//        collisionObject.ApplyTorqueImpulse(handAngVelocity * collisionObject.LocalInertia)
                            
        // reset to normal state
        collisionObject.Activate()
        collisionObject.ForceActivationState(ActivationState.ActiveTag)

    let grab(s : Scene, collisionObject : RigidBody, pb : PhysicsBody) = 
        collisionObject.ClearForces()
        // deactivate by setting infinite mass
        collisionObject.SetMassProps(0.0f, Vector3(0.0f,0.0f,0.0f))
        collisionObject.CollisionFlags <- collisionObject.CollisionFlags ||| BulletSharp.CollisionFlags.StaticObject ||| BulletSharp.CollisionFlags.KinematicObject

        collisionObject.LinearVelocity <- Vector3()
        collisionObject.AngularVelocity <- Vector3()

        // keep always active
        collisionObject.Activate()
        collisionObject.ForceActivationState(ActivationState.DisableDeactivation)
    
    let addContactMessage (contactNormal : V3d) (contactWP : V3d) (obj0Id : int) (obj1Id : int) =
        let found = callbackMessages.Exists 
                        (fun d -> match d with
                                    | CollisionAdded (collider0Id, collider1Id, impulse, contactWorldPos : V3d) ->
                                            (collider0Id = obj0Id && collider1Id = obj1Id) || (collider0Id = obj1Id && collider1Id = obj0Id)
                                    | _ -> false
                        )
        if not found then
//            printfn "CollisionAdded"
            callbackMessages.Add (CollisionAdded(obj0Id, obj1Id, contactNormal, contactWP))

    let processContactPoint (manPoint : ManifoldPoint) (obj0 : CollisionObject) (obj1 : CollisionObject) =
        if manPoint.Distance < -maxPenetrationDepth then
            let impulseStrength = float(manPoint.AppliedImpulse) // Error in Bullet? most of the time zero. only impulse by constraint solver, not collision impulse
            let collisionNormal = toV3d manPoint.NormalWorldOnB
            let contactNormal = collisionNormal
            let contactWP = toV3d(manPoint.PositionWorldOnA)

            let obj0Id = obj0.UserIndex
            let obj1Id = obj1.UserIndex

            addContactMessage contactNormal contactWP obj0Id obj1Id
        
    let contactAdded (manPoint : ManifoldPoint) (collObjA : CollisionObjectWrapper) (partIdA : int) (idA : int) (collObjB : CollisionObjectWrapper) (partIdB : int) (idB : int) = 
        processContactPoint manPoint collObjA.CollisionObject collObjB.CollisionObject
            
    let contactProcessed (manPoint : ManifoldPoint) (collObjA : CollisionObject) (collObjB : CollisionObject) = 
        processContactPoint manPoint collObjA collObjB

    let subStep (dynWorld : DynamicsWorld) (dt : float32) = 
        let numManifolds = dynWorld.Dispatcher.NumManifolds
        for i in 0..numManifolds-1 do
            let manifold = dynWorld.Dispatcher.GetManifoldByIndexInternal(i)
            let numContacts = manifold.NumContacts

            if numContacts > 0 then
                let bodyA = manifold.Body0
                let bodyB = manifold.Body1
                let objAId = bodyA.UserIndex
                let objBId = bodyB.UserIndex

                let mutable averageImpulse = 0.0f
                let mutable averageNormal = Vector3()
                let mutable averageWP = Vector3()
                for c in 0..numContacts-1 do
                    let contact = manifold.GetContactPoint(c)
                    averageImpulse <- averageImpulse + contact.AppliedImpulse
                    averageNormal <- averageNormal + contact.NormalWorldOnB
                    averageWP <- averageWP + contact.PositionWorldOnB
                
                averageImpulse <- averageImpulse / float32 numContacts
                let averageNormalV3d = toV3d(averageNormal) / (float numContacts) * (float averageImpulse)
                let averagePositionV3d = toV3d(averageWP) / (float numContacts)
            
                addContactMessage averageNormalV3d averagePositionV3d objAId objBId

    // Replay changes into physics world....
    type Conversion private() =
        static member Create(o : Object, scene : PhysicsWorld) : PhysicsBody =
            let initialTrafo = o.trafo |> toMatrix

            match o.collisionShape with
                | Some (collisionShape) -> 
                    let setProperties(collObj : BulletSharp.CollisionObject, o : Object) =
                        if not o.isColliding then collObj.CollisionFlags <- collObj.CollisionFlags ||| CollisionFlags.NoContactResponse
                        if o.collisionCallback then collObj.CollisionFlags <- collObj.CollisionFlags ||| CollisionFlags.CustomMaterialCallback
                        collObj.UserIndex <- o.id
                        collObj.Friction <- float32 o.friction
                        collObj.RollingFriction <- float32 o.rollingFriction
                        collObj.CollisionShape <- collisionShape
                        collObj.WorldTransform <- toMatrix o.trafo
                        collObj.Restitution <- float32 o.restitution
                        // continuous collision detection
                        collObj.CcdMotionThreshold <- float32 o.ccdSpeedThreshold
                        collObj.CcdSweptSphereRadius <- float32 o.ccdSphereRadius

                    match o.objectType with
                        | ObjectTypes.Static ->
                            let collObj = new BulletSharp.CollisionObject()
                            setProperties(collObj, o)
                            scene.dynamicsWorld.AddCollisionObject(collObj, o.collisionGroup, o.collisionMask)
                            { 
                                original = o
                                collisionObject = StaticBody collObj 
                                inertia = Vector3.Zero
                                trafo = initialTrafo
                            }
                        | ObjectTypes.Dynamic -> 
                            let inertia = collisionShape.CalculateLocalInertia(o.mass)
                            let state = new MyMotionState()
                            let info = new BulletSharp.RigidBodyConstructionInfo(o.mass, state, collisionShape, inertia)
                            
                            let rigidBody = new BulletSharp.RigidBody(info)
                            rigidBody.SetDamping(0.1f, 0.1f)
                            setProperties(rigidBody, o)
                            rigidBody.LinearVelocity <- (o.linearVelocity |> toVector3)
                            rigidBody.AngularVelocity <- (o.angularVelocity |> toVector3)
                                
                            scene.dynamicsWorld.AddRigidBody(rigidBody, o.collisionGroup, o.collisionMask)
                            let body =  { 
                                            original = o
                                            collisionObject = CollisionObject.RigidBody rigidBody 
                                            inertia = inertia
                                            trafo = initialTrafo
                                        } 
                            state.init body
                            body
                        | ObjectTypes.Ghost ->
                            let ghost = new BulletSharp.PairCachingGhostObject()
                            setProperties(ghost, o)
                            ghost.CollisionFlags <- ghost.CollisionFlags ||| BulletSharp.CollisionFlags.StaticObject ||| BulletSharp.CollisionFlags.KinematicObject
                            scene.dynamicsWorld.AddCollisionObject(ghost, o.collisionGroup, o.collisionMask)
                            { 
                                original = o
                                collisionObject = Ghost ghost 
                                inertia = Vector3.Zero
                                trafo = initialTrafo
                            }
                        | ObjectTypes.Kinematic ->
                            let inertia = collisionShape.CalculateLocalInertia(o.mass)
                            let state = new MyMotionState()
                            let info = new BulletSharp.RigidBodyConstructionInfo(o.mass, state, collisionShape, inertia)
                            
                            let rigidBody = new BulletSharp.RigidBody(info)
                            rigidBody.SetDamping(0.1f, 0.1f)
                            setProperties(rigidBody, o)
                            rigidBody.CollisionFlags <- rigidBody.CollisionFlags ||| BulletSharp.CollisionFlags.StaticObject ||| BulletSharp.CollisionFlags.KinematicObject
                            rigidBody.ForceActivationState(ActivationState.DisableDeactivation)
                                
                            scene.dynamicsWorld.AddRigidBody(rigidBody, o.collisionGroup, o.collisionMask)
                            let body =  { 
                                            original = o
                                            collisionObject = CollisionObject.RigidBody rigidBody 
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
            dynWorld.Gravity <- toVector3(s.physicsInfo.gravity)
            dynWorld.DebugDrawer <- debugDrawer

            // make bounce reliable, no added impulse from penetration
            dynWorld.SolverInfo.SplitImpulse <- 1
            dynWorld.SolverInfo.SplitImpulsePenetrationThreshold <- 0.02f

            let ghostCB = new BulletSharp.GhostPairCallback();
            broad.OverlappingPairCache.SetInternalGhostPairCallback(ghostCB)

            dynWorld.SetInternalTickCallback(DynamicsWorld.InternalTickCallback(subStep))
//            PersistentManifold.add_ContactProcessed(ContactProcessedEventHandler(contactProcessed))
//            ManifoldPoint.add_ContactAdded(ContactAddedEventHandler(contactAdded))
            
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
                    if co.RollingFriction <> o.rollingFriction then co.RollingFriction <- o.rollingFriction
                    match o.collisionShape with
                        | Some shape -> if co.CollisionShape <> shape then co.CollisionShape <- shape
                        | None -> ()
                    if co.Restitution <> o.restitution then co.Restitution <- o.restitution
                    if co.CcdMotionThreshold <> o.ccdSpeedThreshold then co.CcdMotionThreshold <- o.ccdSpeedThreshold
                    if co.CcdSweptSphereRadius <> o.ccdSphereRadius then co.CcdSweptSphereRadius <- o.ccdSphereRadius
                    
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
                let newGravity = toVector3 s.physicsInfo.gravity
                if oldGravity <> newGravity then 
                    pw.dynamicsWorld.SetGravity(ref newGravity)
                    for co in pw.dynamicsWorld.CollisionObjectArray do
                        co.Activate()

                if s.physicsInfo.physicsDebugDraw && debugDrawer.DebugMode <> BulletSharp.DebugDrawModes.DrawWireframe then 
                    debugDrawer.DebugMode <- BulletSharp.DebugDrawModes.DrawWireframe
                else if not s.physicsInfo.physicsDebugDraw && debugDrawer.DebugMode <> BulletSharp.DebugDrawModes.None then 
                    debugDrawer.DebugMode <- BulletSharp.DebugDrawModes.None
                    
                for message in s.physicsMessages do
                    match message with
                        | PhysicsMessage.Grab (objectId, firstController) -> 
                            for pb in pw.bodies do
                                if pb.original.id = objectId then
                                   match pb.collisionObject with
                                    | CollisionObject.RigidBody collisionObject -> 
                                        grab(s, collisionObject, pb)
                                    | CollisionObject.StaticBody collisionObject -> ()
                                    | CollisionObject.Ghost collisionObject -> ()
                                    | CollisionObject.NoObject -> ()
                        | PhysicsMessage.Release (objectId, firstController) -> 
                            for pb in pw.bodies do
                                if pb.original.id = objectId then
                                   match pb.collisionObject with
                                    | CollisionObject.RigidBody collisionObject -> 
                                        release(firstController, s, collisionObject, pb, pb.original)
                                    | CollisionObject.StaticBody collisionObject -> ()
                                    | CollisionObject.Ghost collisionObject -> ()
                                    | CollisionObject.NoObject -> ()

    let simulationSw = System.Diagnostics.Stopwatch()
    let stepSimulation (dt : System.TimeSpan) (s : Scene) : Scene =
        match currentWorld with
            | Some world -> 
                Conversion.Update(world,s)
                simulationSw.Restart()
                world.dynamicsWorld.StepSimulation(float32 dt.TotalSeconds, s.physicsInfo.numSubSteps, float32 s.physicsInfo.subStepTime) |> ignore
                simulationSw.Stop()
//                System.Console.WriteLine(simulationSw.MicroTime.ToString())

                let mutable newScene = s

                for message in callbackMessages do
                    newScene <- LogicalScene.update newScene message

                callbackMessages.Clear()

                let mutable ghostMessages = System.Collections.Generic.List()

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
                                | CollisionObject.Ghost ghost -> 
                                    // see Bullet3\src\BulletDynamics\Character\btCharacterController.cpp line ~220
                                    let overlappingPairs = ghost.OverlappingPairCache.OverlappingPairArray
                                    for pairIndex in 0..(overlappingPairs.Count - 1) do
                                        let broadphasePair = overlappingPairs.[pairIndex]
                                        let collisionPair = world.dynamicsWorld.PairCache.FindPair(broadphasePair.Proxy0, broadphasePair.Proxy1)

                                        let obj0 = collisionPair.Proxy0.ClientObject :?> BulletSharp.CollisionObject
                                        let obj1 = collisionPair.Proxy1.ClientObject :?> BulletSharp.CollisionObject

                                        let firstBodyIsGhost = obj0 = (ghost :> BulletSharp.CollisionObject)
                                        let collidingBody = if firstBodyIsGhost then obj1 else obj0
                                        let collidingObjectId = collidingBody.UserIndex
                                        let ghostBody = if firstBodyIsGhost then obj0 else obj1
                                        let ghostObjectId = ghostBody.UserIndex
                                        
                                        let mutable hasContact = false

                                        let bpPairAlgorithm = collisionPair.Algorithm
                                        if not (isNull bpPairAlgorithm) then
                                            let contactManifoldArray = new BulletSharp.AlignedManifoldArray()
                                            bpPairAlgorithm.GetAllContactManifolds(contactManifoldArray)

                                            for manifoldIndex in 0..(contactManifoldArray.Count - 1) do
                                                if not hasContact then
                                                    let manifold = contactManifoldArray.[manifoldIndex]
                                                    let firstBodyIsGhost = manifold.Body0 = (ghost :> BulletSharp.CollisionObject)
                                                    let directionSign = if firstBodyIsGhost then -1.0f else 1.0f

                                                    let numContactPoints = manifold.NumContacts
                                                    for contactPointIndex in 0..(numContactPoints - 1) do
                                                        if not hasContact then
                                                            let contactPoint = manifold.GetContactPoint(contactPointIndex)
                                                            let dist = contactPoint.Distance
                                                            if dist < -maxPenetrationDepth then
                                                                hasContact <- true

                                            if hasContact then
                                                ghostMessages.Add (Collision(ghostObjectId, collidingObjectId))

//                                                let o = LogicalSceneTypes.getObjectWithId(collidingObjectId, s.objects)
//                                                let firstController = ghostObjectId = s.specialObjectIds.grabTrigger1Id
//                                                let secondController = ghostObjectId = s.specialObjectIds.grabTrigger2Id
//                                                if firstController || secondController then
//                                                    let interactionInfo = if firstController then s.interactionInfo1 else s.interactionInfo2
//                                                    if interactionInfo.triggerPressed && o.isGrabbed = GrabbedOptions.NoGrab then
//                                                        let pb = getBodyWithId(o.id, world.bodies)
//                                                        match pb.collisionObject with
//                                                            | CollisionObject.RigidBody collisionObject -> 
//                                                                grab(s, collisionObject, pb, o)
//                                                            | _ -> ()
                                    
//                                    // from BulletSharp.GhostObject
//                                    let numOverlappingObjects = ghostObject.NumOverlappingObjects
//                                    for i in 0..(numOverlappingObjects - 1) do
//                                        let userObject = ghostObject.GetOverlappingObject(i).UserObject :?> Object
//                                        if userObject.objectType = ObjectTypes.Dynamic then
//                                            //printfn "collision with ghost: %A" userObject.id
//                                            ghostMessages <- ghostMessages @ [Collision(transformedObject, userObject)].v
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

                newScene <- { newScene with objects = PersistentHashSet.ofList objects }

                for message in ghostMessages do
                    newScene <- LogicalScene.update newScene message

                newScene
            | None -> 
                currentWorld <- Conversion.Create s |> Some
                s
