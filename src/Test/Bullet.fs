module Bullet

open BulletSharp
open BulletSharp.Math

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

open System

[<AutoOpen>]
module Types =
    type Shape =
        | Box of Box3d
        | Sphere of Sphere3d
        | Cylinder of Cylinder3d
        | Plane of Plane3d
        | Mesh of IndexedGeometry

        
    

[<AutoOpen>]
module Conversions =
        
    let toTrafo (m : Matrix) =
        let m44 = 
            M44d(
                float m.M11, float m.M21, float m.M31, float m.M41,
                float m.M12, float m.M22, float m.M32, float m.M42,
                float m.M13, float m.M23, float m.M33, float m.M43,
                float m.M14, float m.M24, float m.M34, float m.M44
            )

        Trafo3d(m44, m44.Inverse)

    let toMatrix (t : Trafo3d) =
        let m = t.Forward
        Matrix(
            float32 m.M00, float32 m.M10, float32 m.M20, float32 m.M30,
            float32 m.M01, float32 m.M11, float32 m.M21, float32 m.M31,
            float32 m.M02, float32 m.M12, float32 m.M22, float32 m.M32,
            float32 m.M03, float32 m.M13, float32 m.M23, float32 m.M33
        )

    let toV3d (v : Vector3) =
        V3d(float v.X, float v.Y, float v.Z)

    let toVector3 (v : V3d) =
        Vector3(float32 v.X, float32 v.Y, float32 v.Z)
        
    let toCollisionShape (s : Shape) : Trafo3d * CollisionShape =
        match s with
            | Box b ->
                let trafo = Trafo3d.Translation(b.Center) 
                let shape = new BoxShape(b.Size * 0.5 |> toVector3) :> CollisionShape
                trafo, shape

            | Sphere s ->
                let trafo = Trafo3d.Translation(s.Center)
                let shape = new SphereShape(float32 s.Radius) :> CollisionShape
                trafo, shape

            | Cylinder c ->
                let trafo = Trafo3d.FromNormalFrame(c.P0, c.Axis.Direction.Normalized)
                let shape = new CylinderShapeZ(float32 c.Radius, float32 c.Radius, float32 (c.Height / 2.0)) :> CollisionShape
                trafo, shape

            | Plane p ->
                let shape = new StaticPlaneShape(toVector3 p.Normal, float32 p.Distance) :> CollisionShape
                Trafo3d.Identity, shape

            | Mesh m ->
              
                let positions =
                    match m.IndexedAttributes.TryGetValue DefaultSemantic.Positions with
                        | (true, arr) ->
                            match arr with
                                | :? array<V3f> as a -> a |> Array.copy
                                | :? array<V4f> as a -> a |> Array.map Vec.xyz
                                | :? array<V3d> as a -> a |> Array.map V3f
                                | :? array<V4d> as a -> a |> Array.map V3f
                                | _ -> failwithf "[Sim] unexpected position-type: %A" arr
                        | _ -> 
                            failwith "[Sim] meshes must contain positions"

                let index = 
                    match m.IndexArray with
                        | null -> null
                        | :? array<uint8> as a -> a |> Array.map int
                        | :? array<int8> as a -> a |> Array.map int
                        | :? array<uint16> as a -> a |> Array.map int
                        | :? array<int16> as a -> a |> Array.map int
                        | :? array<uint32> as a -> a |> Array.map int
                        | :? array<int32> as a -> a
                        | :? array<uint64> as a -> a |> Array.map int
                        | :? array<int64> as a -> a |> Array.map int
                        | _ -> failwithf "[Sim] non-integral index-type: %A" m.IndexArray

                let faceVertexCount =
                    if isNull index then positions.Length
                    else index.Length

                let triangles =
                    match m.Mode with
                        | IndexedGeometryMode.TriangleList -> index
                        | IndexedGeometryMode.TriangleStrip ->
                            let cnt = 3 * (faceVertexCount - 2)
                            let mutable p0 = 0
                            let mutable p1 = 1

                            let res = Array.zeroCreate cnt
                            let mutable ti = 0
                            for p2 in 2 .. faceVertexCount-1 do

                                res.[3 * ti + 0] <- p0
                                res.[3 * ti + 1] <- p1
                                res.[3 * ti + 2] <- p2

                                if ti % 2 = 0 then p0 <- p2
                                else p1 <- p2

                                ti <- ti + 1
                            
                            if isNull index then res
                            else res |> Array.map (fun i -> index.[i])
                        | _ ->
                            [||]


                let mesh = new TriangleMesh()
                

                let positions = positions.UnsafeCoerce<Vector3>()
                if isNull triangles then
                    for i in 0 .. 3 .. positions.Length-1 do
                        mesh.AddTriangle(positions.[i + 0], positions.[i + 1], positions.[i + 2])

                else
                    let ids = positions |> Array.map (fun v -> mesh.FindOrAddVertex(v, false))
                    for i in 0 .. 3 .. triangles.Length-1 do
                        let i0 = ids.[triangles.[i + 0]]
                        let i1 = ids.[triangles.[i + 1]]
                        let i2 = ids.[triangles.[i + 2]]
                        mesh.AddTriangleIndices(i0, i1, i2)

                let shape = new BvhTriangleMeshShape(mesh, true, true) :> CollisionShape

                Trafo3d.Identity, shape


[<AutoOpen>]
module Bullet =

    type Body(shape : Shape, trafo : IMod<Trafo3d>, mass : float, body : RigidBody) =
        member x.Shape = shape
        member x.Trafo = trafo
        member x.Mass = mass

        member x.IsStatic = trafo.IsConstant

    type World(time : IMod<DateTime>) =
        inherit Mod.AbstractMod<int>()

        let config = new DefaultCollisionConfiguration()
        let disp = new CollisionDispatcher(config)
        let broad = new DbvtBroadphase()

        let sw = System.Diagnostics.Stopwatch()

        let sim = new DiscreteDynamicsWorld(disp, broad, null, config)
        do sim.Gravity <- Vector3(0.0f, 0.0f, -9.81f)


        let bodies : cset<Body> = CSet.empty


        member x.Gravity
            with get() = toV3d sim.Gravity
            and set v = sim.Gravity <- toVector3 v

        override x.Compute() =
            time.GetValue x |> ignore
            let res = sim.StepSimulation(float32 sw.Elapsed.TotalSeconds)
            sw.Restart()
            res

        member x.AddBody(trafo : Trafo3d, shape : Shape, mass : float) =
            let inner, cshape = toCollisionShape shape
            let state = new DefaultMotionState(toMatrix (inner * trafo))
            let info = new RigidBodyConstructionInfo(float32 mass, state, cshape, cshape.CalculateLocalInertia(float32 mass))
            let body = new BulletSharp.RigidBody(info)
            sim.AddRigidBody(body)
            
            let trafo = 
                x |> Mod.map (fun _ ->
                    let current = body.WorldTransform |> toTrafo
                    inner.Inverse * current
                )

            let body = Body(shape, trafo, mass, body)
            transact (fun () -> bodies.Add(body) |> ignore)
            body

        member x.AddStatic(trafo : Trafo3d, shape : Shape) =
            let inner, cshape = toCollisionShape shape
            
            let state = new DefaultMotionState(toMatrix (inner * trafo))
            let info = new RigidBodyConstructionInfo(0.0f, state, cshape)
            let body = new BulletSharp.RigidBody(info)
            sim.AddCollisionObject(body)
            let body = Body(shape, Mod.constant trafo, Double.PositiveInfinity, body)
            transact (fun () -> bodies.Add(body) |> ignore)
            body

        member x.Bodies = bodies :> aset<_>

        
module BulletSg =
    let private red = Mod.constant C4b.Red
    let private white = Mod.constant C4b.White

    let world (w : World) =
        let objects = 
            w.Bodies |> ASet.map (fun b ->
                let trafo = b.Trafo

                let color =
                    if b.IsStatic then white
                    else red

                let geometry = 
                    match b.Shape with
                        | Box b -> 
                            Sg.box color (Mod.constant b)

                        | Plane p -> 
                            Sg.fullScreenQuad
                                |> Sg.scale 100.0
                                |> Sg.vertexBufferValue DefaultSemantic.Colors (color |> Mod.map (fun c -> c.ToC4f() |> V4f))

                        | Cylinder c ->
                            Sg.cylinder 20 color (Mod.constant c.Radius) (Mod.constant c.Height)
                                    
                                |> Sg.transform (Trafo3d.Translation (V3d(0.0, 0.0, -c.Height / 2.0)) * Trafo3d.Translation c.Center)


                        | Sphere s ->
                            Sg.sphere 5 color (Mod.constant s.Radius)
                                |> Sg.transform (Trafo3d.Translation s.Center)

                        | Mesh m ->
                            Sg.ofIndexedGeometry m


                Sg.trafo trafo geometry
            )

        Sg.set objects
    
    let init(deltaTime : IMod<DateTime>) =

        let w = Bullet.World(deltaTime)
        let r = Random()
        let bounds = Box3d.FromCenterAndSize(V3d(0.0, 0.0, 5.0), V3d(2.0, 2.0, 2.0))
        let randomPosition() =
            bounds.Min + bounds.Size * V3d(r.NextDouble(), r.NextDouble(), r.NextDouble())

        let box = Box3d.FromCenterAndSize(V3d.Zero, V3d.III)
        for i in 1 .. 100 do   
            let initial = randomPosition() |> Trafo3d.Translation
            w.AddBody(initial, Box box, 0.5) |> ignore

        w.AddBody(Trafo3d.Translation(0.0, 0.0, 10.0), Sphere(Sphere3d(V3d.Zero, 1.0)), 0.5) |> ignore
        w.AddBody(Trafo3d.Translation(0.5, 0.0, 25.0), Sphere(Sphere3d(V3d.Zero, 1.0)), 0.5) |> ignore

        w.AddStatic(Trafo3d.Identity, Plane Plane3d.ZPlane) |> ignore

        w.AddStatic(Trafo3d.Translation(0.0, 0.0, 15.0), Box box) |> ignore

        ()