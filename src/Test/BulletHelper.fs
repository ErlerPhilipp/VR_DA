namespace Aardvark.VR

open BulletSharp
open BulletSharp.Math

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph

open System

type Shape =
    | Box            of size :  V3d
    | Sphere         of radius : float
    | Cylinder       of radius : float * height : float
    | Plane          of Plane3d
    | Mesh           of IndexedGeometry
    | TriangleMesh   of Triangle3d[]
    | Compound       of list<Trafo3d * Shape>

type Mass = Infinite | Mass of float32
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Mass =
    let toFloat =
        function | Mass m   -> m
                 | Infinite -> 0.0f

module BulletHelper =

    let toVector3 (v : V3d) = 
        Vector3(float32 v.X, float32 v.Y, float32 v.Z)

    let toV3d (v : Vector3) = 
        V3d(float v.X, float v.Y, float v.Z)
        
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
        
    let rec toCollisionShape (s : Shape) : CollisionShape =
        match s with
            | Box size ->
                let shape = new BoxShape(size * 0.5 |> toVector3) :> CollisionShape
                shape

            | Sphere radius ->
                let shape = new SphereShape(float32 radius) :> CollisionShape
                shape

            | Cylinder(radius, height) ->
                let shape = new CylinderShapeZ(float32 radius, float32 radius, float32 (0.5 * height)) :> CollisionShape
                shape

            | Plane p ->
                let shape = new StaticPlaneShape(toVector3 p.Normal, float32 p.Distance) :> CollisionShape
                shape

            | Compound inner -> 
                let co = new BulletSharp.CompoundShape()
                for (trafo,shape) in inner do
                    let shape = toCollisionShape shape
                    co.AddChildShape(toMatrix trafo, shape)
                co :> _

            | TriangleMesh triangles ->
                let mesh = new TriangleMesh()
                for t in triangles do
                    mesh.AddTriangle(toVector3 t.P0, toVector3 t.P1, toVector3 t.P2)

                let shape = new BvhTriangleMeshShape(mesh, true, true)
                shape.BuildOptimizedBvh()
                shape :> CollisionShape


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
                    for i in 0 .. 3 .. positions.Length-3 do
                        mesh.AddTriangle(positions.[i + 0], positions.[i + 1], positions.[i + 2])

                else
                    let ids = positions |> Array.map (fun v -> mesh.FindOrAddVertex(v, false))
                    for i in 0 .. 3 .. triangles.Length-3 do
                        let i0 = ids.[triangles.[i + 0]]
                        let i1 = ids.[triangles.[i + 1]]
                        let i2 = ids.[triangles.[i + 2]]
                        mesh.AddTriangleIndices(i0, i1, i2)

                let shape = new BvhTriangleMeshShape(mesh, true, true)


                shape :> CollisionShape
                
    type DebugDrawer() = 
        inherit BulletSharp.DebugDraw()
    
        let mutable debugModeBinding = BulletSharp.DebugDrawModes.DrawWireframe // BulletSharp.DebugDrawModes.All
        let mutable lines  = [||]
        let mutable colors = [||]

        let linesMod  = Mod.init [||]
        let colorsMod = Mod.init [||]
        //let linesSg = Sg.lines (Mod.init C4b.White) (linesMod) 

        let pos = (linesMod  |> Mod.map ( fun (a : Line3d[]) -> [| for l in a do yield l.P0.ToV3f(); yield l.P1.ToV3f()|]  ))
        let col = (colorsMod |> Mod.map ( fun (a : V3d[]) -> [| for c in a do yield c.ToC4f().ToC4b(); yield c.ToC4f().ToC4b()|]  ))
        let idx = (pos       |> Mod.map ( fun a -> Array.init (a |> Array.length) id))
        let linesSg =
            Sg.draw IndexedGeometryMode.LineList
                |> Sg.vertexAttribute (DefaultSemantic.Positions)   pos 
                |> Sg.vertexAttribute (DefaultSemantic.Colors)      col
                |> Sg.index                                         idx

        let beamEffect = Sg.effect [
                            DefaultSurfaces.trafo |> toEffect
                            DefaultSurfaces.vertexColor |> toEffect
                            DefaultSurfaces.thickLine |> toEffect
                        ]

        member this.debugDrawerSg = linesSg |> beamEffect
    
        member this.addLineToStack(fromPos : V3d, toPos : V3d, color : V3d) =
            lines <- Array.append lines [| Line3d(fromPos, toPos) |]
            colors <- Array.append colors [| color |]
            ()
    
        member this.flush() =
            transact ( fun _ -> Mod.change linesMod (lines) )
            lines <- Array.empty
            transact ( fun _ -> Mod.change colorsMod (colors) )
            colors <- Array.empty
            ()

        override x.DrawLine(fromPos, toPos, color) = 
            x.addLineToStack(toV3d fromPos, toV3d toPos, toV3d color)

        override x.DrawLine(fromPos, toPos, fromColor, toColor) = 
            x.addLineToStack(toV3d fromPos, toV3d toPos, toV3d fromColor)

        override x.Draw3dText(pos, text) = 
            ()

        override x.ReportErrorWarning(text) = 
            printfn "BulletDebugDrawer Error: %A" text

        override x.get_DebugMode() = 
            debugModeBinding

        override x.set_DebugMode(mode) = 
            debugModeBinding <- mode
        
        // optional
        override x.DrawBox(  bbMin,   bbMax,   trans,   color) = 
            let trafo = toTrafo trans
            let min = toV3d bbMin
            let max = toV3d bbMax
            let positions = [| min; V3d(min.X, min.Y, max.Z); V3d(min.X, max.Y, min.Z); V3d(max.X, min.Y, min.Z); 
                                    V3d(max.X, max.Y, min.Z); V3d(max.X, min.Y, max.Z); V3d(min.X, max.Y, max.Z); max |]

            trafo.Forward.TransformPosArray positions
            x.addLineToStack(positions.[0], positions.[2], toV3d color)
            x.addLineToStack(positions.[3], positions.[4], toV3d color)
            x.addLineToStack(positions.[5], positions.[7], toV3d color)
            x.addLineToStack(positions.[1], positions.[6], toV3d color)
            x.addLineToStack(positions.[0], positions.[3], toV3d color)
            x.addLineToStack(positions.[2], positions.[4], toV3d color)
            x.addLineToStack(positions.[6], positions.[7], toV3d color)
            x.addLineToStack(positions.[1], positions.[5], toV3d color)
            x.addLineToStack(positions.[2], positions.[6], toV3d color)
            x.addLineToStack(positions.[4], positions.[7], toV3d color)
            x.addLineToStack(positions.[3], positions.[5], toV3d color)
            x.addLineToStack(positions.[0], positions.[1], toV3d color)
            
        override x.DrawPlane(  planeNormal,  plane,   transform,   color) = ()

        override x.DrawAabb(  from,   toPos,   color) = ()
        override x.DrawArc(  center,   normal,   axis,   radiusA,  radiusB,  minAngle,  maxAngle,   color,  drawSect,  stepDegrees) = ()
        override x.DrawArc(  center,   normal,   axis,  radiusA,  radiusB,  minAngle,  maxAngle,   color,  drawSect) = ()
        override x.DrawBox(  bbMin,   bbMax,   color) = ()
        override x.DrawCapsule( radius,  halfHeight,  upAxis,   transform,   color) = ()
        override x.DrawCone( radius,  height,  upAxis,   transform,   color) = ()
        override x.DrawContactPoint(  pointOnB,   normalOnB,  distance,  lifeTime,   color) = ()
        override x.DrawCylinder( radius,  halfHeight,  upAxis,   transform,   color) = ()
        //override x.DrawSphere(  p,  radius,   color) = ()
        //override x.DrawSphere( radius,   transform,   color) = ()
        //override x.DrawSpherePatch(  center,   up,   axis,  radius, minTh,  maxTh,  minPs,  maxPs,   color,  stepDegrees,  drawCenter) = ()
        override x.DrawSpherePatch(  center,   up,   axis,  radius, minTh,  maxTh,  minPs,  maxPs,   color,  stepDegrees) = ()
        override x.DrawSpherePatch(  center,   up,   axis,  radius, minTh,  maxTh,  minPs,  maxPs,   color) = ()
        override x.DrawTransform(  transform,  orthoLen) = ()
        override x.DrawTriangle(  v0,   v1,   v2,   color, a) = ()
        override x.DrawTriangle(  v0,   v1,   v2, a,  b,  c,   color,  alpha) = ()

