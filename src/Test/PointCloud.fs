namespace Aardvark.VR

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

[<AutoOpen>]
module PointCloud =

    type DummyDataProvider(root : Box3d) =
    
        interface ILodData with
            member x.BoundingBox = root

            member x.Traverse f =
                let rec traverse (level : int) (b : Box3d) =
                    let box = b
                    let n = 100.0
                    let node = { id = b; level = level; bounds = box; inner = true; granularity = Fun.Cbrt(box.Volume / n); render = true}

                    if f node then
                        let center = b.Center

                        let children =
                            let l = b.Min
                            let u = b.Max
                            let c = center
                            [
                                Box3d(V3d(l.X, l.Y, l.Z), V3d(c.X, c.Y, c.Z))
                                Box3d(V3d(c.X, l.Y, l.Z), V3d(u.X, c.Y, c.Z))
                                Box3d(V3d(l.X, c.Y, l.Z), V3d(c.X, u.Y, c.Z))
                                Box3d(V3d(c.X, c.Y, l.Z), V3d(u.X, u.Y, c.Z))
                                Box3d(V3d(l.X, l.Y, c.Z), V3d(c.X, c.Y, u.Z))
                                Box3d(V3d(c.X, l.Y, c.Z), V3d(u.X, c.Y, u.Z))
                                Box3d(V3d(l.X, c.Y, c.Z), V3d(c.X, u.Y, u.Z))
                                Box3d(V3d(c.X, c.Y, c.Z), V3d(u.X, u.Y, u.Z))
                            ]

                        children |> List.iter (traverse (level + 1))
                    else
                        ()
                traverse 0 root

            member x.Dependencies = []

            member x.GetData (cell : LodDataNode) =
                async {
                    //do! Async.SwitchToThreadPool()
                    let box = cell.bounds
                    let points = 
                        [| for x in 0 .. 9 do
                             for y in 0 .. 9 do
                                for z in 0 .. 9 do
                                    yield V3d(x,y,z)*0.1*box.Size + box.Min |> V3f.op_Explicit
                         |]
                    let colors = Array.create points.Length (Helpers.randomColor())
                    //let points = Helpers.randomPoints cell.bounds 1000
                    //let b = Helpers.box (Helpers.randomColor()) cell.bounds
//                  
                    //do! Async.Sleep(100)
                    let mutable a = 0

//                    for i in 0..(1 <<< 20) do a <- a + 1
//
//                    let a = 
//                        let mutable a = 0
//                        for i in 0..(1 <<< 20) do a <- a + 1
//                        a

                    //return Some <| IndexedGeometry(Mode = unbox a, IndexedAttributes = SymDict.ofList [ DefaultSemantic.Positions, points :> Array; DefaultSemantic.Colors, colors :> System.Array])
                
                    return Some <| Helpers.randomPoints box 1000
                }

    let data = DummyDataProvider(Box3d.FromMinAndSize(V3d.OOO, 200.0 * V3d.III)) :> ILodData

    
    let scene (lodView : IMod<Trafo3d>) (view : IMod<Trafo3d>) (win : VrWindow) (r : IRuntime) =

        let eff =
            let effects = [
                Instanced.trafo |> toEffect           
                DefaultSurfaces.vertexColor  |> toEffect         
            ]
            let e = FShade.SequentialComposition.compose effects
            FShadeSurface(e) :> ISurface 

        let surf = 
            win.Runtime.PrepareSurface(
                win.FramebufferSignature,
                eff
            ) :> ISurface |> Mod.constant

        let f = Frustum.perspective 100.0 0.2 40.0 (1.7) |> Frustum.projTrafo

        let cloud =
            Sg.pointCloud data {
                targetPointDistance     = Mod.constant 200.0
                maxReuseRatio           = 0.5
                minReuseCount           = 1L <<< 20
                pruneInterval           = 500
                customView              = Some lodView //win.Hmd.WorldToDevice 
                customProjection        = Some (Mod.constant f)
                attributeTypes =
                    Map.ofList [
                        DefaultSemantic.Positions, typeof<V3f>
                        DefaultSemantic.Colors, typeof<C4b>
                    ]
                boundingBoxSurface      = Some surf
            } 
                 
        let somePoints =
            let positions =
                [|
                    for z in 0 .. 50 do
                        yield V3f(0.0f, 0.0f, 0.2f * float32 z)
                |]
            
            IndexedGeometryMode.PointList
                |> Sg.draw
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant positions)
                |> Sg.vertexBufferValue DefaultSemantic.Colors (Mod.constant V4f.IOOI)  
        let sg = 
            Sg.group' [
                cloud
                    |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect                  
                        DefaultSurfaces.vertexColor  |> toEffect         
//                        Instanced.pointSprite  |> toEffect     
//                        Instanced.pointSpriteFragment  |> toEffect
//                        Instanced.diffuse |> toEffect 
                    ]
//                Helpers.frustum win.Hmd.WorldToDevice (Mod.constant f)
//                    |> Sg.effect [
//                        DefaultSurfaces.trafo |> toEffect                  
//                        DefaultSurfaces.constantColor C4f.Green  |> toEffect    
//                    ]

                data.BoundingBox.EnlargedByRelativeEps(0.005)
                    |> Helpers.wireBox C4b.VRVisGreen
                    |> Sg.ofIndexedGeometry
            ]

        let final =
            sg |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect                  
                    DefaultSurfaces.vertexColor  |> toEffect 
                    ]
                |> Sg.uniform "PointSize" (Mod.constant 0.05)
                |> Sg.uniform "ViewportSize" win.Sizes

        final

