// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module VRGrabbingTest

open Valve.VR

open OpenTK

open Aardvark.Application.WinForms
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.VR
open Aardvark.Git
open Aardvark.Database
open Aardvark.Base.Native


open LogicalSceneTypes

open Shapes
open Primitives
open Sphere
open SGHelper
open NormalMap
open TextureTiling
open Lighting
open Highlight
open LightRepresentation
open Audio





open System
open Aardvark.Base
open Aardvark.Base.Native
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.Base.Ag
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.SceneGraph.Semantics
open System.IO
open System.Threading
open Aardvark.Git
open Aardvark.Database




open InteractiveSegmentation


module PointSetStorage =
    
    module Lazy =
        let force ( x : Lazy<'a> ) : 'a =
            x.Value


module Rendering =

    open PointSetStorage

     module LodData =
        open System.Collections.Concurrent

        type PointSetLodData(tree : IMod<Octree>, nodeCount : IMod<float>) =
            //let bounds = (root |> Mod.force).cell.BoundingBox
            let bounds = tree.GetValue().bounds

            let mutable cells = []

            member x.Traverse (f : LodDataNode -> bool) = 
             
                let rec traverse (level : int) (cell : GridCell) (n : OctreeNode)  =
                    let bb = cell.BoundingBox
                    match n with
                        | Empty -> ()
                        | Node (points,children)  ->
                            let nn = { id = (n :> obj); level = level; bounds = bb; 
                                       inner = true; granularity = Fun.Cbrt(bb.Volume / 5000.0); 
                                       render = true}

                            if f nn then
                                children |> Array.iteri (fun i child -> traverse (level + 1) (cell.GetChild i) child.Value) 
                        | Leaf points ->                             
                            let nn = { id = (n :> obj); level = level; bounds = bb; 
                                       inner = true; granularity = 1.0; render = true }
                            f nn |> ignore
                            
               
                let tree = Mod.force tree
                traverse 0 tree.cell tree.root.Value


            member x.BoundingBox = bounds

            member x.GetData (n : LodDataNode) : Async<Option<IndexedGeometry>> =
                async {
                    let node = n.id |> unbox<OctreeNode>

                    let points =
                        match node with
                            | Node (points,_) | Leaf points  -> points.Value
                            | Empty -> [||]
                    

                    let (p,n,c) = points    |> Array.map ( fun p -> V3f p.Position, V3f p.Normal, p.Color)
                                            |> Array.unzip3


                    //let real = Box3d(pos |> Seq.map V3d)
                    //Log.warn "bounds: %A" real

                    let r = 
                        IndexedGeometry(
                            IndexedAttributes =
                                SymDict.ofList [
                                    DefaultSemantic.Positions,  p :> Array
                                    DefaultSemantic.Normals,    n :> Array
                                    DefaultSemantic.Colors,     c :> Array
                                ]
                        )

                    return Some r
                }
            


            interface ILodData with
            
                member x.BoundingBox = bounds
                member x.Traverse f = x.Traverse f
                member x.GetData n = x.GetData n
                member x.Dependencies = [ tree :> IMod ]

            
                
    module Logic =
        type LodUserSettings =
            {
                NodeCount           : IModRef<float>
                PointSize           : IModRef<float>
            }


    open Logic


    let lodSettings =
        {   
            NodeCount = Mod.init 150.0
            PointSize = Mod.init 10.0
        }


    let pointCloudInfoSettings =
        {
            targetPointDistance     = Mod.init 50.0
            maxReuseRatio           = 0.5
            minReuseCount           = 1L <<< 20
            pruneInterval           = 500
            customView              = None
            customProjection        = None
            attributeTypes =
                Map.ofList [
                    DefaultSemantic.Positions, typeof<V3f>
                    DefaultSemantic.Colors, typeof<C4b>
                    DefaultSemantic.Normals, typeof<V3f>
                ]
            boundingBoxSurface      = Some BoundingBox.effectRed
        }

    open LodData
    open FShade
    open Aardvark.Base.Rendering.Effects

    let internal debugNormal (v : Vertex) =
        fragment {
            return V3d(abs v.n.X, abs v.n.Y, abs v.n.Z)
        }

    let mkSg (pointSet : IMod<Octree>) = 
        
        Sg.pointCloud' ( PointSetLodData(pointSet, lodSettings.NodeCount) ) pointCloudInfoSettings (LodProgress.Progress.empty)
        |> Sg.effect 
            [
                DefaultSurfaces.trafo       |> toEffect
                DefaultSurfaces.pointSprite |> toEffect
                DefaultSurfaces.vertexColor                 |> toEffect
            ]
        |> Sg.uniform "PointSize" lodSettings.PointSize 

open Rendering






























type CollisionGroups =
    | Static =          0b0000000000000001s
    | Ball =            0b0000000000000010s
    | HandTrigger =     0b0000000000000100s
    | HoopTrigger =     0b0000000000001000s
    | Avatar =          0b0000000000010000s
    | TeleportRaycast = 0b0000000000100000s

[<EntryPoint>]
let main _ =
    Ag.initialize()
    Aardvark.Init()
    
    
    let psp = @"..\..\resources\pointclouds\JBs_Haus.pts"
    let storagePath = @"C:\bla\vgmCache"

//        let psp = @"C:\bla\JBs_Haus.pts"
//        let storagePath = @"C:\bla\vgmCache2"

    //let psp         = @"Technologiezentrum_Teil1.pts"
    //let storagePath = @"cache"
    //use db = Database.NpgSql.remote "localhost" 5432 
    //use db = Database.ofMemory (Memory.hglobal ) //(Memory.newfile @"C:\Aardwork\test.bin")
    //use db = Database.Redis.local @"C:\octree.rdb"
    //use db = Database.Vhd.local  @"C:\Aardwork\octree.vhd"

    let s       = Directory.CreateDirectory(storagePath)
    let mem     = Memory.mapped (Path.combine [storagePath;"memory.mapped"]) //Memory.hglobal 0L //Memory.mapped (Path.combine [storagePath;"memory.mapped"])
    let get i   = NewImpl.Memory.mapped (Path.combine [storagePath;sprintf "memory-chunk-%d.mapped" i]) //Memory.hglobal 0L //NewImpl.Memory.mapped (Path.combine [storagePath;sprintf "memory-chunk-%d.mapped" i])
    let store   = new BlobStore(mem,get)
    use db      = new Database(store)
        
    let r = db.Get(Guid("6f3fd114-f345-4e2d-b82c-2e7172ea6086"))

    let pointset =

        if r.HasValue then
            Log.startTimed "read"
            let t : Octree = !r
            Log.stop()

            printfn "tree : %A" t

            match t.root.Value with
                | null -> ()
                | node -> printfn "node: %A" node.Count

            t

        else
            let file = psp

            let cnt = Pts.approximatePointCount file
            let chunkSize = 1 <<< 20//1 <<< 16
            let points = Pts.readChunked chunkSize file

            Log.start "complete build"

            let off = V3d.Zero
            let tree = Octree.build db 5000 off (1 + cnt / chunkSize) points
            printfn "tree : %A" tree

            r := tree
            //db.Dispose()

            Log.stop()

            tree
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    Aardvark.SceneGraph.IO.Loader.Assimp.initialize()
    Aardvark.Rendering.GL.RuntimeConfig.SupressSparseBuffers <- true

    use app = new OpenGlApplication()
    let vrWin = VrWindow.VrWindow(app.Runtime, true)
    

    let flipYZ = Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, V3d.OIO)

    let pointcloudSg = 
        Rendering.mkSg (pointset |> Mod.constant)
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.vertexColor |> toEffect
                ]
            |> Sg.uniform "ViewportSize" vrWin.Sizes
            |> Sg.trafo (Mod.constant(flipYZ * Trafo3d.Scale(0.01) * Trafo3d.Translation(V3d(0.0, 0.0, 2.0))))





    //#region Trafos / Architecture   
    let trackingAreaSize = 2.9
    let trackingAreaHeight = 5.2
    let wallThickness = 1.0

    let hoopScale = 2.0
    let hoopTrafoWithoutScale = Trafo3d.RotationYInDegrees(90.0) * Trafo3d.Translation(trackingAreaSize * 0.48, -1.8, 0.0)
    let scoreScale = 0.1
    let scoreTrafo = Trafo3d.Scale(scoreScale * hoopScale) * Trafo3d.RotationYInDegrees(180.0) * hoopTrafoWithoutScale * 
                        Trafo3d.Translation(V3d(-0.03, 1.71, -3.3 * scoreScale) * hoopScale)
    
    let wallLateralOffset = trackingAreaSize * 0.5 + wallThickness * 0.5
    let wallHorizontalOffset = trackingAreaHeight * 0.5 - wallThickness
    
    let pedestalHeight = 1.0
    let pedestalRadius = 0.005
    let pedestalHorizontalOffset = trackingAreaSize / 2.0 - 0.3
    let pedestalVerticalOffset = pedestalHeight / 2.0
    let pedestalPosition = V3d(+pedestalHorizontalOffset, pedestalVerticalOffset, +pedestalHorizontalOffset)
        
    let cushionHeight = pedestalRadius * 0.75
    let cushionSize = pedestalRadius * 1.5
    let cushionPosition = V3d(pedestalPosition.X, pedestalPosition.Y + pedestalHeight / 2.0 + cushionHeight / 2.0, pedestalPosition.Z)
        
//    let pointCloudRadius = 0.1213
//    let pointCloudResetPos = V3d(cushionPosition.X, cushionPosition.Y + pointCloudRadius, cushionPosition.Z)
    //#endregion
       
    //#region Effects and Surfaces   
    let defaultTrafoEffect = DefaultSurfaces.trafo |> toEffect
    let defaultSimpleLightingEffect = DefaultSurfaces.simpleLighting |> toEffect
    let defaultDiffuseTextureEffect = DefaultSurfaces.diffuseTexture |> toEffect

    let constColorEffect =      [
                                    defaultTrafoEffect
                                    LightRepresentation.Effect
                                    defaultSimpleLightingEffect
                                ]
    let constColorSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, constColorEffect) :> ISurface
    let diffuseEffect =         [
                                    defaultTrafoEffect
                                    defaultDiffuseTextureEffect
                                    OmnidirShadowShader.Effect false
                                ] 
    let diffuseSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, diffuseEffect) :> ISurface
    let normalDiffuseEffect =   [
                                    defaultTrafoEffect
                                    TextureTiling.Effect
                                    NormalMap.Effect
                                    defaultDiffuseTextureEffect
                                    OmnidirShadowShader.Effect false
                                ]
    let normalDiffuseSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, normalDiffuseEffect) :> ISurface
    //#endregion
    
    //#region Textures   
    let textureParam = { TextureParams.empty with wantMipMaps = true }
    let groundNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_normalmap_S.jpg", textureParam) :> ITexture))
    let ceilingNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_normalmap_S.jpg", textureParam) :> ITexture))
    let wallNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_normalmap_S.jpg", textureParam) :> ITexture))
    let cushionNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Tufted Leather\TexturesCom_TuftedLeather_normalmap_S.png", textureParam) :> ITexture))
    let pedestalNormalMap = Sg.texture DefaultSemantic.NormalMapTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Marble Polished\TexturesCom_MarblePolishedWhite1_normalmap_S.png", textureParam) :> ITexture))

    let groundDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wood Floor\TexturesCom_Wood Floor A_albedo_S.jpg", textureParam) :> ITexture))
    let ceilingDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Wooden Planks\TexturesCom_Wood Planks_albedo_S.jpg", textureParam) :> ITexture))
    let wallDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Painted Bricks\TexturesCom_Painted Bricks_albedo_S.jpg", textureParam) :> ITexture))
    let cushionDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Tufted Leather\TexturesCom_TuftedLeather_albedo_S.png", textureParam) :> ITexture))
    let pedestalDiffuseTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\Marble Polished\TexturesCom_MarblePolishedWhite1_diffuse_S.png", textureParam) :> ITexture))
    //#endregion
    
    //#region SceneGraph
    let assimpFlagsSteamVR = 
        Assimp.PostProcessSteps.None
        
    let loadVR f = Loader.Assimp.Load(f,assimpFlagsSteamVR)
   
    let controllerSg = 
        let controllerBody = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\bodytri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        let controllerButton =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\buttontri.obj" |> loadVR|> Sg.AdapterNode :> ISg
        let controllerLGrip =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\lgriptri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        let controllerRGrip = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\rgriptri.obj" |> loadVR|> Sg.AdapterNode :> ISg
        let controllerSysButton = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\sysbuttontri.obj" |> loadVR |> Sg.AdapterNode :> ISg
        let controllerTrackpad =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\trackpadtri.obj" |> loadVR |> Sg.AdapterNode :> ISg
        let controllerTrigger =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\triggertri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        [ controllerBody; controllerButton; controllerLGrip; controllerRGrip; controllerSysButton; controllerTrackpad; controllerTrigger ]
            |> Sg.group :> ISg
            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\models\SteamVR\vr_controller_vive_1_5\onepointfive_texture.png", textureParam) :> ITexture))

//    let basestationSg = 
//            Loader.Assimp.Load(@"..\..\resources\models\SteamVR\lh_basestation_vive\basestationtri.obj", assimpFlagsSteamVR)
//            |> Sg.AdapterNode :> ISg
//            |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\models\SteamVR\lh_basestation_vive\lh_basestation_vive.png", textureParam) :> ITexture))
            
    let lightSg = Sg.sphere 3 (Mod.constant C4b.White) (Mod.constant 0.1)

    let groundSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
                            |> groundDiffuseTexture |> groundNormalMap
    let ceilingSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, wallThickness, trackingAreaSize))))
                            |> ceilingDiffuseTexture |> ceilingNormalMap
    let wallSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(trackingAreaSize, trackingAreaHeight, wallThickness))))
                            |> wallDiffuseTexture |> wallNormalMap

    let pedestalSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(pedestalRadius, pedestalHeight, pedestalRadius))))
                            |> pedestalDiffuseTexture |> pedestalNormalMap
    let cushionSg = BoxSg.box (Mod.constant C4b.Gray) (Mod.constant (Box3d.FromCenterAndSize(V3d.OOO, V3d(cushionSize, cushionHeight, cushionSize))))
                            |> cushionDiffuseTexture |> cushionNormalMap
    //#endregion

    //#region Objects   
    let controller1Object = 
        { defaultObject with
            id = newId()
            model = Some controllerSg
            surface = Some diffuseSurface
        }
    let controller2Object = { controller1Object with id = newId() }
//    let grabTrigger1 = 
//        { defaultObject with
//            id = newId()
//            castsShadow = false
//        }
//    let grabTrigger2 = { grabTrigger1 with id = newId() }
    let camObject1 = 
        { defaultObject with
            id = newId()
//            model = Some basestationSg
//            surface = Some diffuseSurface
        }
    let camObject2 = { camObject1 with id = newId() }
    let lightObject =
        { defaultObject with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Translation(0.0, trackingAreaHeight - wallThickness * 2.5, 0.0)
            model = Some lightSg
            surface = Some constColorSurface
        }

    let groundTilingFactor = 0.3
    let ceilingTilingFactor = 0.4
    let groundObject = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Translation(0.0, -0.5 * wallThickness, 0.0)
            model = Some groundSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(groundTilingFactor * trackingAreaSize)
        }

    let ceilingObject = 
        { defaultObject with
            id = newId()
            trafo = Trafo3d.Translation(0.0, trackingAreaHeight - 1.5 * wallThickness, 0.0)
            model = Some ceilingSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(ceilingTilingFactor * trackingAreaSize)
        }

    let wallBase = 
        { defaultObject with
            model = Some wallSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(0.25 * trackingAreaSize)
        }

    let wall1 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.Translation(0.0, wallHorizontalOffset, -wallLateralOffset)
        }
    let wall2 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (-90.0) * Trafo3d.Translation(wallLateralOffset, wallHorizontalOffset, 0.0)
        }
    let wall3 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (180.0) * Trafo3d.Translation(0.0, wallHorizontalOffset, wallLateralOffset)
        }
    let wall4 = 
        { wallBase with 
            id = newId()
            trafo = Trafo3d.RotationYInDegrees (90.0) * Trafo3d.Translation(-wallLateralOffset, wallHorizontalOffset, 0.0)
        }
    
    let pedestalBase = 
        { defaultObject with
            model = Some pedestalSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(1.0, pedestalRadius / pedestalHeight)
        }
    let pedestal = { pedestalBase with id = newId(); trafo = Trafo3d.Translation(pedestalPosition)}

    let cushionBase =
        { defaultObject with
            model = Some cushionSg
            surface = Some normalDiffuseSurface
            tilingFactor = V2d(0.25)
        }
    let cushion = { cushionBase with id = newId(); trafo = Trafo3d.Translation(cushionPosition)}

    let objects =
        [lightObject]
        @ [groundObject; ceilingObject; wall1; wall2; wall3; wall4]
        @ [pedestal; cushion]
        @ [controller1Object; controller2Object; camObject1; camObject2]
    //#endregion
        
    //#region Scene   
    let specialObjectIds =
        {
            cam1ObjectId        = camObject1.id
            cam2ObjectId        = camObject2.id
            controller1ObjectId = controller1Object.id
            controller2ObjectId = controller2Object.id
            lightId             = lightObject.id
        }

    let sceneObj =
        {
            objects             = PersistentHashSet.ofList objects
            viewTrafo           = Trafo3d.Identity
            trackingToWorld     = Trafo3d.Identity

            lightColor          = V3d(1.0, 1.0, 1.0)
            deltaTime           = 0.0
            scoreTrafo          = scoreTrafo
            scoreText           = "test"
            pointCloudSg        = pointcloudSg

            selectionVolume     = Shape.Sphere(0.25)
            
            specialObjectIds    = specialObjectIds
            interactionInfo1    = DefaultInteractionInfo
            interactionInfo2    = DefaultInteractionInfo
        }
    //#endregion

    let scene = GraphicsScene.createScene sceneObj vrWin

    let task = app.Runtime.CompileRender(vrWin.FramebufferSignature, scene)
    vrWin.RenderTask <- task
    
    vrWin.Run()
    
    Logging.endSession()

    OpenVR.Shutdown()

    0
