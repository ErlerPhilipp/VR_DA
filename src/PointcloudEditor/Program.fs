// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module VRGrabbingTest

open System
open System.IO

open Valve.VR

open OpenTK

open Aardvark.Application
open Aardvark.Application.WinForms
open Aardvark.Base
open Aardvark.Base.Ag
open Aardvark.Base.Native
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.Semantics
open Aardvark.VR
open Aardvark.Git
open Aardvark.Database
open Aardvark.Base.Native

open LogicalSceneTypes
open SelectionVolume
open Primitives
open Sphere
open SGHelper
open NormalMap
open TextureTiling
open Lighting
open Highlight
open LightRepresentation
open Audio
open PointCloudHelper
open InteractiveSegmentation

[<EntryPoint>]
let main argv =

    Ag.initialize()
    Aardvark.Init()
    
    let renderingQuality = 
        if argv.Length > 0 then
            let arg = argv.[0]
            let mutable asFloat = 0.0
            let success = System.Double.TryParse(arg, &asFloat)
            if success then asFloat else 1.0
        else 
            printfn @"No argument given for rendering quality. Using full quality '1.0'"
            1.0

    let pointSetFile = 
        if argv.Length > 1 then
            argv.[1]
        else 
            printfn @"No argument given for point set file. Using '..\..\resources\pointclouds\JBs_Haus.pts'"
            @"..\..\resources\pointclouds\JBs_Haus.pts"

    let referenceOperationsFile = 
        if argv.Length > 2 then
            argv.[2]
        else 
            printfn @"No argument given for reference operation. Using '..\..\resources\pointclouds\JBs_Haus_groundTruth.xml'"
            @"..\..\resources\pointclouds\JBs_Haus_groundTruth.xml"

    let loadGroundTruth =
        if argv.Length > 3 then
            let arg = argv.[3]
            let mutable asBool = false
            let success = System.Boolean.TryParse(arg, &asBool)
            if success then asBool else false
        else 
            printfn @"No argument given for loading ground truth. Using 'true'"
            true

    let storagePath = 
        if argv.Length > 4 then
            argv.[4]
        else 
            printfn @"No argument given for storage path. Using '..\..\vgmCache'"
            @"..\..\vgmCache"

    let autoCompareInSec = 
        if argv.Length > 5 then
            let arg = argv.[5]
            let mutable asFloat = 0.0
            let success = System.Double.TryParse(arg, &asFloat)
            if success then asFloat else 60.0
        else 
            printfn @"No argument given for auto compare time. Using never, '0.0' sec"
            0.0
    
    Logging.log("Starting Pointcloud Editor with loadGroundTruth = " + loadGroundTruth.ToString())

    let refOps = 
        if loadGroundTruth then
            OperationsComp.loadOperationsFromFile(referenceOperationsFile)
        else
            [||]

    Directory.CreateDirectory(storagePath) |> ignore
    let mem     = Memory.mapped (Path.combine [storagePath;"memory.mapped"])
    let get i   = NewImpl.Memory.mapped (Path.combine [storagePath;sprintf "memory-chunk-%d.mapped" i])
    let store   = new BlobStore(mem,get)
    use db      = new Database(store)
        
    let lastSixteenChars = pointSetFile.Substring(pointSetFile.Length-20, 16)
    let hash = System.Text.Encoding.ASCII.GetBytes(lastSixteenChars)
    let r = db.Get(System.Guid(hash))

    let pointSet =
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
            let file = pointSetFile

            let cnt = Pts.approximatePointCount file
            let chunkSize = 1 <<< 20//1 <<< 16
            let points = Pts.readChunked chunkSize file

            Log.start "complete build"

            let off = V3d.Zero
            let tree = Octree.build db 5000 off (1 + cnt / chunkSize) points
//            let tree = Octree.build db 10000 off (1 + cnt / chunkSize) points
            printfn "tree : %A" tree

            r := tree
            //db.Dispose()

            Log.stop()

            tree
    
    Aardvark.SceneGraph.IO.Loader.Assimp.initialize()
    Aardvark.Rendering.GL.RuntimeConfig.SupressSparseBuffers <- true

    use app = new OpenGlApplication()
    let vrWin = VrWindow.VrWindow(app.Runtime, true)

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
    
    let pedestalHeight = 0.8
    let pedestalRadius = 0.5
    let pedestalVerticalOffset = pedestalHeight / 2.0
    let pedestalPosition = V3d(0.0, pedestalVerticalOffset, 0.0)
        
    let cushionHeight = pedestalRadius * 0.75
    let cushionSize = pedestalRadius * 1.5
    let cushionPosition = V3d(pedestalPosition.X, pedestalPosition.Y + pedestalHeight / 2.0 + cushionHeight / 2.0, pedestalPosition.Z)

    let minNumPoints = 100000

    let rec getPoints (numPoints : int) (n : OctreeNode) =
        match n with
            | Empty -> [||]
            | Node (points, children)  -> 
                let points = points.GetValue() |> Array.map (fun p -> p.Position)
                let childPoints =
                    if numPoints < minNumPoints then
                        children 
                            |> Array.map (fun c -> c.GetValue()) 
                            |> Array.map (getPoints (numPoints + points.Length))
                            |> Array.concat
                    else
                        [||]
                Array.append points childPoints
            | Leaf points -> 
                points.GetValue() |> Array.map (fun p -> p.Position)

    let centroid (n : OctreeNode) = 
        let pos = getPoints 0 n
        let posSum = (pos |> Array.fold (+) V3d.OOO)
        let count = pos |> Array.length |> float
        let avgPos = posSum / count
        let variances = pos |> Array.map (fun p -> (avgPos - p).Length )
        let varSum = (variances |> Array.fold (+) V3d.OOO)
        let variance = varSum / count
        (avgPos, variance, count)
        
    let (pointCloudBBCenter, variance, count) = centroid(pointSet.root.GetValue())
    printfn "centroid = %A, variance = %A, count = %A" pointCloudBBCenter variance count
    let pointCloudResetPos = V3d(cushionPosition.X, cushionPosition.Y + 0.3, cushionPosition.Z)
    let pointCloudOffset = -pointCloudBBCenter
    let pointCloudCenterTrafo = Trafo3d.Translation(pointCloudOffset)
        
    let pointCloudBBSize = variance
    let pointCloudScaleCorrection = Trafo3d.Scale(1.0 / pointCloudBBSize.Length * 0.5)
    
    let controllerToTrackpadTrafo = Trafo3d.Scale(0.0200) * Trafo3d.RotationInDegrees(-83.0, 0.0, 0.0) * Trafo3d.Translation(0.0, 0.006, 0.050)
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
    let constTransparentColorEffect =    [
                                    defaultTrafoEffect
                                    DefaultSurfaces.constantColor (C4f(0.5f, 0.5f, 0.5f, 0.75f)) |> toEffect
                                ] 
    let constTransparentColorSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, constTransparentColorEffect) :> ISurface
    let constTextureEffect =    [
                                    defaultTrafoEffect
                                    defaultDiffuseTextureEffect
                                ] 
    let constTextureSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, constTextureEffect) :> ISurface
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
    let centroidEffect =            [
                                    SphereTexture.vertex |> toEffect
                                    defaultTrafoEffect
                                    SphereTexture.fragment |> toEffect
                                    defaultDiffuseTextureEffect
                                    OmnidirShadowShader.Effect false
                                ]
    let centroidSurface = vrWin.Runtime.PrepareEffect(vrWin.FramebufferSignature, centroidEffect) :> ISurface
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

    let controllerOverlayTexture = Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\overlay.png", textureParam) :> ITexture))
    //#endregion
    
    //#region SceneGraph
    let assimpFlagsSteamVR = 
        Assimp.PostProcessSteps.None
        
    let loadVR f = Loader.Assimp.Load(f,assimpFlagsSteamVR)
                    
    let thumbPosSg = Sg.sphere 4 (Mod.constant C4b.Green) (Mod.constant 1.0)
                                |> Sg.blendMode(Mod.constant (BlendMode(true))) 
                                |> Sg.writeBuffers (Some (Set.singleton DefaultSemantic.Colors))
                                |> Sg.pass (Renderpasses.ThumbPass)
   
    let controllerSg = 
        let controllerBody = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\bodytri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        let controllerButton =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\buttontri.obj" |> loadVR|> Sg.AdapterNode :> ISg
        let controllerLGrip =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\lgriptri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        let controllerRGrip = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\rgriptri.obj" |> loadVR|> Sg.AdapterNode :> ISg
        let controllerSysButton = @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\sysbuttontri.obj" |> loadVR |> Sg.AdapterNode :> ISg
        let controllerTrackpad =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\trackpadtri.obj" |> loadVR |> Sg.AdapterNode :> ISg
        let controllerTrigger =  @"..\..\resources\models\SteamVR\vr_controller_vive_1_5\triggertri.obj"|> loadVR |> Sg.AdapterNode :> ISg
        let controllerOverlay = Sg.fullScreenQuad 
                                    |> Sg.surface (Mod.constant constTextureSurface)
                                    |> controllerOverlayTexture 
                                    |> Sg.blendMode(Mod.constant (BlendMode(true)))
                                    |> Sg.writeBuffers (Some (Set.singleton DefaultSemantic.Colors))
                                    |> Sg.trafo (Mod.constant controllerToTrackpadTrafo)
                                    |> Sg.pass (Renderpasses.SelectionVolumePass)
        [ controllerBody; controllerButton; controllerLGrip; controllerRGrip; controllerSysButton; controllerTrackpad; controllerTrigger; controllerOverlay ]
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
          
    let pointCloudModelTrafo = pointCloudCenterTrafo * pointCloudScaleCorrection * flipYZ
            
    let centroidSg = Sg.sphere 6 (Mod.constant C4b.DarkYellow) (Mod.constant 0.05)
                    |> Sg.texture DefaultSemantic.DiffuseColorTexture (Mod.constant (FileTexture(@"..\..\resources\textures\balls\SoftballColor.jpg", textureParam) :> ITexture))
    //#endregion

    //#region Objects   
    let controller1Object = 
        { defaultObject with
            id = newId()
            model = Some controllerSg
            surface = Some diffuseSurface
        }
    let controller2Object = { controller1Object with id = newId() }
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
    let centroidObject = 
        { defaultObject with
            id = newId()
            model = Some centroidSg
            trafo = Trafo3d.Translation(pointCloudResetPos)
            surface = Some centroidSurface
        }
    let thumbPosObject1 =
        { defaultObject with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Identity
            model = Some thumbPosSg
            surface = Some constTransparentColorSurface
        }
    let thumbPosObject2 = { thumbPosObject1 with id = newId()}
    let selectionVolumeObject1 =
        { defaultObject with
            id = newId()
            castsShadow = false
            trafo = Trafo3d.Identity
            model = Some selectionVolumeSg
            surface = Some (makeSelectionVolumeSurface(vrWin))
        }
    let selectionVolumeObject2 = { selectionVolumeObject1 with id = newId()}

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
        [lightObject; centroidObject]
        @ [groundObject; ceilingObject]
        @ [wall1; wall2; wall3; wall4]
        //@ [pedestal; cushion]
        @ [controller1Object; controller2Object; camObject1; camObject2; thumbPosObject1; thumbPosObject2; selectionVolumeObject1; selectionVolumeObject2]
    //#endregion
    
    //#region Scene   
    let specialObjectIds =
        {
            cam1ObjectId        = camObject1.id
            cam2ObjectId        = camObject2.id
            controller1ObjectId = controller1Object.id
            controller2ObjectId = controller2Object.id
            lightId             = lightObject.id
            centroidId          = centroidObject.id
            thumbPos1Id         = thumbPosObject1.id
            thumbPos2Id         = thumbPosObject2.id
            selectionVolume1Id  = selectionVolumeObject1.id
            selectionVolume2Id  = selectionVolumeObject2.id
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
            pointCloudTrafo     = pointCloudModelTrafo
            initialOctree       = pointSet
            currentOctree       = pointSet
            contrToTrackpad     = controllerToTrackpadTrafo
            allOperations       = [||]
            refOperationsFile   = referenceOperationsFile
            referenceOperations = refOps
            timeSinceLastComp   = 0.0
            loadGroundTruth     = loadGroundTruth
            autoCompareInSec   = autoCompareInSec
            
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
