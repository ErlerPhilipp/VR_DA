namespace Aardvark.VR

open Aardvark.Base

open OpenTK
open OpenTK.Audio

module Audio =
    open System.IO

    let ctx = new AudioContext()
    
    let toALVector3 (v : V3d) =
        Vector3(float32 v.X, float32 v.Y, float32 v.Z)

    let setListenerPosition(v : V3d) =
        let mutable pos = toALVector3 v
        AL.Listener(ALListener3f.Position, &pos)

    let setListenerVelocity(v : V3d) =
        let mutable vel = toALVector3 v
        AL.Listener(ALListener3f.Velocity, &vel)

    let setListenerOrientation(lookAt : V3d, up : V3d) =
        let mutable vectors = [|float32 lookAt.X; float32 lookAt.Y; float32 lookAt.Z; float32 up.X; float32 up.Y; float32 up.Z;|]
        AL.Listener(ALListenerfv.Orientation, &vectors)

    type Sound(ctx : AudioContext, source : int) =
        
        let mutable location = V3d.Zero
        let mutable velocity = V3d.Zero
        let mutable loop = false
        let mutable volume = 1.0
        let mutable pitch = 1.0

        member x.Loop
            with get() = loop
            and set l =
                loop <- l
                ctx.MakeCurrent()
                AL.Source(source, ALSourceb.Looping, l)

        member x.Location
            with get() = location
            and set l =
                ctx.MakeCurrent()
                location <- l
                let mutable pos = toALVector3 l
                AL.Source(source, ALSource3f.Position, &pos)

        member x.Velocity
            with get() = velocity
            and set v =
                ctx.MakeCurrent()
                velocity <- v
                let mutable pos = toALVector3 v
                AL.Source(source, ALSource3f.Velocity, &pos)

        member x.Volume
            with get() = volume
            and set v =
                ctx.MakeCurrent()
                volume <- v
                AL.Source(source, ALSourcef.Gain, float32 volume)

        member x.Pitch
            with get() = pitch
            and set p =
                ctx.MakeCurrent()
                pitch <- p
                AL.Source(source, ALSourcef.Pitch, float32 pitch)

        member x.Play() =
            ctx.MakeCurrent()
            AL.SourcePlay(source)

        member x.Pause() =
            ctx.MakeCurrent()
            AL.SourcePause(source)

        member x.Stop() =
            ctx.MakeCurrent()
            AL.SourceStop(source)

        member x.IsPlaying() =
            ctx.MakeCurrent()
            let mutable state = 0
            AL.GetSource(source, ALGetSourcei.SourceState, &state)
            (state = int(ALSourceState.Playing))
            
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Sound = 

        // Loads a wave/riff audio file.
        let LoadWave(stream : Stream) =

            if stream == null then
                failwith "stream"

            let reader = new BinaryReader(stream)
//            printfn "4: %A" (reader.ReadChars(4))
            // RIFF header
            let signature = System.String(reader.ReadChars(4))
            if signature <> "RIFF" then
                failwith("Specified stream is not a wave file.")

            let riff_chunck_size = reader.ReadInt32()

            let format = System.String(reader.ReadChars(4))
            // If format is "FLLR", you've found a shitty Apple WAV. 
            // Ignore the section or convert the file with e.g. Audacity (WAV Microsoft Signed 16-bit PCM)
            if (format <> "WAVE") then
                failwith("Specified stream is not a wave file.")

            // WAVE header
            let format_signature = System.String(reader.ReadChars(4))
            if (format_signature <> "fmt ") then
                failwith("Specified wave file is not supported.")

            let format_chunk_size = reader.ReadInt32()
            let audio_format = reader.ReadInt16()
            let num_channels = reader.ReadInt16()
            let sample_rate = reader.ReadInt32()
            let byte_rate = reader.ReadInt32()
            let block_align = reader.ReadInt16()
            let bits_per_sample = reader.ReadInt16()

            let data_signature = new System.String(reader.ReadChars(4))
            if (data_signature <> "data") then
                failwith("Specified wave file is not supported.")

            let data_chunk_size = reader.ReadInt32()

            (reader.ReadBytes((int)reader.BaseStream.Length), num_channels, bits_per_sample, sample_rate)

        let GetSoundFormat(channels : int16, bits : int16) =
            match channels with
                | 1s -> if bits = 8s then ALFormat.Mono8 else ALFormat.Mono16
                | 2s -> if bits = 8s then ALFormat.Stereo8 else ALFormat.Stereo16
                | _ -> failwith("The specified sound format is not supported.")

        let bufferFromFile (file : string) =

            ctx.MakeCurrent()

            let buffer = AL.GenBuffer()
            let (sound_data, channels, bits_per_sample, sample_rate) = LoadWave(File.Open(file, FileMode.Open))
            AL.BufferData(buffer, GetSoundFormat(channels, bits_per_sample), sound_data, sound_data.Length, sample_rate)
            buffer

        let sourceFromBuffer(bufferIndex : int) =
            let source = AL.GenSource()
            AL.Source(source, ALSourcei.Buffer, bufferIndex)
            Sound(ctx, source)
            