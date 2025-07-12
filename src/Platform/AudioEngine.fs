namespace HamicomEmu.Platform.AudioEngine

open System
open Microsoft.Xna.Framework.Audio

type AudioEngine(sampleRate: int) =

    let instance = new DynamicSoundEffectInstance(sampleRate, AudioChannels.Mono)
    let mutable isPlaying = false
    let mutable timeOffset = 0.0

    member _.Submit(samples: float32 list) =
        let clipped = samples |> List.map (fun s -> s |> max -1.0f |> min 1.0f)

        let pcmArray = clipped |> List.map (fun x -> int16 (x * 32767.0f)) |> List.toArray

        let byteArray = Array.zeroCreate<byte> (pcmArray.Length * 2)
        Buffer.BlockCopy(pcmArray, 0, byteArray, 0, byteArray.Length)

        instance.SubmitBuffer(byteArray)

        if instance.State <> SoundState.Playing then
            instance.Play()
