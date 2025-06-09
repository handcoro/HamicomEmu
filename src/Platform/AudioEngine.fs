namespace HamicomEmu.Platform.AudioEngine

open System
open Microsoft.Xna.Framework.Audio

type AudioEngine(sampleRate: int) =

  let instance = new DynamicSoundEffectInstance(sampleRate, AudioChannels.Mono)
  let bufferSize = sampleRate / 60 // 1フレームあたりのサンプル数（約735）
  let buffer = Array.zeroCreate<int16>(bufferSize)

  let mutable isPlaying = false
  let mutable timeOffset = 0.0

  member _.PushSample(generator: float -> float32) =
    let dt = 1.0 / float sampleRate
    let mutable t = timeOffset

    for i in 0 .. bufferSize - 1 do
      let sample =
        generator t
        |> (*) 0.3f
        // クリップ
        |> max -1.0f
        |> min 1.0f
      buffer[i] <- int16 (sample * 32767.0f)
      t <- t + dt

    timeOffset <- t

    let byteBuffer = Array.zeroCreate<byte>(bufferSize * 2)
    Buffer.BlockCopy(buffer, 0, byteBuffer, 0, byteBuffer.Length)

    instance.SubmitBuffer(byteBuffer)

    if not isPlaying && instance.PendingBufferCount >= 2 then
      instance.Play()
      isPlaying <- true