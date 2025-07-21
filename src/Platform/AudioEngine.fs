namespace HamicomEmu.Platform

module AudioEngine =

    open System.Collections.Generic
    open OpenTK.Audio.OpenAL

    type AudioEngine(sampleRate: int) =
        // OpenALデバイス/コンテキスト
        let device = ALC.OpenDevice(null)
        let context = ALC.CreateContext(device, (null: int[]))
        do ALC.MakeContextCurrent(context) |> ignore

        let source = AL.GenSource()
        let buffers = [| for _ in 1..6 -> AL.GenBuffer() |]
        // バッファごとに送信したサンプル数を記録するキュー
        let submittedLengths = Queue<int>()
        let availableBuffers = Queue<int>(buffers)

        member _.Submit(samples: float32[]) =
            // 再生済みバッファをすべてアンキューして available に戻す
            let processed = AL.GetSource(source, ALGetSourcei.BuffersProcessed)
            for _ in 1..processed do
                let id = AL.SourceUnqueueBuffer(source)
                availableBuffers.Enqueue(id)
                if submittedLengths.Count > 0 then submittedLengths.Dequeue() |> ignore

            // バッファが足りなければ skip（または一時的に GenBuffer する？）
            if availableBuffers.Count = 0 then
                () // バッファ不足（本来は警告を出す）

            else
                // 空いてるバッファにPCMを書き込み
                let pcm = samples |> Array.map (fun x -> int16 (x * 32767.0f)) // float32[] → int16[]（PCM16）へ変換
                let bufferId = availableBuffers.Dequeue()
                if pcm.Length > 0 then
                    AL.BufferData(bufferId, ALFormat.Mono16, &pcm[0], pcm.Length * 2, sampleRate)
                AL.SourceQueueBuffer(source, bufferId)
                submittedLengths.Enqueue(samples.Length)

                // ソースが止まっていたら再生
                let state = AL.GetSource(source, ALGetSourcei.SourceState) |> enum<ALSourceState>
                if state <> ALSourceState.Playing then
                    AL.SourcePlay(source)

        member _.PendingSamples =
            submittedLengths |> Seq.sum

        /// 明示的なリソース開放が必要らしい
        member _.Dispose() =
            AL.SourceStop(source)
            AL.DeleteSource(source)
            buffers |> Array.iter AL.DeleteBuffer
            ALC.DestroyContext(context)
            let closed = ALC.CloseDevice(device)
            if not closed then
                printfn "Warning: ALC.CloseDevice failed!"
