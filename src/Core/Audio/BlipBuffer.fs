namespace HamicomEmu.Audio

module BlipBuffer =

    open System

    type BlipBuffer(sampleRate: int, bufferSize: int, numPhases: int, kernelLength: int) =
        let kernelCenter = kernelLength / 2

        // sincカーネル生成用
        let sinc x = if x = 0.0 then 1.0 else sin(Math.PI * x) / (Math.PI * x)
        let hann n = 0.5 - 0.5 * cos(2.0 * Math.PI * float n / float (kernelLength - 1))

        // カーネル1本を正規化
        let normalizeKernel (kernel: float[]) =
            let sum = Array.sum kernel
            if sum = 0.0 then kernel else kernel |> Array.map (fun v -> v / sum)

        // 多次元Impulseカーネルをコンストラクタで生成
        let impulseTable : float[][] =
            Array.init numPhases (fun phase ->
                let frac = float phase / float numPhases
                let kernel =
                    Array.init kernelLength (fun i ->
                        let pos = float i - float kernelCenter - frac
                        sinc pos * hann i
                    )
                normalizeKernel kernel  // 正規化
            )
        // 内部バッファ。リングバッファ方式で管理
        let buf = Array.zeroCreate<float> bufferSize
        let mutable bufOffset = 0         // 読み出し位置
        let mutable bufAvailable = 0      // バッファ内にあるサンプル数
        let mutable timeAcc = 0           // 現在のタイムスタンプ（APU/CPUクロック単位）

        /// サンプルレートなど再設定したいとき用
        member _.SetSampleRate(_sampleRate: int) =
            // 必要ならカーネル再生成（今回は省略）

            ()

        /// バッファを全クリア
        member _.Clear() =
            Array.Clear(buf, 0, buf.Length)
            bufOffset <- 0
            bufAvailable <- 0
            timeAcc <- 0

        /// エッジイベントを追加（time: サンプル時刻、delta: 振幅差分）
        member _.AddDelta (time: float, delta: float) =
            let intTime = int time
            let frac = time - float intTime // 小数点以下
            let phase = min (numPhases - 1) (max 0 (int (frac * float numPhases)))
            let kernel = impulseTable[phase]
            for i = 0 to kernelLength - 1 do
                let idx = intTime + i - kernelCenter
                if idx >= 0 && idx < bufferSize then
                    buf[idx] <- buf[idx] + kernel[i] * delta

            // 本家はここで「time」も進めておく（必要なら外部で管理）

        /// サンプルをまとめて読み出し（読み出した分だけ消費）
        member _.ReadSamples(dst: float[], count: int) =
            let n = min count bufAvailable
            for i = 0 to n - 1 do
                dst[i] <- buf[(bufOffset + i) % bufferSize]
                buf[(bufOffset + i) % bufferSize] <- 0.0
            bufOffset <- (bufOffset + n) % bufferSize
            bufAvailable <- bufAvailable - n
            n

        /// 新しいサンプルを確保（波形が増えたぶんだけAvailableを進める）
        member _.EndFrame(samplesGenerated: int) =
            bufAvailable <- bufAvailable + samplesGenerated

        /// 現在のバッファ残量
        member _.Available = bufAvailable

        /// バッファサイズ
        member _.Capacity = bufferSize
