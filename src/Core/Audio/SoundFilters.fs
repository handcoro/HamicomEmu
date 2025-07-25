namespace HamicomEmu.Audio

module SoundFilters =

    open System

    let normalize (buf: float[]) =
        let maxAbs = buf |> Array.map abs |> Array.max
        if maxAbs > 0.0 then
            buf |> Array.map (fun x -> x / maxAbs)
        else
            buf

    /// オーバーサンプリング倍率数ごとに平均化して出力配列を作る
    let downsampleAverage (oversample: int) (input: float[]) =
        let samples = input.Length / oversample
        Array.init samples (fun i ->
            let sum = 
                let mutable acc = 0.
                for j in 0 .. oversample - 1 do
                    acc <- acc + input[i * oversample + j]
                acc
            sum / float oversample
        )

    /// 移動平均FIRフィルタ
    /// window: 3 - 15 くらい
    let movingAverage window (input: float[]) =
        let output = Array.zeroCreate input.Length
        let mutable sum = 0.
        for i in 0 .. input.Length - 1 do
            sum <- sum + input[i]
            if i >= window then
                sum <- sum - input[i - window]
            if i >= window - 1 then
                output[i] <- sum / float window
            else
                output[i] <- input[i] // 窓サイズに満たない間はそのまま
        output

    /// 単純一次ローパスフィルタ
    /// alpha: 0.15 - 0.25 くらい
    let lowpass alpha (input: float[]) =
        let output = Array.zeroCreate input.Length
        let mutable prev = 0.
        for i in 0..input.Length - 1 do
            let y = alpha * input[i] + (1. - alpha) * prev
            output[i] <- y
            prev <- y
        output

    /// 簡単な一次IIRハイパスフィルタ
    /// alpha: 0.95 - 0.995 くらい
    let highpass alpha (input: float[]) =
        let output = Array.zeroCreate input.Length
        let mutable prevInput = 0.
        let mutable prevOutput = 0.
        for i in 0 .. input.Length - 1 do
            let y = alpha * (prevOutput + input[i] - prevInput)
            output[i] <- y
            prevInput <- input[i]
            prevOutput <- y
        output

    /// 急激なジャンプ（例えば0.5以上）の箇所だけ直線補間する
    let linearInterpJumps threshold (input: float[]) =
        let output = Array.copy input
        for i in 1..input.Length - 1 do
            if abs (input[i] - input[i - 1]) > threshold then
                // 前値と今値の間を取る（重みは調整可）
                output[i] <- 0.8 * input[i - 1] + 0.2 * input[i]
        output

    let addDither (amplitude: float32) (input: float32 array) : float32 array =
        let rnd = Random()
        input |> Array.map (fun s -> s + amplitude * (float32 (rnd.NextDouble()) - 0.5f))

    let simpleFirLowpass (input: float[]) =
        let n = input.Length
        let output = Array.zeroCreate n
        for i in 1..n-2 do
            output[i] <- (input[i - 1] + input[i] + input[i+1]) / 3.
        output[0] <- input[0]
        output[n-1] <- input[n - 1]
        output

    /// 差分型（FIR一次ハイパス）
    let simpleFirHighpass (input: float[]) =
        let output = Array.zeroCreate input.Length
        output[0] <- input[0]
        for i in 1 .. input.Length - 1 do
            output[i] <- input[i] - input[i - 1]
        output

    let medianFilter window (input: float[]) =
        let output = Array.zeroCreate input.Length
        let half = window / 2
        for i in 0 .. input.Length - 1 do
            let left = max 0 (i - half)
            let right = min (input.Length - 1) (i + half)
            let sorted = input[left..right] |> Array.sort
            output[i] <- sorted[sorted.Length / 2]
        output

    let firMovingAverageHighpass window (input: float[]) =
        let output = Array.zeroCreate input.Length
        let movingAvg = movingAverage window input
        for i in 0 .. input.Length - 1 do
            output[i] <- input[i] - movingAvg[i]
        output

    let firLowpass (weights: float[]) (input: float[]) =
        let n = weights.Length
        let output = Array.zeroCreate input.Length
        let half = n / 2
        for i in half .. input.Length - half - 1 do
            let mutable acc = 0.0
            for k in 0 .. n - 1 do
                acc <- acc + input[i + k - half] * weights[k]
            output[i] <- acc
        output

    let downsampleWithLowpass oversample input =
        let firLen = oversample * 3 // フィルタ窓幅は3倍以上
        let weights =
            // Blackman窓係数を生成
            Array.init firLen (fun n ->
                0.42 - 0.5 * cos (2.0 * Math.PI * float n / float (firLen - 1))
                + 0.08 * cos (4.0 * Math.PI * float n / float (firLen - 1))
            )
        let norm = Array.sum weights
        let normWeights = weights |> Array.map (fun w -> w / norm)

        let filtered = firLowpass normWeights input
        downsampleAverage oversample filtered

    let cleanDownsample oversample input =
        let medianed = medianFilter 3 input      // パルスや異常値の抑制
        let lp = movingAverage (oversample * 2 + 1) medianed // 2倍幅の移動平均ローパス
        downsampleAverage oversample lp

    let removeDC (input: float[]) =
        let avg = Array.average input
        input |> Array.map (fun x -> x - avg)
