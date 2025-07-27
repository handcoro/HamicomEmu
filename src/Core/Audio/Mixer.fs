namespace HamicomEmu.Audio

module Mixer =

    let toPositive (blip: float) =
        (blip + 1.0) * 0.5 * 15.0

    /// 複数バッファをAPUミキサ式で合成
    let mixBuffers (pulse1: float[]) (pulse2: float[]) (triangle: float[]) (noise: float[]) (dmc: float[]) : float[] =
        let len = min pulse1.Length triangle.Length
        Array.init len (fun i ->
            let ch1 = pulse1[i]
            let ch2 = pulse2[i]
            let ch3 = triangle[i]
            let ch4 = noise[i]
            let ch5 = dmc[i]

            // 実機ミキサ式をそのまま流用
            let pulseMix =
                if ch1 + ch2 = 0. then
                    0.
                else
                    95.88 / (8128. / (ch1 + ch2) + 100.)

            let tndMix =
                if ch3 = 0. && ch4 = 0. && ch5 = 0. then
                    0.
                else
                    159.79 / (1. / (ch3 / 8227. + ch4 / 12241. + ch5 / 22638.) + 100.)

            pulseMix + tndMix // 必要に応じて正規化や masterGain 掛ける
        )
