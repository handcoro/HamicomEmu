namespace HamicomEmu.Apu

module Mixer =

    open HamicomEmu.Apu.Types

    // ミキサー入力型定義
    type ChannelOutputs = {
        pulse1: byte
        pulse2: byte
        triangle: byte
        noise: byte
        dmc: byte
    }

    // ミキサー係数（NES実機のミキサー回路を模した値）
    [<Literal>]
    let private pulseCoeffA = 95.88f

    [<Literal>]
    let private pulseCoeffB = 8128.0f

    [<Literal>]
    let private pulseCoeffC = 100.0f

    [<Literal>]
    let private tndCoeff = 159.79f

    [<Literal>]
    let private tndCoeffT = 8227.0f

    [<Literal>]
    let private tndCoeffN = 12241.0f

    [<Literal>]
    let private tndCoeffD = 22638.0f

    [<Literal>]
    let private tndCoeffC = 100.0f

    /// 1 サンプル合成出力 (最適化版: キャッシュ局所性向上)
    /// NES実機の回路を模したミキサー
    /// https://www.nesdev.org/wiki/APU_Mixer
    let inline mix (outputs: ChannelOutputs) : float32 =

        // 出力値を直接int変換で取得（キャッシュ局所性向上）
        let ch1 = int outputs.pulse1
        let ch2 = int outputs.pulse2
        let ch3 = int outputs.triangle
        let ch4 = int outputs.noise
        let ch5 = int outputs.dmc

        // Pulse ミキサー: ch1 + ch2 が 0 なら結果も 0
        let pulseSum = ch1 + ch2
        let pulseMix =
            if pulseSum = 0 then
                0.0f
            else
                pulseCoeffA / (pulseCoeffB / float32 pulseSum + pulseCoeffC)

        // TND ミキサー: t, n, d いずれかが 0 でない場合のみ計算
        let t = float32 ch3
        let n = float32 ch4
        let d = float32 ch5

        let tndMix =
            if t = 0.0f && n = 0.0f && d = 0.0f then
                0.0f
            else
                tndCoeff / (1.0f / (t / tndCoeffT + n / tndCoeffN + d / tndCoeffD) + tndCoeffC)

        pulseMix + tndMix
