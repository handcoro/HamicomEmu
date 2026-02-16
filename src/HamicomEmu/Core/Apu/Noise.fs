namespace HamicomEmu.Apu

module Noise =

    open HamicomEmu.Apu.Types

    let init = {
        volume = 0uy
        loopAndHalt = false
        isConstant = false

        periodIndex = 0
        timer = 0us
        isShortMode = false

        envelope = Envelope.init

        lengthCounter = 1uy
        lfsr = 1us
    }

    /// ノイズ生成
    /// シフトレジスタをいじって疑似乱数を生む
    let private nextNoise lfsr isShortMode =
        let x = if isShortMode then 6 else 1 // 比較するビットを周期モードによって変える
        let feedback = (lfsr &&& 1us) ^^^ (lfsr >>> x &&& 1us)
        let shifted = lfsr >>> 1
        let newLfsr = feedback <<< 14 ||| shifted
        newLfsr

    let tick (noi: NoiseState) =
        if noi.timer = 0us then
            noi.timer <- Constants.noisePeriods[noi.periodIndex] |> uint16
            noi.lfsr <- nextNoise noi.lfsr noi.isShortMode
        else
            noi.timer <- noi.timer - 1us
        
        noi


    /// ノイズチャンネルの 1 サンプルを生成する
    /// 以下の場合に出力:
    /// * シフトレジスタの bit 0 がセットされていない
    /// * 長さカウンタが 0 でない
    let output (noi: NoiseState) =
        if noi.lengthCounter = 0uy then
            0uy
        else
            let bit = noi.lfsr &&& 1us

            let sample =
                if bit = 0us then
                    if noi.isConstant then noi.volume else noi.envelope.volume
                else
                    0uy

            sample
