namespace HamicomEmu.Apu

module Dmc =

    open HamicomEmu.Apu.Types

    let init = {
        irqEnabled = false
        isLoop = false
        rateIndex = 0uy

        outputLevel = 0uy

        startAddress = 0uy
        sampleLength = 0uy

        currentAddress = 0us
        bytesRemaining = 0us
        buffer = None

        shiftRegister = 0uy
        bitsRemaining = 0

        timer = 0us
        isSilence = false

        irqRequested = false

        lastOutput = 0uy
    }

    let startSample dmc = {
        dmc with
            currentAddress = 0xC000us + (uint16 dmc.startAddress <<< 6)
            bytesRemaining = uint16 dmc.sampleLength <<< 4 |> (+) 1us
    }

    let stopSample dmc = { dmc with bytesRemaining = 0us }

    let needsSampleRead dmc =
        dmc.buffer.IsNone && dmc.bytesRemaining > 0us

    let applySampleRead value dmc =
        let nextAddr =
            if dmc.currentAddress = 0xFFFFus then
                0x8000us
            else
                dmc.currentAddress + 1us

        let nextRemaining = dmc.bytesRemaining - 1us

        let irqRequested = not dmc.isLoop && nextRemaining = 0us && dmc.irqEnabled

        let dmc' = {
            dmc with
                buffer = Some value
                currentAddress = nextAddr
                bytesRemaining = nextRemaining
                irqRequested = irqRequested
        }

        if nextRemaining = 0us && dmc.isLoop then
            startSample dmc'
        else
            dmc'

    /// 1 bit ずつ判定して音量を上げ下げする
    let tick (dmc: DmcState) =
        let mutable dmc = dmc

        if dmc.timer > 0us then
            dmc.timer <- dmc.timer - 1us

            dmc, None, None
        else
            dmc.timer <- uint16 Constants.dmcRateTable[int dmc.rateIndex]

            let stall =
                if dmc.bitsRemaining <> 0 then
                    None
                else
                    // バッファをシフトレジスタに移す
                    match dmc.buffer with
                    | Some byte ->
                        dmc.shiftRegister <- byte
                        dmc.bitsRemaining <- 8
                        dmc.buffer <- None
                        dmc.isSilence <- false
                        Some 4u // バッファの読み込みで 4 サイクルストール

                    | None ->
                        if dmc.bytesRemaining <> 0us then
                            None // バッファまち
                        else
                            dmc.isSilence <- true
                            None

            // 1 bit 処理
            let bit = dmc.shiftRegister &&& 1uy
            let shift' = dmc.shiftRegister >>> 1

            let level =
                if dmc.isSilence then
                    dmc.outputLevel
                elif bit = 1uy && dmc.outputLevel <= 125uy then
                    dmc.outputLevel + 2uy
                elif bit = 0uy && dmc.outputLevel >= 2uy then
                    dmc.outputLevel - 2uy
                else
                    dmc.outputLevel

            dmc.shiftRegister <- shift'
            dmc.bitsRemaining <- max 0 (dmc.bitsRemaining - 1)
            dmc.outputLevel <- level

            // 必要なら Bus.tick でメモリ読み込みの要求をしてバッファに格納
            let req =
                if needsSampleRead dmc then
                    Some {
                        addr = dmc.currentAddress
                        onRead = fun byte -> applySampleRead byte dmc
                    }
                else
                    None

            dmc, req, stall

    let output dmc = dmc.outputLevel
