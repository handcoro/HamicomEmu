namespace HamicomEmu.Apu

module Dmc =

  open HamicomEmu.Apu.Types

  let initial = {
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

    outputBuffer = ResizeArray()
    lastOutput = 0uy
  }

  let startSample dmc = {
    dmc with
      currentAddress = 0xC000us + (uint16 dmc.startAddress <<< 6)
      bytesRemaining = uint16 dmc.sampleLength <<< 4 |> (+) 1us
  }

  let stopSample dmc = { dmc with bytesRemaining = 0us }

  let needsSampleRead dmc = dmc.buffer.IsNone && dmc.bytesRemaining > 0us

  let applySampleRead value dmc =
    let nextAddr =
      if dmc.currentAddress = 0xFFFFus then 0x8000us
      else dmc.currentAddress + 1us

    let nextRemaining = dmc.bytesRemaining - 1us

    let dmc' = {
      dmc with
        buffer = Some value
        currentAddress = nextAddr
        bytesRemaining = nextRemaining
        irqRequested = false
    }

    if nextRemaining = 0us && dmc.isLoop then startSample dmc' else dmc'

  let clearOutputBuffer dmc = dmc.outputBuffer.Clear

  let popSample dmc =
    if dmc.outputBuffer.Count > 0 then
      let value = dmc.outputBuffer[0]
      dmc.outputBuffer.RemoveAt(0)
      value, dmc
    else
      0uy, dmc

  let popSamples (n: int) (dmc: DmcState) =
    let taken = ResizeArray<byte>()
    let count = min n dmc.outputBuffer.Count
    for _ in 1 .. count do
      taken.Add(dmc.outputBuffer[0])
      dmc.outputBuffer.RemoveAt(0)
    Seq.toList taken, dmc

  /// 
  /// 1 bit ずつ判定して音量を上げ下げする
  let tick (dmc : DmcState) =
    let irqRequested =
      not dmc.isLoop &&
      dmc.bytesRemaining = 0us &&
      dmc.irqEnabled

    if dmc.timer > 0us then
      { dmc with timer = dmc.timer - 1us }, None, None
    else
      let dmc = { dmc with timer = uint16 Constants.dmcRateTable[int dmc.rateIndex] }

      let dmc, stall =
        if dmc.bitsRemaining <> 0 then dmc, None else
        // バッファをシフトレジスタに移す
        match dmc.buffer with
        | Some byte ->
          { dmc with
              shiftRegister = byte
              bitsRemaining = 8
              buffer = None
              isSilence = false }, Some 4u // バッファの読み込みで 4 サイクルストール
        | None ->
          if dmc.bytesRemaining <> 0us then
            dmc, None // バッファまち
          else
            { dmc with
                isSilence = true
                irqRequested = irqRequested }, None

      // 1 bit 処理
      let bit = dmc.shiftRegister &&& 1uy
      let shift' = dmc.shiftRegister >>> 1
      let level =
        if dmc.isSilence then dmc.outputLevel
        elif bit = 1uy && dmc.outputLevel <= 125uy then dmc.outputLevel + 2uy
        elif bit = 0uy && dmc.outputLevel >= 2uy then dmc.outputLevel - 2uy
        else dmc.outputLevel

      let output = dmc.outputLevel

      dmc.outputBuffer.Add(output)

      let dmc = {
        dmc with
          shiftRegister = shift'
          bitsRemaining = max 0 (dmc.bitsRemaining - 1)
          outputLevel = level
          // バッファが切れたときの保険用
          // 色々なエミュレーションがきちんと実装できてきたらいらなくなるはず
          lastOutput = output
      }

      // 必要なら Bus.tick でメモリ読み込みの要求をしてバッファに格納
      let req =
        if needsSampleRead dmc then
          Some {
            addr = dmc.currentAddress
            onRead = fun byte -> applySampleRead byte dmc
          }
        else None

      dmc, req, stall

