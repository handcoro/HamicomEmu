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

    outputBuffer = []
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

  let clearOutputBuffer dmc = { dmc with outputBuffer = [] }

  let popSample dmc =
    match dmc.outputBuffer with
    | x :: rest -> x, { dmc with outputBuffer = rest }
    | [] -> 0uy, dmc

  let popSamples (n: int) (dmc: DmcState) =
    let rec loop acc n buf =
      match n, buf with
      | 0, _ -> List.rev acc, buf
      | _, [] -> List.rev acc, []
      | n, x::xs -> loop (x::acc) (n - 1) xs

    let taken, rest = loop [] n dmc.outputBuffer
    taken, { dmc with outputBuffer = rest }

  /// 
  /// 1 bit ずつ判定して音量を上げ下げする
  let tick (dmc : DmcState) =
    let irqRequested =
      not dmc.isLoop &&
      dmc.bytesRemaining = 0us &&
      dmc.irqEnabled

    if dmc.timer > 0us then
      { dmc with timer = dmc.timer - 1us }, None
    else
      let dmc = { dmc with timer = uint16 Constants.dmcRateTable[int dmc.rateIndex] }

      let dmc =
        if dmc.bitsRemaining <> 0 then dmc else
        // バッファをシフトレジスタに移す
        match dmc.buffer with
        | Some byte ->
          { dmc with
              shiftRegister = byte
              bitsRemaining = 8
              buffer = None
              isSilence = false }
        | None ->
          if dmc.bytesRemaining <> 0us then
            dmc // バッファまち
          else
            { dmc with
                isSilence = true
                irqRequested = irqRequested }

      // 1 bit 処理
      let bit = dmc.shiftRegister &&& 1uy
      let shift' = dmc.shiftRegister >>> 1
      let level =
        if dmc.isSilence then dmc.outputLevel
        elif bit = 1uy && dmc.outputLevel <= 125uy then dmc.outputLevel + 2uy
        elif bit = 0uy && dmc.outputLevel >= 2uy then dmc.outputLevel - 2uy
        else dmc.outputLevel

      let output = dmc.outputLevel

      let dmc = {
        dmc with
          shiftRegister = shift'
          bitsRemaining = max 0 (dmc.bitsRemaining - 1)
          outputLevel = level
          outputBuffer = dmc.outputBuffer @ [output]
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

      dmc, req

