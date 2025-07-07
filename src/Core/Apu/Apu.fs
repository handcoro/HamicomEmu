namespace HamicomEmu.Apu

module Apu =

  open HamicomEmu.Apu.Types
  open HamicomEmu.Common
  open HamicomEmu.Common.BitUtils

  let initialLowPassFilter = { lastOutput = 0.0f }
  let initialFrameCounter = {
    mode = FourStep
    irqInhibit = false
    irqRequested = false
  }

  let initial = {
    pulse1 = Pulse.initial One
    pulse2 = Pulse.initial Two
    triangle = Triangle.initial
    noise = Noise.initial
    dmc = Dmc.initial
    filterState = initialLowPassFilter
    status = 0uy
    frameCounter = initialFrameCounter
    cycle = 0u
    step = Step1
  }

  let hasLoopFlag v = hasFlag GeneralMasks.envelopeLoopFlag v

  /// TODO: DMC 関連の操作
  let writeToStatus value apu =
    let pulse1Enabled = hasFlag StatusFlags.pulse1Enable value
    let pulse2Enabled = hasFlag StatusFlags.pulse2Enable value
    let triEnabled = hasFlag StatusFlags.triangleEnable value
    let noiseEnabled = hasFlag StatusFlags.noiseEnable value
    let dmcEnabled = hasFlag StatusFlags.dmcEnable value

    let pulse1Counter = if pulse1Enabled then apu.pulse1.lengthCounter else 0uy
    let pulse2Counter = if pulse2Enabled then apu.pulse2.lengthCounter else 0uy
    let triCounter = if triEnabled then apu.triangle.lengthCounter else 0uy
    let noiCounter = if noiseEnabled then apu.noise.lengthCounter else 0uy

    let dmc =
      if dmcEnabled then
        apu.dmc |> Dmc.startSample
      else
        apu.dmc
        |> Dmc.stopSample
        |> fun d -> { d with irqRequested = false }

    let status =
      value
      |> clearFlag StatusFlags.dmcInterrupt
    
    printfn "write to APU status: %02X" value

    { apu with
        pulse1.lengthCounter = pulse1Counter
        pulse2.lengthCounter = pulse2Counter
        triangle.lengthCounter = triCounter
        noise.lengthCounter = noiCounter
        dmc = dmc
        status = status
    }

  let private tickEnvelopeAndLinear apu =

    let ch1Ev = Envelope.tick apu.pulse1.envelope apu.pulse1.volume apu.pulse1.loopAndHalt
    let ch2Ev = Envelope.tick apu.pulse2.envelope apu.pulse2.volume apu.pulse2.loopAndHalt
    let ch3 = Triangle.tickLinearCounter apu.triangle
    let ch4Ev = Envelope.tick apu.noise.envelope apu.noise.volume apu.noise.loopAndHalt

    apu.pulse1.envelope <- ch1Ev
    apu.pulse2.envelope <- ch2Ev
    apu.triangle <- ch3
    apu.noise.envelope <- ch4Ev

    apu

  let private tickLengthAndSweep apu =
    let ch1 =
      apu.pulse1
      |> LengthCounter.tickPulse
      |> SweepUnit.tick

    let ch2 =
      apu.pulse2
      |> LengthCounter.tickPulse
      |> SweepUnit.tick

    let ch3 = LengthCounter.tickTriangle apu.triangle
    let ch4 = LengthCounter.tickNoise apu.noise

    apu.pulse1 <- ch1
    apu.pulse2 <- ch2
    apu.triangle <- ch3
    apu.noise <- ch4

    apu

  // mode 0:    mode 1:       function
  // ---------  -----------  -----------------------------
  // - - - f    - - - - -    IRQ (if bit 6 is clear)
  // - l - l    - l - - l    Length counter and sweep
  // e e e e    e e e - e    Envelope and linear counter
  let private runFrameStep step mode apu =
    match step with
    | Step1 | Step3 ->
      apu |> tickEnvelopeAndLinear

    | Step2 ->
      apu |> tickEnvelopeAndLinear
          |> tickLengthAndSweep

    | Step4 when mode = FourStep ->
      apu |> tickEnvelopeAndLinear
          |> tickLengthAndSweep
          // NOTE: ここでレコードを再生成すると状態の一貫性が保たれなくなる
          //       このように代入するか、Bus.tick でレコードを再生成して整合性を取ることで対処
          |> fun a ->
            if not a.frameCounter.irqInhibit then
              a.frameCounter.irqRequested <- true
              a
            else a

    | Step5 when mode = FiveStep ->
      apu |> tickEnvelopeAndLinear
          |> tickLengthAndSweep

    | _ ->
      apu

  let private nextStep = function
  | Step1 -> Step2
  | Step2 -> Step3
  | Step3 -> Step4
  | Step4 -> Step5
  | Step5 -> Step1

  /// APU のサイクルを進める
  /// 4-step モードは 240Hz で 1 フレーム
  let tick n apu : TickResult =
    let mutable apu = apu
    apu.cycle <- apu.cycle + n

    let mode = apu.frameCounter.mode

    let dmc', req, stall = Dmc.tick apu.dmc
    apu.dmc <- dmc'

    if apu.cycle < Constants.frameStepCycles then
      { apu = apu; dmcRead = req; stallCpuCycles = stall }

    else
      apu.cycle <- apu.cycle - Constants.frameStepCycles

      // フレームステップ更新
      apu <- runFrameStep apu.step mode apu

      // ステップカウンタのロールオーバー
      match apu.step, mode with
      | Step4, FourStep | Step5, FiveStep -> apu.step <- Step1
      | _ -> apu.step <- nextStep apu.step


      { apu = apu; dmcRead = req; stallCpuCycles = stall }

  let getReadRequest apu =
    if Dmc.needsSampleRead apu.dmc then
      Some (DmcSampleRead apu.dmc.currentAddress)
    else
      None

  let writeToFrameCounter v apu =
    let mode = if hasFlag FrameCounterFlags.mode v then FiveStep else FourStep
    let irqInhibit = hasFlag FrameCounterFlags.irqInhibit v
    let irqReq = if irqInhibit then false else apu.frameCounter.irqRequested
    let fc = { mode = mode; irqInhibit = irqInhibit; irqRequested = irqReq }

    apu.cycle <- 0u
    apu.step <- Step1
    
    let apu =
      if mode = FiveStep then
        apu
        |> tickEnvelopeAndLinear
        |> tickLengthAndSweep
      else
        apu

    { apu with frameCounter = fc }


  /// 00: 12.5%, 01: 25%, 10: 50%, 11: 75%
  let private parseDuty v = v &&& PulseBitMasks.dutyCycleMask >>> 6

  let private parseLengthCounter v = Constants.lengthTable[int v >>> 3]

  let private parseVolumeControlPulse (pulse : PulseState) v =
    { pulse with
        volume = PulseBitMasks.volumeMask &&& v
        duty = parseDuty v
        isConstant = hasFlag PulseBitMasks.constantVolumeFlag v
        loopAndHalt = hasFlag PulseBitMasks.lengthCounterHaltFlag v
    }

  let private parseSweep v = {
    enabled = hasFlag PulseBitMasks.sweepFlag v
    negate = hasFlag PulseBitMasks.sweepNegateFlag v
    period = v &&& PulseBitMasks.sweepPeriodMask >>> 4
    shift = v &&& PulseBitMasks.sweepShiftMask
    reload = true
    divider = 0uy
  }


  let private parselinearCounterTriangle v = TriangleBitMasks.linearCounterMask &&& v

  let private parseControlAndHaltTriangle v = hasFlag TriangleBitMasks.controlFlag v

  let private parseVolumeControlNoise (noise : NoiseState) v =
    { noise with
        volume = NoiseBitMasks.volumeMask &&& v
        isConstant = hasFlag NoiseBitMasks.constantVolumeFlag v
        loopAndHalt = hasFlag NoiseBitMasks.lengthCounterHaltFlag v
    }
  
  let private parsePeriodIndexNoise v = NoiseBitMasks.periodMask &&& v

  let private parseModeNoise v = hasFlag NoiseBitMasks.modeFlag v

  /// タイマーの下位 8 ビットを更新
  let private updateTimerLo timer lo =
    let hi = timer &&& (uint16 PulseBitMasks.timerHiMask <<< 8)
    let lo = uint16 lo
    hi ||| lo

  /// タイマーの上位 3 ビットを更新
  let private updateTimerHi timer hi =
    let hi = hi &&& GeneralMasks.timerHiMask |> uint16 <<< 8
    let lo = timer &&& 0xFFus
    hi ||| lo

  let private parseRateIndexDmc v = DmcBitMasks.rateIndexMask &&& v

  let private parseIrqEnabledDmc v = hasFlag DmcBitMasks.irqEnabledFlag v

  let private parseLoopDmc v = hasFlag DmcBitMasks.loopFlag v

  let private parseDirectLoad v = DmcBitMasks.directLoadMask &&& v

  let write addr value apu =

    match addr with
    // Ch1: 矩形波
    | 0x4000us ->
      { apu with pulse1 = parseVolumeControlPulse apu.pulse1 value }
    | 0x4001us ->
      { apu with pulse1.sweep = parseSweep value }
    | 0x4002us -> // timer lo 部分の上書き
      let timer = updateTimerLo apu.pulse1.timer value
      { apu with pulse1.timer = timer }
    | 0x4003us -> // timer hi と長さカウンタ
      let timer = updateTimerHi apu.pulse1.timer value
      let c = parseLengthCounter value
      { apu with
          pulse1.timer = timer
          pulse1.envelope.reload = true
          pulse1.lengthCounter = c
      }
    // Ch2: 矩形波
    | 0x4004us ->
      { apu with pulse2 = parseVolumeControlPulse apu.pulse2 value}
    | 0x4005us ->
      { apu with pulse2.sweep = parseSweep value }
    | 0x4006us ->
      let timer = updateTimerLo apu.pulse2.timer value
      { apu with pulse2.timer = timer }
    | 0x4007us ->
      let timer = updateTimerHi apu.pulse2.timer value
      let c = parseLengthCounter value
      { apu with
          pulse2.timer = timer
          pulse2.envelope.reload = true
          pulse2.lengthCounter = c
      }
    // Ch3: 三角波
    | 0x4008us ->
      { apu with
          triangle.linearCounterLoad = parselinearCounterTriangle value
          triangle.ctrlAndHalt = parseControlAndHaltTriangle value
          triangle.linearReloadFlag = true
      }
    | 0x400Aus ->
      let timer = updateTimerLo apu.triangle.timer value
      { apu with triangle.timer = timer }
    | 0x400Bus ->
      let c = parseLengthCounter value
      let timer = updateTimerHi apu.triangle.timer value
      { apu with
          triangle.timer = timer
          triangle.lengthCounter = c
          triangle.linearReloadFlag = true
      }
    // Ch4: ノイズ
    | 0x400Cus ->
      { apu with noise = parseVolumeControlNoise apu.noise value }
    | 0x400Eus ->
      let pIdx = parsePeriodIndexNoise value
      let mode = parseModeNoise value
      { apu with noise.periodIndex = pIdx; noise.isShortMode = mode }
    | 0x400Fus ->
      let c = parseLengthCounter value
      { apu with
          noise.envelope.reload = true
          noise.lengthCounter = c
      }
    | 0x4010us -> // flags and rate
      let irq = parseIrqEnabledDmc value
      let loop = parseLoopDmc value
      let rateIdx = parseRateIndexDmc value
      { apu with
          dmc.irqEnabled = irq
          dmc.irqRequested = if not irq then false else apu.dmc.irqRequested
          dmc.isLoop = loop
          dmc.rateIndex = rateIdx
      }
    | 0x4011us -> // direct load
      { apu with dmc.outputLevel = parseDirectLoad value }
    | 0x4012us -> // sample address
      { apu with dmc.startAddress = value }
    | 0x4013us -> // sample length
      { apu with dmc.sampleLength = value }

    | 0x4015us ->
      let apu' = writeToStatus value apu
      apu'

    | 0x4017us ->
      // * If the write occurs during an APU cycle, the effects occur 3 CPU cycles after the $4017 write cycle,
      //   and if the write occurs between APU cycles, the effects occurs 4 CPU cycles after the write cycle. 
      // 細かい…
      let apu' = writeToFrameCounter value apu
      apu'

    | _ ->
      // printfn "This APU register is not implemented yet. %04X" addr
      apu
  
  let read addr apu =
    match addr with
    | 0x4015us -> // TODO: オープンバスの挙動があるけど優先度は低い
      let frameIrq = apu.frameCounter.irqRequested
      let dmcIrq = apu.dmc.irqRequested

      let dmcCond = apu.dmc.bytesRemaining > 0us
      let noiCond = hasFlag StatusFlags.noiseEnable apu.status && apu.noise.lengthCounter > 0uy
      let triCond = hasFlag StatusFlags.triangleEnable apu.status && apu.triangle.lengthCounter > 0uy
      let p2Cond = hasFlag StatusFlags.pulse2Enable apu.status && apu.pulse2.lengthCounter > 0uy
      let p1Cond = hasFlag StatusFlags.pulse1Enable apu.status && apu.pulse1.lengthCounter > 0uy
      let status' =
        apu.status
        |> updateFlag StatusFlags.frameInterrupt frameIrq
        |> updateFlag StatusFlags.dmcInterrupt dmcIrq
        |> updateFlag StatusFlags.dmcActive dmcCond
        |> updateFlag StatusFlags.noiseLengthCounterLargerThanZero noiCond
        |> updateFlag StatusFlags.triangleLengthCounterLargerThanZero triCond
        |> updateFlag StatusFlags.pulse2LengthCounterLargerThanZero p2Cond
        |> updateFlag StatusFlags.pulse1LengthCounterLargerThanZero p1Cond

      // フレーム割り込みは読み出し後にクリアされる
      apu.frameCounter.irqRequested <- false
      let clearedStatus =
        status'
        |> clearFlag StatusFlags.frameInterrupt

      status', { apu with status = clearedStatus }
    | _ ->
      // printfn "This APU register is not implemented yet. %04X" addr
      0uy, apu

  /// 一次 IIR ローパスフィルタ
  let nextLowPass alpha input (state: LowPassFilterState) =
    let y = alpha * input + (1.0f - alpha) * state.lastOutput
    y, { state with lastOutput = y }

  /// 1 サンプル合成出力（dtベース）
  let mix (dt: float) apu =
    let ch1, pu1 = Pulse.output dt apu.pulse1
    let ch2, pu2 = Pulse.output dt apu.pulse2
    let ch3, tri = Triangle.output dt apu.triangle
    let ch4, noi = Noise.output dt apu.noise

    let samplesToPop = int (dt * Constants.cpuClockNTSC)

    let dmcSamples, dmc' = Dmc.popSamples samplesToPop apu.dmc
    let ch5 =
      match dmcSamples with
      | hd::_ -> hd  // 先頭のサンプルを採用
      | [] -> dmc'.lastOutput

    let ch1 = ch1 |> int
    let ch2 = ch2 |> int
    let ch3 = ch3 |> int
    let ch4 = ch4 |> int
    let ch5 = ch5 |> int

    let masterGain = 1.0f

    // 実機の回路を模したミキサーらしい
    // https://www.nesdev.org/wiki/APU_Mixer
    let pulseMix =
      if ch1 + ch2 = 0 then 0.0f
      else 95.88f / (8128.0f / float32(ch1 + ch2) + 100.0f)

    let tndMix =
      let t, n, d = float32 ch3 , float32 ch4, float32 ch5
      if t = 0.0f && n = 0.0f && d = 0.0f then 0.0f
      else 159.79f / (1.0f / (t/8227.0f + n/12241.0f + d /22638.0f) + 100.0f)

    let sample = (pulseMix + tndMix) * masterGain

    // NOTE: ローパスフィルタ実装は検討中
    // let sample, filterState = nextLowPass 0.18f sample apu.filterState
    // apu.filterState <- filterState

    apu.pulse1 <- pu1
    apu.pulse2 <- pu2
    apu.triangle <- tri
    apu.noise <- noi
    apu.dmc <- dmc'

    sample, apu
