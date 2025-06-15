namespace HamicomEmu.Apu

module Apu =

  open HamicomEmu.Apu.Types

  let hasFlag flag b = b &&& flag <> 0uy
  let setFlag flag b = b ||| flag
  let clearFlag flag b = b &&& (~~~flag)
  let updateFlag flag condition b =
    if condition then setFlag flag b else clearFlag flag b

  let initialEnvelope = {
    volume = 0uy
    divider = 0uy
    decay = 0uy
    reload = false
  }

  let initialSweep = {
    enabled = false
    negate = false
    period = 0uy
    shift = 0uy
    reload = false
    divider = 0uy
  }

  let parseSweep v = {
    enabled = hasFlag PulseBitMasks.sweepFlag v
    negate = hasFlag PulseBitMasks.sweepNegateFlag v
    period = v &&& PulseBitMasks.sweepPeriodMask >>> 4
    shift = v &&& PulseBitMasks.sweepShiftMask
    reload = true
    divider = 0uy
  }

  let initialPulse ch = {
    channel = ch
    volume = 0uy
    duty = 0uy
    loopAndHalt = false
    isConstant = false

    sweep = initialSweep
    timer = 0us

    envelope = initialEnvelope

    lengthCounter = 1uy

    phase = 0.0
  }


  let initialTriangle = {
    linearCounterLoad = 0uy
    ctrlAndHalt = false

    timer = 0us

    linearCounter = 0uy
    linearReloadFlag = false

    lengthCounter = 1uy

    phase = 0.0
  }

  let initialNoise = {
    volume = 0uy
    loopAndHalt = false
    isConstant = false

    periodIndex = 0uy
    isShortMode = false

    envelope = initialEnvelope

    lengthCounter = 1uy
    shift = 1us
    phase = 0.0
  }

  let initialDeltaModulation = {
    irqEnabled = false
    isLoop = false
    rateIndex = 0uy

    sampleAddress = 0uy
    sampleLength = 0uy

    sampleBuffer = 0uy
    lengthCounter = 0uy
    currentAddress = 0us
    bytesRemaining = 0uy
    shiftResister = 0uy
    bitCounter = 0

    outputLevel = 0uy
    timer = 0
    isSilence = false
  }

  let initialFrameCounter = {
    mode = FourStep
    irqInhibit = false
  }

  let initial = {
    pulse1 = initialPulse One
    pulse2 = initialPulse Two
    triangle = initialTriangle
    noise = initialNoise
    status = 0uy
    frameCounter = initialFrameCounter
    cycle = 0u
    step = Step1
    irq = false
  }

  let hasLoopFlag v = hasFlag GeneralMasks.envelopeLoopFlag v

  /// 00: 12.5%, 01: 25%, 10: 50%, 11: 75%
  let parseDuty v = v &&& PulseBitMasks.dutyCycleMask >>> 6
  let lengthTable = [|
    10uy; 254uy; 20uy;  2uy; 40uy;  4uy; 80uy;  6uy;
    160uy; 8uy; 60uy; 10uy; 14uy; 12uy; 26uy; 14uy;
    12uy; 16uy; 24uy; 18uy; 48uy; 20uy; 96uy; 22uy;
    192uy; 24uy; 72uy; 26uy; 16uy; 28uy; 32uy; 30uy
  |]

  let parseLengthCounter v = lengthTable[int v >>> 3]

  let parseVolumeControlPulse (pulse : PulseChannel) v =
    { pulse with
        volume = PulseBitMasks.volumeMask &&& v
        duty = parseDuty v
        isConstant = hasFlag PulseBitMasks.constantVolumeFlag v
        loopAndHalt = hasFlag PulseBitMasks.lengthCounterHaltFlag v
    }

  let parselinearCounterTriangle v = TriangleBitMasks.linearCounterMask &&& v

  let parseControlAndHaltTriangle v = hasFlag TriangleBitMasks.controlFlag v

  let parseVolumeControlNoise (noise : NoiseChannel) v =
    { noise with
        volume = NoiseBitMasks.volumeMask &&& v
        isConstant = hasFlag NoiseBitMasks.constantVolumeFlag v
        loopAndHalt = hasFlag NoiseBitMasks.lengthCounterHaltFlag v
    }
  
  let parsePeriodIndexNoise v = NoiseBitMasks.periodMask &&& v

  let parseModeNoise v = hasFlag NoiseBitMasks.modeFlag v

  /// タイマーの下位 8 ビットを更新
  let updateTimerLo timer lo =
    let hi = timer &&& (uint16 PulseBitMasks.timerHiMask <<< 8)
    let lo = uint16 lo
    hi ||| lo

  /// タイマーの上位 3 ビットを更新
  let updateTimerHi timer hi =
    let hi = hi &&& GeneralMasks.timerHiMask |> uint16 <<< 8
    let lo = timer &&& 0xFFus
    hi ||| lo


  /// エンベロープを進める
  let tickEnvelope (ev : EnvelopeState) reload loop =
    let vol =
      if ev.reload then
        ev.divider <- reload
        ev.decay <- 15uy
        ev.reload <- false
        15uy
      else if ev.divider = 0uy then // 分周器の励起
        ev.divider <- reload
        if ev.decay = 0uy then
          if loop then 15uy else 0uy
        else
          ev.decay <- ev.decay - 1uy
          ev.decay
      else
        ev.divider <- ev.divider - 1uy
        ev.decay

    { ev with volume = vol }

  
  /// TODO: DMC 関連の操作
  let writeToStatus value apu =
    let pulse1EnableCond = hasFlag StatusFlags.pulse1Enable value
    let pulse2EnableCond = hasFlag StatusFlags.pulse2Enable value
    let triEnableCond = hasFlag StatusFlags.triangleEnable value
    let noiseEnableCond = hasFlag StatusFlags.noiseEnable value 

    let pulse1Counter = if pulse1EnableCond then apu.pulse1.lengthCounter else 0uy
    let pulse2Counter = if pulse2EnableCond then apu.pulse2.lengthCounter else 0uy
    let triCounter = if triEnableCond then apu.triangle.lengthCounter else 0uy
    let noiCounter = if noiseEnableCond then apu.noise.lengthCounter else 0uy

    let status' =
      value
      |> clearFlag StatusFlags.dmcInterrupt

    { apu with
        pulse1.lengthCounter = pulse1Counter
        pulse2.lengthCounter = pulse2Counter
        triangle.lengthCounter = triCounter
        noise.lengthCounter = noiCounter
        status = status'
    }

  let tickEnvelopeAndLinear apu =

    let ch1Ev = tickEnvelope apu.pulse1.envelope apu.pulse1.volume apu.pulse1.loopAndHalt
    let ch2Ev = tickEnvelope apu.pulse2.envelope apu.pulse2.volume apu.pulse2.loopAndHalt
    let ch3 = Triangle.tickLinearCounter apu.triangle
    let ch4Ev = tickEnvelope apu.noise.envelope apu.noise.volume apu.noise.loopAndHalt

    { apu with
        pulse1.envelope = ch1Ev
        pulse2.envelope = ch2Ev
        triangle = ch3
        noise.envelope = ch4Ev
    }

  let tickLengthAndSweep apu =
    let ch1 =
      apu.pulse1
      |> Pulse.tickLengthCounter
      |> Pulse.tickSweep

    let ch2 =
      apu.pulse2
      |> Pulse.tickLengthCounter
      |> Pulse.tickSweep

    let ch3 = Triangle.tickLengthCounter apu.triangle
    let ch4 = Noise.tickLengthCounter apu.noise

    { apu with
        pulse1 = ch1
        pulse2 = ch2
        triangle = ch3
        noise = ch4
    }

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
      let irq = not apu.frameCounter.irqInhibit
      apu |> tickEnvelopeAndLinear
          |> tickLengthAndSweep
          |> fun a -> { a with irq = irq }
    | Step5 when mode = FiveStep ->
      apu |> tickEnvelopeAndLinear
          |> tickLengthAndSweep
    | _ ->
      printfn "Invalid APU frame step: %A" step
      apu

  let private nextStep = function
  | Step1 -> Step2
  | Step2 -> Step3
  | Step3 -> Step4
  | Step4 -> Step5
  | Step5 -> Step1

  let frameStepCycles = 7457u // 1 step のサイクル数

  /// APU のサイクルを進める
  /// 4-step モードは 240Hz で 1 フレーム
  let tick n apu =
    apu.cycle <- apu.cycle + n

    if apu.cycle < frameStepCycles then
      apu
    else
      apu.cycle <- apu.cycle - frameStepCycles

      let mode = if hasFlag FrameCounterFlags.mode apu.status then FiveStep else FourStep
      let apu' = runFrameStep apu.step mode apu

      apu'.step <- nextStep apu.step

      match apu'.step, mode with
      | Step4, FourStep | Step5, FiveStep -> apu'.step <- Step1
      | _ -> ()

      apu'

  let rec tickNTimes n apu =
    if n <= 0 then apu
    else
      let apu' = tick 1u apu
      tickNTimes (n - 1) apu'

  let writeToFrameCounter v apu =
    let mode = if hasFlag FrameCounterFlags.mode v then FiveStep else FourStep
    let irqInhibit = hasFlag FrameCounterFlags.irqInhibit v
    let fc = { mode = mode; irqInhibit = irqInhibit }

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
    | 0x4015us ->
      let apu' = writeToStatus value apu
      apu'
    // TODO: DMC (DPCM) 関連の処理
    | 0x4010us -> // flags and rate
      apu
    | 0x4011us -> // direct load
      apu
    | 0x4012us -> // sample address
      apu
    | 0x4013us -> // sample length
      apu

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
    | 0x4015us -> // TODO: オープンバスの挙動
      let noiCond = hasFlag StatusFlags.noiseEnable apu.status && apu.noise.lengthCounter > 0uy
      let triCond = hasFlag StatusFlags.triangleEnable apu.status && apu.triangle.lengthCounter > 0uy
      let p2Cond = hasFlag StatusFlags.pulse2Enable apu.status && apu.pulse2.lengthCounter > 0uy
      let p1Cond = hasFlag StatusFlags.pulse1Enable apu.status && apu.pulse1.lengthCounter > 0uy
      let status' =
        apu.status
        |> clearFlag StatusFlags.frameInterrupt
        // TODO: DMC
        // |> updateFlag StatusFlags.deltaModulationActive (dmcBytes > 0) 
        |> updateFlag StatusFlags.noiseLengthCounterLargerThanZero noiCond
        |> updateFlag StatusFlags.triangleLengthCounterLargerThanZero triCond
        |> updateFlag StatusFlags.pulse2LengthCounterLargerThanZero p2Cond
        |> updateFlag StatusFlags.pulse1LengthCounterLargerThanZero p1Cond
      status', { apu with status = status' }
    | _ ->
      // printfn "This APU register is not implemented yet. %04X" addr
      0uy, apu

/// 1 サンプル合成出力（dtベース）
  let mix (dt: float) apu =
    let ch1, pu1 = Pulse.output dt apu.pulse1
    let ch2, pu2 = Pulse.output dt apu.pulse2
    let ch3, tri = Triangle.output dt apu.triangle
    let ch4, noi = Noise.output dt apu.noise

    let sample = (ch1 + ch2 + ch3 + ch4) * 0.25f
    sample, { apu with
                pulse1 = pu1
                pulse2 = pu2
                triangle = tri
                noise = noi
            }
