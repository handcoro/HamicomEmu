namespace HamicomEmu.Apu

module ApuConstants =
  let cpuClockNTSC = 1_789_773.0 // NTSC NES の CPU クロック

module Registers =


  module GeneralMasks =
    // 矩形波とノイズ共通
    let volumeMask   = 0b0000_1111uy
    let envelopeMask = 0b0000_1111uy
    let constantVolumeFlag    = 0b0001_0000uy
    let envelopeLoopFlag      = 0b0010_0000uy
    let lengthCounterHaltFlag = 0b0010_0000uy
    
    // 矩形波と三角波とノイズ共通
    let lengthCounterMask = 0b1111_1000uy
    let timerHiMask       = 0b0000_0111uy

  module PulseBitMasks =
    // $4000, $4004
    let volumeMask            = 0b0000_1111uy
    let envelopeMask          = 0b0000_1111uy
    let constantVolumeFlag    = 0b0001_0000uy
    let envelopeLoopFlag      = 0b0010_0000uy
    let lengthCounterHaltFlag = 0b0010_0000uy
    let dutyCycleMask         = 0b1100_0000uy
    // $4001, $4005
    let sweepFlag       = 0b1000_0000uy
    let sweepPeriodMask = 0b0111_0000uy
    let sweepNegateFlag = 0b0000_1000uy
    let sweepShiftMask  = 0b0000_0111uy
    // $4003, $4007
    let timerHiMask       = 0b0000_0111uy
    let lengthCounterMask = 0b1111_1000uy

  module TriangleBitMasks =
    // $4008
    let linearCounterMask     = 0b0111_1111uy
    let controlFlag           = 0b1000_0000uy
    let lengthCounterHaltFlag = 0b1000_0000uy
    // $400B
    let timerHiMask       = 0b0000_0111uy
    let lengthCounterMask = 0b1111_1000uy

  module NoiseBitMasks =
    // $400C
    let volumeMask            = 0b0000_1111uy
    let envelopeMask          = 0b0000_1111uy
    let constantVolumeFlag    = 0b0001_0000uy
    let envelopeLoopFlag      = 0b0010_0000uy
    let lengthCounterHaltFlag = 0b0010_0000uy
    let modeFlag              = 0b1000_0000uy
    // $400E
    let periodMask        = 0b0000_1111uy
    let lengthCounterMask = 0b1111_1000uy
  
  module DeltaModulationBitMasks =
    // $4010
    let irqEnabledFlag = 0b1000_0000uy
    let loopFlag       = 0b0100_0000uy
    let rateIndexMask  = 0b0000_1111uy
    // $4011
    let directLoadMask = 0b0111_1111uy

  module StatusFlags =
    // $4015 write
    let deltaModulationEnable = 0b0001_0000uy
    let noiseEnable           = 0b0000_1000uy
    let triangleEnable        = 0b0000_0100uy
    let pulse2Enable          = 0b0000_0010uy
    let pulse1Enable          = 0b0000_0001uy
    // $4015 read
    let dmcInterrupt                        = 0b1000_0000uy
    let frameInterrupt                      = 0b0100_0000uy
    let deltaModulationActive               = 0b0001_0000uy
    let noiseLengthCounterLargerThanZero    = 0b0000_1000uy
    let triangleLengthCounterLargerThanZero = 0b0000_0100uy
    let pulse2LengthCounterLargerThanZero   = 0b0000_0010uy
    let pulse1LengthCounterLargerThanZero   = 0b0000_0001uy
  
  module FrameCounterFlags =
    // $4017
    let mode       = 0b1000_0000uy // 0 = 4-step, 1 = 5-step
    let irqInhibit = 0b0100_0000uy

  let hasFlag flag b = b &&& flag <> 0uy
  let setFlag flag b = b ||| flag
  let clearFlag flag b = b &&& (~~~flag)
  let updateFlag flag condition b =
    if condition then setFlag flag b else clearFlag flag b

  type EnvelopeState = {
    volume: byte
    mutable divider: byte
    mutable decay: byte
    mutable reload: bool
  }

  let initialEnvelope = {
    volume = 0uy
    divider = 0uy
    decay = 0uy
    reload = false
  }

  type SweepState = {
    enabled: bool
    negate: bool
    period: byte
    shift: byte
    mutable reload: bool
    mutable divider: byte
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

  type PulseChannel =
  | One | Two

  type Pulse = {
    channel: PulseChannel
    volume: byte
    duty: byte
    isLoopAndLengthCounterHalt: bool // エンベロープのループと長さカウンタ停止フラグ兼用
    isConstant: bool

    sweep: SweepState
    // timer は値が低いほど周波数が高くなる
    timer: uint16

    envelope: EnvelopeState

    // 長さカウンタ内部状態
    lengthCounter: byte

    phase: float
  }

  let initialPulse ch = {
    channel = ch
    volume = 0uy
    duty = 0uy
    isLoopAndLengthCounterHalt = false
    isConstant = false

    sweep = initialSweep
    timer = 0us

    envelope = initialEnvelope

    lengthCounter = 1uy

    phase = 0.0
  }

  type Triangle = {
    linearCounterLoad: byte
    isLinearCtrlAndLengthCounterHalt: bool

    timer: uint16

    linearCounter: byte
    linearReloadFlag: bool

    lengthCounter: byte

    phase: float
  }

  let initialTriangle = {
    linearCounterLoad = 0uy
    isLinearCtrlAndLengthCounterHalt = false

    timer = 0us

    linearCounter = 0uy
    linearReloadFlag = false

    lengthCounter = 1uy

    phase = 0.0
  }

  type Noise = {
    volume: byte
    isLoopAndLengthCounterHalt : bool
    isConstant : bool

    periodIndex: byte
    isShortMode: bool

    envelope: EnvelopeState

    lengthCounter: byte
    shift: uint16
    phase: float
  }
  let initialNoise = {
    volume = 0uy
    isLoopAndLengthCounterHalt = false
    isConstant = false

    periodIndex = 0uy
    isShortMode = false

    envelope = initialEnvelope

    lengthCounter = 1uy
    shift = 1us
    phase = 0.0
  }

  type FrameCounter = {
    irqInhibitMode: byte
  }

  let initialFrameCounter = {
    irqInhibitMode = 0uy;
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
 


  let parseVolumeControlPulse (pulse : Pulse) v =
    { pulse with
        volume = PulseBitMasks.volumeMask &&& v
        duty = parseDuty v
        isConstant = hasFlag PulseBitMasks.constantVolumeFlag v
        isLoopAndLengthCounterHalt = hasFlag PulseBitMasks.lengthCounterHaltFlag v
    }

  let parselinearCounterTriangle v = TriangleBitMasks.linearCounterMask &&& v

  let parseControlTriangle v = hasFlag TriangleBitMasks.controlFlag v

  let isLengthHaltTriangle (tri : Triangle) = tri.isLinearCtrlAndLengthCounterHalt

  let hasControlTriangle (tri: Triangle) = tri.isLinearCtrlAndLengthCounterHalt
  
  let parseVolumeControlNoise (noise : Noise) v =
    { noise with
        volume = NoiseBitMasks.volumeMask &&& v
        isConstant = hasFlag NoiseBitMasks.constantVolumeFlag v
        isLoopAndLengthCounterHalt = hasFlag NoiseBitMasks.lengthCounterHaltFlag v
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

  let freqPulseHz timer =
    if timer < 8us then 0.0
    else ApuConstants.cpuClockNTSC / (16.0 * float (timer + 1us))

  let freqTriangleHz timer =
    ApuConstants.cpuClockNTSC / (32.0 * float (timer + 1us))

  let isIrqInhibited v = hasFlag FrameCounterFlags.irqInhibit v

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

  let isMutedPulse (pulse : Pulse) = pulse.lengthCounter = 0uy || pulse.timer < 8us || pulse.timer > 0x7FFus

  /// スウィープ
  /// ミュートされてても分周器は進む
  let tickSweep pulse =
    let sw = pulse.sweep
    let shouldSweep =
      sw.enabled &&
      sw.divider = 0uy &&
      sw.shift <> 0uy &&
      not (isMutedPulse pulse)

    let newTimer =
      if shouldSweep then
        let delta = pulse.timer >>> int sw.shift
        if sw.negate then
          pulse.timer - delta - if pulse.channel = One then 1us else 0us
        else
          pulse.timer + delta
      else
        pulse.timer

    if sw.reload || sw.divider = 0uy then
      sw.divider <- sw.period
      sw.reload <- false
    else
      sw.divider <- sw.divider - 1uy

    // if shouldSweep then
    //   printfn "sweep: ch=%A timer=%A → %A (delta=%A neg=%A)" pulse.channel pulse.timer newTimer (pulse.timer >>> int sw.shift) sw.negate

    { pulse with timer = newTimer }

  let tickLinearCounter tri =
    let counter =
      if tri.linearReloadFlag then
        tri.linearCounterLoad
      elif tri.linearCounter > 0uy then
        tri.linearCounter - 1uy
      else 0uy

    let reloadFlag =
      if hasControlTriangle tri then tri.linearReloadFlag
      else false

    { tri with
        linearCounter = counter
        linearReloadFlag = reloadFlag }

  let tickLengthCounterPulse (ch : Pulse) =
    if not ch.isLoopAndLengthCounterHalt && ch.lengthCounter > 0uy then
      { ch with lengthCounter = ch.lengthCounter - 1uy }
    else
      ch
  
  let tickLengthCounterTriangle (ch : Triangle) =
    if not (isLengthHaltTriangle ch) && ch.lengthCounter > 0uy then
      { ch with lengthCounter = ch.lengthCounter - 1uy }
    else
      ch
  
  let tickLengthCounterNoise (ch : Noise) =
    if not ch.isLoopAndLengthCounterHalt && ch.lengthCounter > 0uy then
      { ch with lengthCounter = ch.lengthCounter - 1uy }
    else
      ch

module Apu =

  type ApuStep =
    | Step1
    | Step2
    | Step3
    | Step4
    | Step5

  type ApuState = {
    pulse1: Registers.Pulse
    pulse2: Registers.Pulse
    triangle: Registers.Triangle
    noise: Registers.Noise
    // TODO: DPCM
    status: byte
    frameCounter: Registers.FrameCounter
    mutable cycle: uint
    mutable step: ApuStep
    irq: bool
  }

  let initial = {
    pulse1 = Registers.initialPulse Registers.One
    pulse2 = Registers.initialPulse Registers.Two
    triangle = Registers.initialTriangle
    noise = Registers.initialNoise
    status = 0uy
    frameCounter = Registers.initialFrameCounter
    cycle = 0u
    step = Step1
    irq = false
  }


  /// TODO: DMC 関連の操作
  let writeToStatus value apu =
    let pulse1EnableCond = Registers.hasFlag Registers.StatusFlags.pulse1Enable value
    let pulse2EnableCond = Registers.hasFlag Registers.StatusFlags.pulse2Enable value
    let triEnableCond = Registers.hasFlag Registers.StatusFlags.triangleEnable value
    let noiseEnableCond = Registers.hasFlag Registers.StatusFlags.noiseEnable value 

    let pulse1Counter = if pulse1EnableCond then apu.pulse1.lengthCounter else 0uy
    let pulse2Counter = if pulse2EnableCond then apu.pulse2.lengthCounter else 0uy
    let triCounter = if triEnableCond then apu.triangle.lengthCounter else 0uy
    let noiCounter = if noiseEnableCond then apu.noise.lengthCounter else 0uy

    let status' =
      value
      |> Registers.clearFlag Registers.StatusFlags.dmcInterrupt

    { apu with
        pulse1.lengthCounter = pulse1Counter
        pulse2.lengthCounter = pulse2Counter
        triangle.lengthCounter = triCounter
        noise.lengthCounter = noiCounter
        status = status'
    }

  let tickEnvelopeAndLinear apu =

    let ch1Ev = Registers.tickEnvelope apu.pulse1.envelope apu.pulse1.volume apu.pulse1.isLoopAndLengthCounterHalt
    let ch2Ev = Registers.tickEnvelope apu.pulse2.envelope apu.pulse2.volume apu.pulse2.isLoopAndLengthCounterHalt
    let ch3 = Registers.tickLinearCounter apu.triangle
    let ch4Ev = Registers.tickEnvelope apu.noise.envelope apu.noise.volume apu.noise.isLoopAndLengthCounterHalt

    { apu with
        pulse1.envelope = ch1Ev
        pulse2.envelope = ch2Ev
        triangle = ch3
        noise.envelope = ch4Ev
    }

  let tickLengthAndSweep apu =
    let ch1 =
      apu.pulse1
      |> Registers.tickLengthCounterPulse
      |> Registers.tickSweep

    let ch2 =
      apu.pulse2
      |> Registers.tickLengthCounterPulse
      |> Registers.tickSweep

    let ch3 = Registers.tickLengthCounterTriangle apu.triangle
    let ch4 = Registers.tickLengthCounterNoise apu.noise

    { apu with
        pulse1 = ch1
        pulse2 = ch2
        triangle = ch3
        noise = ch4
    }

  type FrameCounterMode = FourStep | FiveStep

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
      let irq = not (Registers.isIrqInhibited apu.frameCounter.irqInhibitMode)
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

      let mode = if Registers.hasFlag Registers.FrameCounterFlags.mode apu.status then FiveStep else FourStep
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
    let fc = v
    let mode = if Registers.hasFlag Registers.FrameCounterFlags.mode v then FiveStep else FourStep

    apu.cycle <- 0u
    apu.step <- Step1
    
    let apu =
      if mode = FiveStep then
        apu
        |> tickEnvelopeAndLinear
        |> tickLengthAndSweep
      else
        apu

    { apu with frameCounter.irqInhibitMode = fc }

  let write addr value apu =

    match addr with
    // Ch1: 矩形波
    | 0x4000us ->
      { apu with pulse1 = Registers.parseVolumeControlPulse apu.pulse1 value }
    | 0x4001us ->
      { apu with pulse1.sweep = Registers.parseSweep value }
    | 0x4002us -> // timer lo 部分の上書き
      let timer = Registers.updateTimerLo apu.pulse1.timer value
      { apu with pulse1.timer = timer }
    | 0x4003us -> // timer hi と長さカウンタ
      let timer = Registers.updateTimerHi apu.pulse1.timer value
      let c = Registers.parseLengthCounter value
      { apu with
          pulse1.timer = timer
          pulse1.envelope.reload = true
          pulse1.lengthCounter = c
      }
    // Ch2: 矩形波
    | 0x4004us ->
      { apu with pulse2 = Registers.parseVolumeControlPulse apu.pulse2 value}
    | 0x4005us ->
      { apu with pulse2.sweep = Registers.parseSweep value }
    | 0x4006us ->
      let timer = Registers.updateTimerLo apu.pulse2.timer value
      { apu with pulse2.timer = timer }
    | 0x4007us ->
      let timer = Registers.updateTimerHi apu.pulse2.timer value
      let c = Registers.parseLengthCounter value
      { apu with
          pulse2.timer = timer
          pulse2.envelope.reload = true
          pulse2.lengthCounter = c
      }
    // Ch3: 三角波
    | 0x4008us ->
      { apu with
          triangle.linearCounterLoad = Registers.parselinearCounterTriangle value
          triangle.isLinearCtrlAndLengthCounterHalt = Registers.parseControlTriangle value
          triangle.linearReloadFlag = true
      }
    | 0x400Aus ->
      let timer = Registers.updateTimerLo apu.triangle.timer value
      { apu with triangle.timer = timer }
    | 0x400Bus ->
      let c = Registers.parseLengthCounter value
      let timer = Registers.updateTimerHi apu.triangle.timer value
      { apu with
          triangle.timer = timer
          triangle.lengthCounter = c
          triangle.linearReloadFlag = true
      }
    // Ch4: ノイズ
    | 0x400Cus ->
      { apu with noise = Registers.parseVolumeControlNoise apu.noise value }
    | 0x400Eus ->
      let pIdx = Registers.parsePeriodIndexNoise value
      let mode = Registers.parseModeNoise value
      { apu with noise.periodIndex = pIdx; noise.isShortMode = mode }
    | 0x400Fus ->
      let c = Registers.parseLengthCounter value
      { apu with
          noise.envelope.reload = true
          noise.lengthCounter = c
      }
    | 0x4015us ->
      let apu' = writeToStatus value apu
      apu'
    // TODO: DMC (DPCM) 関連の処理

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
      let noiCond = Registers.hasFlag Registers.StatusFlags.noiseEnable apu.status && apu.noise.lengthCounter > 0uy
      let triCond = Registers.hasFlag Registers.StatusFlags.triangleEnable apu.status && apu.triangle.lengthCounter > 0uy
      let p2Cond = Registers.hasFlag Registers.StatusFlags.pulse2Enable apu.status && apu.pulse2.lengthCounter > 0uy
      let p1Cond = Registers.hasFlag Registers.StatusFlags.pulse1Enable apu.status && apu.pulse1.lengthCounter > 0uy
      let status' =
        apu.status
        |> Registers.clearFlag Registers.StatusFlags.frameInterrupt
        // TODO: DMC
        // |> Registers.updateFlag Registers.StatusFlags.deltaModulationActive (dmcBytes > 0) 
        |> Registers.updateFlag Registers.StatusFlags.noiseLengthCounterLargerThanZero noiCond
        |> Registers.updateFlag Registers.StatusFlags.triangleLengthCounterLargerThanZero triCond
        |> Registers.updateFlag Registers.StatusFlags.pulse2LengthCounterLargerThanZero p2Cond
        |> Registers.updateFlag Registers.StatusFlags.pulse1LengthCounterLargerThanZero p1Cond
      status', { apu with status = status' }
    | _ ->
      // printfn "This APU register is not implemented yet. %04X" addr
      0uy, apu

  let pulseDutyTable : int[][] = [|
    [| 0;1;0;0;0;0;0;0 |]; // 12.5%
    [| 0;1;1;0;0;0;0;0 |]; // 25%
    [| 0;1;1;1;1;0;0;0 |]; // 50%
    [| 1;0;0;1;1;1;1;1 |]; // 25% negated
  |]

  /// 矩形波出力
  /// TODO: ミュートによる位相のリセットをするかどうかは後で決めたい
  let outputPulse dt (pulse : Registers.Pulse) =
    let freq = Registers.freqPulseHz pulse.timer
    if freq = 0.0 then 0.0f, pulse
    else
      let period = 1.0 / freq
      let dutyIndex = int pulse.duty &&& 0b11

      let isMuted = Registers.isMutedPulse pulse

      let newPhase =
        if isMuted then pulse.phase
        else (pulse.phase + dt) % period

      let step = int (pulse.phase / period * 8.0) % 8
      let bit = pulseDutyTable[dutyIndex][step]

      let sample =
        if bit = 1 then
          float (if pulse.isConstant then pulse.volume else pulse.envelope.volume) / 15.0
        else
          0.0
      float32 sample, { pulse with phase = newPhase }

  let triangleTable : float32[] =
    Array.append [|15.. -1 .. 0|] [|0..15|]
    |> Array.map (fun x -> float32 x / 15.0f * 2.0f - 1.0f) // 正規化

  /// 三角波出力
  let outputTriangle dt (tri : Registers.Triangle ) =
    let freq = Registers.freqTriangleHz tri.timer
    if freq = 0.0 then 0.0f, tri
    else
      let period = 1.0 / float freq

      let isMuted = tri.lengthCounter = 0uy || tri.linearCounter = 0uy

      let newPhase =
        if isMuted then tri.phase // ミュート時は位相を維持する
        else (tri.phase + dt) % period

      let index = int (tri.phase / period * 32.0) % 32
      let sample = triangleTable[index]

      sample, { tri with phase = newPhase }
      

  /// 除数インデックス
  let noisePeriods = 
    [| 4; 8; 16; 32; 64; 96; 128; 160; 202; 254; 380; 508; 762; 1016; 2034; 4068 |]

  /// 周波数テーブル生成
  let generateNoiseFreqTable (cpuClockHz: float) =
    noisePeriods |> Array.map (fun p -> cpuClockHz / float p)

  let noiseFreqs = generateNoiseFreqTable ApuConstants.cpuClockNTSC


  /// ノイズ生成
  /// シフトレジスタをいじって疑似乱数を生む
  let nextNoise shift isShortMode =
    let x = if isShortMode then 6 else 1 // 比較するビットを周期モードによって変える
    let feedback = (shift &&& 1us) ^^^ (shift >>> x &&& 1us)
    let shifted = shift >>> 1
    let newShift = feedback <<< 14 ||| shifted
    newShift

  /// ノイズチャンネルの 1 サンプルを生成する
  /// 以下の場合に出力:
  /// * シフトレジスタの bit 0 がセットされていない
  /// * 長さカウンタが 0 でない
  let outputNoise dt (noi: Registers.Noise) =
    if noi.lengthCounter = 0uy then 0.0f, noi
    else
      let index = noi.periodIndex |> int
      let freq = noiseFreqs[index]
      let period = 1.0 / freq

      let newPhase = (noi.phase + dt) % period
      let newShift =
        if newPhase < noi.phase then
          nextNoise noi.shift noi.isShortMode
        else
          noi.shift

      let bit = noi.shift &&& 1us
      let sample =
        if bit = 0us then
          float (if noi.isConstant then noi.volume else noi.envelope.volume) / 15.0
        else
          0.0
      let noi' = { noi with shift = newShift; phase = newPhase }
      float32 sample, noi'

/// 1 サンプル合成出力（dtベース）
  let mix (dt: float) apu =
    let ch1, pu1 = outputPulse dt apu.pulse1
    let ch2, pu2 = outputPulse dt apu.pulse2
    let ch3, tri = outputTriangle dt apu.triangle
    let ch4, noi = outputNoise dt apu.noise

    let sample = (ch1 + ch2 + ch3 + ch4) * 0.25f
    sample, { apu with
                pulse1 = pu1
                pulse2 = pu2
                triangle = tri
                noise = noi
            }
