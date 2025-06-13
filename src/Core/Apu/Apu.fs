namespace HamicomEmu.Apu

module ApuConstants =
  let cpuClockNTSC = 1_789_773.0 // NTSC NES の CPU クロック

module Registers =

  /// 矩形波とノイズ共通
  module GeneralMasks =
    let volumeMask   = 0b0000_1111uy
    let envelopeMask = 0b0000_1111uy
    let constantVolumeFlag    = 0b0001_0000uy
    let envelopeLoopFlag      = 0b0010_0000uy
    let lengthCounterHaltFlag = 0b0010_0000uy
    let lengthCounterMask = 0b1111_1000uy

  module PulseBitMasks =
    // $4000, $4004
    let volumeMask            = 0b0000_1111uy
    let envelopeMask          = 0b0000_1111uy
    let constantVolumeFlag    = 0b0001_0000uy
    let envelopeLoopFlag      = 0b0010_0000uy
    let lengthCounterHaltFlag = 0b0010_0000uy
    let dutyCycleMask         = 0b1100_0000uy
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

  type Envelope = {
    envVolume: byte
    mutable envDivider: byte
    mutable envDecay: byte
    mutable envStart: bool
  }

  let initEnvelope = {
    envVolume = 0uy
    envDivider = 0uy
    envDecay = 0uy
    envStart = false
  }

  type Pulse = {
    volumeTone: byte // Volume & duty cycle
    sweep: byte
    // timer は値が低いほど周波数が高くなる
    timerLo: byte
    timerHiLen: byte  // High bits of timer & length counter load

    envelope: Envelope

    // 長さカウンタ内部状態
    lengthCounter: byte
  }

  let defaultPulse = {
    volumeTone = 0uy
    sweep = 0uy
    timerLo = 0uy
    timerHiLen = 0uy

    envelope = initEnvelope

    lengthCounter = 1uy
  }

  type Triangle = {
    linearCounterCtrl: byte
    timerLo: byte
    timerHiLen: byte

    linearCounter: byte
    linearReloadFlag: bool

    lengthCounter: byte
  }

  let defaultTriangle = {
    linearCounterCtrl = 0uy
    timerLo = 0uy
    timerHiLen = 0uy

    linearCounter = 0uy
    linearReloadFlag = false

    lengthCounter = 1uy
  }

  type Noise = {
    volume: byte
    periodMode: byte
    length: byte

    envelope: Envelope

    lengthCounter: byte
    shift: uint16
    phase: int
  }
  let defaultNoise = {
    volume = 0uy
    periodMode = 0uy
    length = 0uy

    envelope = initEnvelope

    lengthCounter = 1uy
    shift = 1us
    phase = 0
  }

  type frameCounter = {
    irqInhibitMode: byte
  }

  let defaultframeCounter = {
    irqInhibitMode = 0uy;
  }

  let hasFlag flag b = b &&& flag <> 0uy
  let setFlag flag b = b ||| flag
  let clearFlag flag b = b &&& (~~~flag)
  let updateFlag flag condition b =
    if condition then setFlag flag b else clearFlag flag b

  let reloadEnvelope v = v &&& GeneralMasks.envelopeMask

  let hasLoopFlag v = hasFlag GeneralMasks.envelopeLoopFlag v
  let volume ev v =
    let constant = hasFlag GeneralMasks.constantVolumeFlag v
    if constant then
      v &&& GeneralMasks.volumeMask
    else
      ev.envVolume

  let volumeNoise noise = noise.volume &&& NoiseBitMasks.volumeMask

  /// 00: 12.5%, 01: 25%, 10: 50%, 11: 75%
  let duty pulse = pulse.volumeTone &&& PulseBitMasks.dutyCycleMask >>> 6

  let lengthTable = [|
    10uy; 254uy; 20uy;  2uy; 40uy;  4uy; 80uy;  6uy;
    160uy; 8uy; 60uy; 10uy; 14uy; 12uy; 26uy; 14uy;
    12uy; 16uy; 24uy; 18uy; 48uy; 20uy; 96uy; 22uy;
    192uy; 24uy; 72uy; 26uy; 16uy; 28uy; 32uy; 30uy
  |]

  let lengthCounter v = lengthTable[int v >>> 3]
 
  let isLengthHaltPulse (pulse : Pulse) = hasFlag PulseBitMasks.lengthCounterHaltFlag pulse.volumeTone

  let isLengthHaltTriangle (tri : Triangle) = hasFlag TriangleBitMasks.lengthCounterHaltFlag tri.linearCounterCtrl

  let isLengthHaltNoise (noi : Noise) = hasFlag NoiseBitMasks.lengthCounterHaltFlag noi.volume

  let reloadLinearCounter (tri : Triangle) = tri.linearCounterCtrl &&& TriangleBitMasks.linearCounterMask

  let hasControl (tri: Triangle) = hasFlag TriangleBitMasks.controlFlag tri.linearCounterCtrl

  let timerPulse (pulse: Pulse) =
    let lo = pulse.timerLo |> uint16
    let hi = pulse.timerHiLen &&& PulseBitMasks.timerHiMask |> uint16 <<< 8
    hi ||| lo

  let timerTriangle (tri: Triangle) =
    let lo = tri.timerLo |> uint16
    let hi = tri.timerHiLen &&& TriangleBitMasks.timerHiMask |> uint16 <<< 8
    hi ||| lo

  let freqPulseHz timer =
    if timer < 8us then 0.0
    else ApuConstants.cpuClockNTSC / (16.0 * float (timer + 1us))

  let freqTriangleHz timer =
    ApuConstants.cpuClockNTSC / (32.0 * float (timer + 1us))

  let isShortFreq v = v &&& NoiseBitMasks.modeFlag <> 0uy

  let isIrqInhibited v = hasFlag FrameCounterFlags.irqInhibit v

  /// ノイズ生成
  /// シフトレジスタをいじって疑似乱数を生む
  let nextNoise periodMode shift =
    let x = if isShortFreq periodMode then 6 else 1 // 比較するビットを周期モードによって変える
    let feedback = (shift &&& 1us) ^^^ (shift >>> x &&& 1us)
    let shifted = shift >>> 1
    let newShift = feedback <<< 14 ||| shifted
    newShift
  

  /// エンベロープを進める
  let tickEnvelope ev reload loop =
    let vol =
      if ev.envStart then
        ev.envDivider <- reload
        ev.envDecay <- 15uy
        ev.envStart <- false
        15uy
      else if ev.envDivider = 0uy then // 分周器の励起
        ev.envDivider <- reload
        if ev.envDecay = 0uy then
          if loop then 15uy else 0uy
        else
          ev.envDecay <- ev.envDecay - 1uy
          ev.envDecay
      else
        ev.envDivider <- ev.envDivider - 1uy
        ev.envDecay

    { ev with envVolume = vol }

  let tickLinearCounter tri =
    let counter =
      if tri.linearReloadFlag then
        reloadLinearCounter tri
      elif tri.linearCounter > 0uy then
        tri.linearCounter - 1uy
      else 0uy

    let reloadFlag =
      if hasControl tri then tri.linearReloadFlag
      else false

    { tri with
        linearCounter = counter
        linearReloadFlag = reloadFlag }

  let tickLengthCounterPulse (ch : Pulse) =
    if not (isLengthHaltPulse ch) && ch.lengthCounter > 0uy then
      { ch with lengthCounter = ch.lengthCounter - 1uy }
    else
      ch
  
  let tickLengthCounterTriangle (ch : Triangle) =
    if not (isLengthHaltTriangle ch) && ch.lengthCounter > 0uy then
      { ch with lengthCounter = ch.lengthCounter - 1uy }
    else
      ch
  
  let tickLengthCounterNoise (ch : Noise) =
    if not (isLengthHaltNoise ch) && ch.lengthCounter > 0uy then
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
    frameCounter: Registers.frameCounter
    mutable cycle: uint
    mutable step: ApuStep
    irq: bool
  }

  let initial = {
    pulse1 = Registers.defaultPulse
    pulse2 = Registers.defaultPulse
    triangle = Registers.defaultTriangle
    noise = Registers.defaultNoise
    status = 0uy
    frameCounter = Registers.defaultframeCounter
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
    let ch1Rld = Registers.reloadEnvelope apu.pulse1.volumeTone
    let ch1Loop = Registers.hasLoopFlag apu.pulse1.volumeTone
    let ch2Rld = Registers.reloadEnvelope apu.pulse2.volumeTone
    let ch2Loop = Registers.hasLoopFlag apu.pulse2.volumeTone

    let ch4Rld = Registers.reloadEnvelope apu.noise.volume
    let ch4Loop = Registers.hasLoopFlag apu.noise.volume

    let ch1Ev = Registers.tickEnvelope apu.pulse1.envelope ch1Rld ch1Loop
    let ch2Ev = Registers.tickEnvelope apu.pulse2.envelope ch2Rld ch2Loop
    let ch3 = Registers.tickLinearCounter apu.triangle
    let ch4Ev = Registers.tickEnvelope apu.noise.envelope ch4Rld ch4Loop

    { apu with
        pulse1.envelope = ch1Ev
        pulse2.envelope = ch2Ev
        triangle = ch3
        noise.envelope = ch4Ev
    }

  let tickLengthAndSweep apu =
    let ch1 = Registers.tickLengthCounterPulse apu.pulse1
    let ch2 = Registers.tickLengthCounterPulse apu.pulse2
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

  let frameStepCycles = 7457u // これで割ることで 240Hz を表現する

  /// APU のサイクルを進める
  /// 4-step モードは 240Hz で 1 step 進む
  let tick n apu =
    apu.cycle <- apu.cycle + n

    if apu.cycle < frameStepCycles then
      apu
    else
      apu.cycle <- apu.cycle - frameStepCycles

      let mode = if Registers.hasFlag Registers.FrameCounterFlags.mode apu.status then FiveStep else FourStep
      let apu' = runFrameStep apu.step mode apu

      match apu.step, mode with
      | Step4, FourStep | Step5, FiveStep -> apu'.step <- Step1
      | _ -> ()

      apu.step <- nextStep apu.step

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
      { apu with pulse1.volumeTone = value }
    | 0x4001us ->
      { apu with pulse1.sweep = value }
    | 0x4002us ->
      { apu with pulse1.timerLo = value }
    | 0x4003us ->
      let c = Registers.lengthCounter value
      { apu with pulse1.timerHiLen = value; pulse1.envelope.envStart = true; pulse1.lengthCounter = c }
    // Ch2: 矩形波
    | 0x4004us ->
      { apu with pulse2.volumeTone = value}
    | 0x4005us ->
      { apu with pulse2.sweep = value }
    | 0x4006us ->
      { apu with pulse2.timerLo = value }
    | 0x4007us ->
      let c = Registers.lengthCounter value
      { apu with pulse2.timerHiLen = value; pulse2.envelope.envStart = true; pulse2.lengthCounter = c }
    // Ch3: 三角波
    | 0x4008us ->
      { apu with triangle.linearCounterCtrl = value; triangle.linearReloadFlag = true }
    | 0x400Aus ->
      { apu with triangle.timerLo = value }
    | 0x400Bus ->
      let c = Registers.lengthCounter value
      { apu with triangle.timerHiLen = value; triangle.lengthCounter = c }
    // Ch4: ノイズ
    | 0x400Cus ->
      { apu with noise.volume = value }
    | 0x400Eus ->
      { apu with noise.periodMode = value }
    | 0x400Fus ->
      let c = Registers.lengthCounter value
      { apu with noise.length = value; noise.envelope.envStart = true; noise.lengthCounter = c }
    | 0x4015us ->
      // TODO: DMC 関連の処理
      let apu' = writeToStatus value apu
      apu'
    // TODO: DPCM

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
      { apu with status = status' }
    | _ ->
      printfn "This APU register is not implemented yet. %04X" addr
      apu

  // TODO: 本当の出力は以下の通りなのでこのテーブルを参照したい
  // 0 1 0 0 0 0 0 0 （12.5%）
  // 0 1 1 0 0 0 0 0 （25%）
  // 0 1 1 1 1 0 0 0 （50%）
  // 1 0 0 1 1 1 1 1 （25% 反転） 
  let dutyTable = [| 0.125; 0.25; 0.5; 0.75 |]

  /// 矩形波出力
  /// TODO: スウィープ、ミュートによる位相のリセット
  let outputPulse t (pulse : Registers.Pulse) =
    if pulse.lengthCounter = 0uy then 0.0f
    else
      let timer = Registers.timerPulse pulse
      let freq = Registers.freqPulseHz timer
      let duty = Registers.duty pulse

      if freq = 0.0 then 0.0f
      else
        let period = 1.0 / float freq
        let phase = (t % period) / period
        let v = if phase < dutyTable[int duty] then 1.0 else -1.0
        v * float (Registers.volume pulse.envelope pulse.volumeTone) / 15.0 |> float32

  let triangleTable =
    [| 0 .. 31 |]
    |> Array.map ( fun i ->
      let v = if i < 16 then i else 31 - i
      float32 (v - 15) / 15.0f // -1.0 - +1.0 正規化
    )

  /// 三角波出力
  /// TODO: ミュートしてもゼロの出力にはしない、位相はリセットされない
  let outputTriangle t (tri : Registers.Triangle ) =
    if tri.lengthCounter = 0uy || tri.linearCounter = 0uy then 0.0f 
    else
      let timer = Registers.timerTriangle tri
      let freq = Registers.freqTriangleHz timer
      if freq = 0.0 then 0.0f
      else
        let period = 1.0 / float freq
        let index = int ((t % period) / period * 32.0) % 32
        triangleTable[index]

  /// 除数インデックス
  let noisePeriods = 
    [| 4; 8; 16; 32; 64; 96; 128; 160; 202; 254; 380; 508; 762; 1016; 2034; 4068 |]

  /// 周波数テーブル生成
  let generateNoiseFreqTable (cpuClockHz: float) =
    noisePeriods |> Array.map (fun p -> cpuClockHz / float p)

  let noiseFreqs = generateNoiseFreqTable ApuConstants.cpuClockNTSC

  /// ノイズチャンネルの 1 サンプルを生成する
  /// 以下の場合に出力:
  /// * シフトレジスタの bit 0 がセットされていない
  /// * 長さカウンタが 0 でない
  let outputNoise t (noi: Registers.Noise) =
    if noi.lengthCounter = 0uy then 0.0f, noi
    else
      let index = noi.periodMode &&& Registers.NoiseBitMasks.periodMask |> int
      let freq = noiseFreqs[index]
      let period = 1.0 / freq
      let phase = int (t / period)

      let newShift, newPhase =
        if phase <> noi.phase then
          let shift = Registers.nextNoise noi.periodMode noi.shift
          shift, phase
        else
          noi.shift, noi.phase

      let bit = noi.shift &&& 1us
      let out =
        if bit = 0us then
          float (Registers.volume noi.envelope noi.volume) / 15.0
        else
          0.0
      let noi' = { noi with shift = newShift; phase = newPhase }
      float32 out, noi'


  /// 1 サンプル合成出力
  let mix t apu =
    let ch1 = outputPulse t apu.pulse1
    let ch2 = outputPulse t apu.pulse2
    let ch3 = outputTriangle t apu.triangle
    let ch4, noi = outputNoise t apu.noise

    // ボリューム平均化
    (ch1 + ch2 + ch3 + ch4) * 0.25f, { apu with noise = noi }
