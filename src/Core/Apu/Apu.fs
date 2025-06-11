namespace HamicomEmu.Apu

module ApuConstants =
  let cpuClockNTSC = 1_789_773.0 // NTSC NES の CPU クロック

module Registers =

  module PulseBitMasks =
    // $4000, $4004
    let volumeMask        = 0b0000_1111uy
    let envelopeMask      = 0b0000_1111uy
    let envelopeFlag      = 0b0001_0000uy
    let envelopeLoopFlag  = 0b0010_0000uy
    let dutyCycleMask     = 0b1100_0000uy
    // $4003, $4007
    let timerHiMask       = 0b0000_0111uy
    let lengthCounterMask = 0b1111_1000uy

  module TriangleBitMasks =
    // $4008
    let linerCounterMask  = 0b0111_1111uy
    let linerCounterFlag  = 0b1000_0000uy
    // $400B
    let timerHiMask       = 0b0000_0111uy
    let lengthCounterMask = 0b1111_1000uy

  module NoiseBitMasks =
    // $400C
    let volumeMask        = 0b0000_1111uy
    let envelopeMask      = 0b0000_1111uy
    let envelopeFlag      = 0b0001_0000uy
    let envelopeLoopFlag  = 0b0010_0000uy
    let modeFlag          = 0b1000_0000uy
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
    // mode 0:    mode 1:       function
    // ---------  -----------  -----------------------------
    // - - - f    - - - - -    IRQ (if bit 6 is clear)
    // - l - l    - l - - l    Length counter and sweep
    // e e e e    e e e - e    Envelope and linear counter
    let mode       = 0b1000_0000uy // 0 = 4-step, 1 = 5-step
    let irqInhibit = 0b0100_0000uy

  type Pulse = {
    volumeTone: byte // Volume & duty cycle
    sweep: byte
    // timer は値が低いほど周波数が高くなる
    timerLo: byte
    timerHiLen: byte  // High bits of timer & length counter load

    counter: byte
  }

  let defaultPulse = {
    volumeTone = 0uy
    sweep = 0uy
    timerLo = 0uy
    timerHiLen = 0uy

    counter = 0uy
  }

  type Triangle = {
    linearCounterCtrl: byte
    timerLo: byte
    timerHiLen: byte

    counter: byte
  }

  let defaultTriangle = {
    linearCounterCtrl = 0uy
    timerLo = 0uy
    timerHiLen = 0uy

    counter = 0uy
  }

  type Noise = {
    volume: byte
    periodMode: byte
    length: byte

    counter: byte
    shift: uint16
    phase: int
  }
  let defaultNoise = {
    volume = 0uy
    periodMode = 0uy
    length = 0uy

    counter = 0uy
    shift = 1us
    phase = 0
  }


  let hasFlag flag b = b &&& flag <> 0uy
  let setFlag flag b = b ||| flag
  let clearFlag flag b = b &&& (~~~flag)
  let updateFlag flag condition b =
    if condition then setFlag flag b else clearFlag flag b
  let volumePulse pulse = pulse.volumeTone &&& PulseBitMasks.volumeMask

  let volumeNoise noise = noise.volume &&& NoiseBitMasks.volumeMask

  /// 00: 12.5%, 01: 25%, 10: 50%, 11: 75%
  let duty pulse = pulse.volumeTone &&& PulseBitMasks.dutyCycleMask >>> 6

  let lengthCounterPulse (pulse : Pulse) =
    pulse.timerHiLen &&& PulseBitMasks.lengthCounterMask >>> 3

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

  /// ノイズ生成
  /// シフトレジスタをいじって疑似乱数を生む
  let nextNoise periodMode shift =
    let x = if isShortFreq periodMode then 6 else 1 // 比較するビットを周期モードによって変える
    let feedback = (shift &&& 1us) ^^^ (shift >>> x &&& 1us)
    let shifted = shift >>> 1
    let newShift = feedback <<< 14 ||| shifted
    newShift
  

module Apu =
  type ApuState = {
    pulse1: Registers.Pulse
    pulse2: Registers.Pulse
    triangle: Registers.Triangle
    noise: Registers.Noise
    // TODO: DPCM
    status: byte
  }

  let initial = {
    pulse1 = Registers.defaultPulse
    pulse2 = Registers.defaultPulse
    triangle = Registers.defaultTriangle
    noise = Registers.defaultNoise
    status = 0uy
  }

  /// TODO: DMC 関連の操作
  let writeToStatus value apu =
    let pulse1EnableCond = Registers.hasFlag Registers.StatusFlags.pulse1Enable value
    let pulse2EnableCond = Registers.hasFlag Registers.StatusFlags.pulse2Enable value
    let triEnableCond = Registers.hasFlag Registers.StatusFlags.triangleEnable value
    let noiseEnableCond = Registers.hasFlag Registers.StatusFlags.noiseEnable value 

    let pulse1Counter = if pulse1EnableCond then apu.pulse1.counter else 0uy
    let pulse2Counter = if pulse2EnableCond then apu.pulse2.counter else 0uy
    let triCounter = if triEnableCond then apu.triangle.counter else 0uy
    let noiCounter = if noiseEnableCond then apu.noise.counter else 0uy

    let status' =
      value
      |> Registers.clearFlag Registers.StatusFlags.dmcInterrupt

    { apu with
        pulse1.counter = pulse1Counter
        pulse2.counter = pulse2Counter
        triangle.counter = triCounter
        noise.counter = noiCounter
        status = status'
    }

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
      { apu with pulse1.timerHiLen = value }
    // Ch2: 矩形波
    | 0x4004us ->
      { apu with pulse2.volumeTone = value }
    | 0x4005us ->
      { apu with pulse2.sweep = value }
    | 0x4006us ->
      { apu with pulse2.timerLo = value }
    | 0x4007us ->
      { apu with pulse2.timerHiLen = value }
    // Ch3: 三角波
    | 0x4008us ->
      { apu with triangle.linearCounterCtrl = value }
    | 0x400Aus ->
      { apu with triangle.timerLo = value }
    | 0x400Bus ->
      { apu with triangle.timerHiLen = value }
    // Ch4: ノイズ
    | 0x400Cus ->
      { apu with noise.volume = value }
    | 0x400Eus ->
      { apu with noise.periodMode = value }
    | 0x400Fus ->
      { apu with noise.length = value }
    | 0x4015us ->
      // TODO: DMC 関連の処理
      let apu' = writeToStatus value apu
      apu'
    // TODO: DPCM

    | _ ->
      printfn "This APU register is not implemented yet. %04X" addr
      apu
  
  let read addr apu =
    match addr with
    | 0x4015us -> // TODO: オープンバスの挙動
      let noiCond = Registers.hasFlag Registers.StatusFlags.noiseEnable apu.status && apu.noise.counter > 0uy
      let triCond = Registers.hasFlag Registers.StatusFlags.triangleEnable apu.status && apu.triangle.counter > 0uy
      let p2Cond = Registers.hasFlag Registers.StatusFlags.pulse2Enable apu.status && apu.pulse2.counter > 0uy
      let p1Cond = Registers.hasFlag Registers.StatusFlags.pulse1Enable apu.status && apu.pulse1.counter > 0uy
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

  let dutyTable = [| 0.125; 0.25; 0.5; 0.75 |]

  /// 矩形波出力
  /// TODO: スウィープ、エンベロープ処理、長さカウンタ
  let outputPulse t pulse =
    let timer = Registers.timerPulse pulse
    let freq = Registers.freqPulseHz timer
    let duty = Registers.duty pulse

    if freq = 0.0 then 0.0f
    else
      let period = 1.0 / float freq
      let phase = (t % period) / period
      let v = if phase < dutyTable[int duty] then 1.0 else -1.0
      v * float (Registers.volumePulse pulse) / 15.0 |> float32

  let triangleTable =
    [| 0 .. 31 |]
    |> Array.map ( fun i ->
      let v = if i < 16 then i else 31 - i
      float32 (v - 15) / 15.0f // -1.0 - +1.0 正規化
    )

  /// 三角波出力
  /// TODO: 長さカウンタ、ミュートしても位相はリセットされずに保持されるようにする
  let outputTriangle t tri =
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
  /// * TODO: 長さカウンタが 0 でない
  let outputNoise t (noi: Registers.Noise) =
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
        float (Registers.volumeNoise noi) / 15.0
      else
        0.0
    let noi' = { noi with shift = newShift; phase = newPhase }
    float32 out, noi'


  /// 1 サンプル合成出力
  let mix t apu =
    let ch1 = outputPulse t apu.pulse1
    let ch2 = outputPulse t apu.pulse2
    // 三角波ミュートはまだ実装しておらずうるさいのでとりあえずオミット
    // let ch3 = outputTriangle t apu.triangle
    let ch4, noi = outputNoise t apu.noise

    // ボリューム平均化
    (ch1 + ch2 + (*ch3 +*) ch4) * 0.25f, { apu with noise = noi }
