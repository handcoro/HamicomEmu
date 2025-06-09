namespace HamicomEmu.Apu

module ApuConstants =
  let cpuClockNTSC = 1_789_773.0 // NTSC NES の CPU クロック

module Registers =

  module PulseBitMasks =

    let volumeMask = 0b0000_1111uy
    let envelopeFlag = 0b0001_0000uy
    let envelopeLoopFlag = 0b0010_0000uy
    let dutyCycleMask = 0b1100_0000uy

    let timerHiMask = 0b0000_0111uy
    let lengthCounterMask = 0b1111_1000uy

  module TriangleBitMasks =

    let linerCounterMask = 0b0111_1111uy
    let controlFlag = 0b1000_0000uy
    let timerHiMask = 0b0000_0111uy
    let lengthCounterMask = 0b1111_1000uy

  module NoiseBitMasks =

    let volumeMask = 0b0000_1111uy
    let envelopeFlag = 0b0001_0000uy
    let envelopeLoopFlag = 0b0010_0000uy
    let modeFlag = 0b1000_0000uy
    let periodMask = 0b0000_1111uy
    let lengthCounterMask = 0b1111_1000uy

  type Pulse = {
    volumeTone: byte // Volume & duty cycle
    sweep: byte
    // timer は値が低いほど周波数が高くなる
    timerLo: byte
    timerHiLen: byte  // High bits of timer & length counter load
  }

  let defaultPulse = {
    volumeTone = 0uy
    sweep = 0uy
    timerLo = 0uy
    timerHiLen = 0uy
  }

  type Triangle = {
    linearCounterCtrl: byte
    timerLo: byte
    timerHiLen: byte
  }

  let defaultTriangle = {
    linearCounterCtrl = 0uy
    timerLo = 0uy
    timerHiLen = 0uy
  }

  type Noise = {
    volume: byte
    periodMode: byte
    length: byte
    mutable shift: uint16
    mutable phase: int
  }
  let defaultNoise = {
    volume = 0uy
    periodMode = 0uy
    length = 0uy
    shift = 1us
    phase = 0
  }

  let volumePulse pulse = pulse.volumeTone &&& PulseBitMasks.volumeMask

  let volumeNoise noise = noise.volume &&& NoiseBitMasks.volumeMask

  /// 00: 12.5%, 01: 25%, 10: 50%, 11: 75%
  let duty pulse = pulse.volumeTone &&& PulseBitMasks.dutyCycleMask >>> 6

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
  }

  let initial = {
    pulse1 = Registers.defaultPulse
    pulse2 = Registers.defaultPulse
    triangle = Registers.defaultTriangle
    noise = Registers.defaultNoise
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
      // TODO: ステータスレジスタ
      apu
    // TODO: DPCM

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
