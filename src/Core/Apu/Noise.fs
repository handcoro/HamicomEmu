namespace HamicomEmu.Apu

module Noise =

  open HamicomEmu.Apu.Types


  let tickLengthCounter (noi : NoiseState) =
    if not noi.loopAndHalt && noi.lengthCounter > 0uy then
      { noi with lengthCounter = noi.lengthCounter - 1uy }
    else
      noi

  /// 除数インデックス
  let noisePeriods = 
    [| 4; 8; 16; 32; 64; 96; 128; 160; 202; 254; 380; 508; 762; 1016; 2034; 4068 |]

  /// 周波数テーブル生成
  let generateNoiseFreqTable (cpuClockHz: float) =
    noisePeriods |> Array.map (fun p -> cpuClockHz / float p)

  let noiseFreqs = generateNoiseFreqTable Constants.cpuClockNTSC

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
  let output dt (noi: NoiseState) =
    if noi.lengthCounter = 0uy then 0uy, noi
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
          if noi.isConstant then noi.volume else noi.envelope.volume
        else 0uy
      let noi' = { noi with shift = newShift; phase = newPhase }
      sample, noi'