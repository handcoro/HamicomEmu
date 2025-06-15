namespace HamicomEmu.Apu

module Pulse =

  open HamicomEmu.Apu.Types

  let isMuted (pul : PulseChannel) =
    pul.lengthCounter = 0uy || pul.timer < 8us || pul.timer > 0x7FFus

  /// スウィープ
  /// ミュートされてても分周器は進む
  let tickSweep pulse =
    let sw = pulse.sweep
    let shouldSweep =
      sw.enabled &&
      sw.divider = 0uy &&
      sw.shift <> 0uy &&
      not (isMuted pulse)

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

  let tickLengthCounter (p: PulseChannel) =
    if not p.loopAndHalt && p.lengthCounter > 0uy then
      { p with lengthCounter = p.lengthCounter - 1uy }
    else
      p

  let dutyTable : int[][] = [|
    [| 0;1;0;0;0;0;0;0 |]; // 12.5%
    [| 0;1;1;0;0;0;0;0 |]; // 25%
    [| 0;1;1;1;1;0;0;0 |]; // 50%
    [| 1;0;0;1;1;1;1;1 |]; // 25% negated
  |]

  let freqPulseHz timer =
    if timer < 8us then 0.0
    else Constants.cpuClockNTSC / (16.0 * float (timer + 1us))

  /// 矩形波出力
  /// TODO: ミュートによる位相のリセットをするかどうかは後で決めたい
  let output dt (pulse : PulseChannel) =
    let freq = freqPulseHz pulse.timer
    if freq = 0.0 then 0.0f, pulse
    else
      let period = 1.0 / freq
      let dutyIndex = int pulse.duty &&& 0b11

      let isMuted = isMuted pulse

      let newPhase =
        if isMuted then pulse.phase
        else (pulse.phase + dt) % period

      let step = int (pulse.phase / period * 8.0) % 8
      let bit = dutyTable[dutyIndex][step]

      let sample =
        if bit = 1 then
          float (if pulse.isConstant then pulse.volume else pulse.envelope.volume) / 15.0
        else
          0.0
      float32 sample, { pulse with phase = newPhase }
