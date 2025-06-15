namespace HamicomEmu.Apu

module Triangle =

  open HamicomEmu.Apu.Types

  let hasControlFlag (tri: TriangleChannel) = tri.ctrlAndHalt
  let hasLengthHaltFlag (tri : TriangleChannel) = tri.ctrlAndHalt  

  let tickLinearCounter tri =
    let counter =
      if tri.linearReloadFlag then
        tri.linearCounterLoad
      elif tri.linearCounter > 0uy then
        tri.linearCounter - 1uy
      else 0uy

    let reloadFlag =
      if hasControlFlag tri then tri.linearReloadFlag
      else false

    { tri with
        linearCounter = counter
        linearReloadFlag = reloadFlag }

  let tickLengthCounter (tri : TriangleChannel) =
    if not (hasLengthHaltFlag tri) && tri.lengthCounter > 0uy then
      { tri with lengthCounter = tri.lengthCounter - 1uy }
    else
      tri

  let triangleTable : float32[] =
    Array.append [|15.. -1 .. 0|] [|0..15|]
    |> Array.map (fun x -> float32 x / 15.0f * 2.0f - 1.0f) // 正規化

  let freqHz timer =
    Constants.cpuClockNTSC / (32.0 * float (timer + 1us))

  /// 三角波出力
  let output dt (tri : TriangleChannel ) =
    let freq = freqHz tri.timer
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