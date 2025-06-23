namespace HamicomEmu.Apu

module LengthCounter =

  open HamicomEmu.Apu.Types

  let private tick
    (getHalt: 'a -> bool)
    (getLength: 'a -> byte)
    (setLength: 'a -> byte -> 'a)
    (state: 'a) : 'a =
    if not (getHalt state) && getLength state > 0uy then
      setLength state (getLength state - 1uy)
    else
      state

  let tickPulse (p: PulseState) : PulseState =
    if not p.loopAndHalt && p.lengthCounter > 0uy then
      p.lengthCounter <- p.lengthCounter - 1uy
    p

  let tickTriangle (tri: TriangleState) : TriangleState =
    if not tri.ctrlAndHalt && tri.lengthCounter > 0uy then
      tri.lengthCounter <- tri.lengthCounter - 1uy
    tri

  let tickNoise (noi : NoiseState) : NoiseState =
    if not noi.loopAndHalt && noi.lengthCounter > 0uy then
      noi.lengthCounter <- noi.lengthCounter - 1uy
    noi
