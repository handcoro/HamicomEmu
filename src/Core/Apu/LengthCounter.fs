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
    tick
      (fun x -> x.loopAndHalt)
      (fun x -> x.lengthCounter)
      (fun s v -> { s with lengthCounter = v })
      p

  let tickTriangle (tri: TriangleState) : TriangleState =
    tick
      (fun x -> x.ctrlAndHalt)
      (fun x -> x.lengthCounter)
      (fun s v -> { s with lengthCounter = v })
      tri

  let tickNoise (noi : NoiseState) : NoiseState =
    tick
      (fun x -> x.loopAndHalt)
      (fun x -> x.lengthCounter)
      (fun s v -> { s with lengthCounter = v })
      noi
