namespace HamicomEmu.Apu

module Envelope =

  open HamicomEmu.Apu.Types

  let initial = {
    volume = 0uy
    divider = 0uy
    decay = 0uy
    reload = false
  }

  /// エンベロープを進める
  let tick (ev : EnvelopeState) reload loop =
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
