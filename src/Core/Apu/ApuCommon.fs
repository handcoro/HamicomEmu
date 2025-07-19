namespace HamicomEmu.Apu

module Common =

    open HamicomEmu.Apu.Types

    let isMuted (pul: PulseState) =
        pul.timer < 8us || pul.targetTimer > 0x7FFus
