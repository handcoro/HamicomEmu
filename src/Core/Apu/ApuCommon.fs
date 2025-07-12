namespace HamicomEmu.Apu

module Common =

    open HamicomEmu.Apu.Types

    let isMuted (pul: PulseState) =
        pul.lengthCounter = 0uy || pul.timer < 8us || pul.timer > 0x7FFus
