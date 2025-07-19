namespace HamicomEmu.Apu

module Pulse =

    open HamicomEmu.Common
    open HamicomEmu.Apu.Types

    let initial ch = {
        channel = ch
        volume = 0uy
        duty = 0uy
        loopAndHalt = false
        isConstant = false

        sweep = SweepUnit.initial
        timer = 0us
        targetTimer = 0us

        envelope = Envelope.initial

        lengthCounter = 1uy

        dutyStep = 0
    }

    let tick (pulse: PulseState) =
        if pulse.timer = 0us then
            let nextStep = (pulse.dutyStep + 1) % 8
            pulse.timer <- pulse.targetTimer
            pulse.dutyStep <- nextStep
        else
            pulse.timer <- pulse.timer - 1us

        pulse

    /// 矩形波出力
    let output (pulse: PulseState) =
        let dutyIndex = int pulse.duty &&& 0b11

        let isMuted = Common.isMuted pulse || pulse.lengthCounter = 0uy

        let bit = Constants.dutyTable[dutyIndex][pulse.dutyStep]

        let sample =
            if isMuted then
                0uy
            elif bit = 1 then
                if pulse.isConstant then
                    pulse.volume
                else
                    pulse.envelope.volume
            else
                0uy

        sample
