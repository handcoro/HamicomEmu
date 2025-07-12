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

        envelope = Envelope.initial

        lengthCounter = 1uy

        phase = 0.0
    }

    let private freqHz timer =
        if timer < 8us then
            0.0
        else
            Constants.cpuClockNTSC / (16.0 * float (timer + 1us))

    /// 矩形波出力
    /// TODO: ミュートによる位相のリセットをするかどうかは後で決めたい
    let output dt (pulse: PulseState) =
        let freq = freqHz pulse.timer

        if freq = 0.0 then
            0uy, pulse
        else
            let period = 1.0 / freq
            let dutyIndex = int pulse.duty &&& 0b11

            let isMuted = Common.isMuted pulse

            let newPhase = if isMuted then pulse.phase else (pulse.phase + dt) % period

            let step = int (pulse.phase / period * 8.0) % 8
            let bit = Constants.dutyTable[dutyIndex][step]

            let sample =
                if bit = 1 then
                    if pulse.isConstant then
                        pulse.volume
                    else
                        pulse.envelope.volume
                else
                    0uy

            pulse.phase <- newPhase

            sample, pulse
