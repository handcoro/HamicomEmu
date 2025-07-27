namespace HamicomEmu.Apu

module Pulse =

    open HamicomEmu.Common
    open HamicomEmu.Apu.Types
    open HamicomEmu.Audio.BlipBuffer
    open HamicomEmu.Apu.Constants

    let init ch = {
        channel = ch
        volume = 0uy
        duty = 0uy
        loopAndHalt = false
        isConstant = false

        sweep = SweepUnit.init
        timer = 0us
        targetTimer = 0us

        envelope = Envelope.init

        lengthCounter = 1uy

        dutyStep = 0
        blip = BlipBuffer(sampleRate, bufferSize, numPhase, kernelLength)
        blipOutputs = Array.zeroCreate bufferSize

        lastOutput = 0.
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

    let getDelta (pulse: PulseState) =
        let newValue = output pulse |> float
        let delta = newValue - pulse.lastOutput
        delta

    let updateLastOutput (pulse: PulseState) delta =
        pulse.lastOutput <- pulse.lastOutput + delta
