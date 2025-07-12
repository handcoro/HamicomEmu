namespace HamicomEmu.Apu

module SweepUnit =

    open HamicomEmu.Apu.Types

    let initial = {
        enabled = false
        negate = false
        period = 0uy
        shift = 0uy
        reload = false
        divider = 0uy
    }

    /// スウィープ
    /// ミュートされてても分周器は進む
    let tick pulse =
        let sw = pulse.sweep

        let shouldSweep =
            sw.enabled && sw.divider = 0uy && sw.shift <> 0uy && not (Common.isMuted pulse)

        let newTimer =
            if shouldSweep then
                let delta = pulse.timer >>> int sw.shift

                if sw.negate then
                    pulse.timer - delta - (if pulse.channel = One then 1us else 0us)
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

        pulse.timer <- newTimer

        pulse
