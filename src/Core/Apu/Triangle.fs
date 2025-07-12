namespace HamicomEmu.Apu

module Triangle =

    open HamicomEmu.Common
    open HamicomEmu.Apu.Types

    let initial = {
        linearCounterLoad = 0uy
        ctrlAndHalt = false

        timer = 0us

        linearCounter = 0uy
        linearReloadFlag = false

        lengthCounter = 1uy

        phase = 0.0
    }


    let tickLinearCounter tri =

        let counter =
            if tri.linearReloadFlag then tri.linearCounterLoad
            elif tri.linearCounter > 0uy then tri.linearCounter - 1uy
            else 0uy

        let reloadFlag = if tri.ctrlAndHalt then tri.linearReloadFlag else false

        tri.linearCounter <- counter
        tri.linearReloadFlag <- reloadFlag
        tri

    let private freqHz timer =
        Constants.cpuClockNTSC / (32.0 * float (timer + 1us))

    /// 三角波出力
    let output dt (tri: TriangleState) =
        let mutable tri = tri
        let freq = freqHz tri.timer

        if freq = 0.0 then
            0uy, tri
        else
            let period = 1.0 / float freq

            let isMuted = tri.lengthCounter = 0uy || tri.linearCounter = 0uy

            let newPhase =
                if isMuted then
                    tri.phase // ミュート時は位相を維持する
                else
                    (tri.phase + dt) % period

            let index = int (tri.phase / period * 32.0) % 32
            let sample = Constants.triangleTable[index] |> byte

            tri.phase <- newPhase

            sample, tri
