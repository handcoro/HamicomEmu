namespace HamicomEmu.Apu

module Triangle =

    open HamicomEmu.Common
    open HamicomEmu.Apu.Types

    let initial = {
        linearCounterLoad = 0uy
        ctrlAndHalt = false

        timer = 0us
        timerReloadValue = 0us

        linearCounter = 0uy
        linearReloadFlag = false

        lengthCounter = 1uy

        triangleStep = 0
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

    let tick (tri: TriangleState) =
        let isMuted = tri.lengthCounter = 0uy || tri.linearCounter = 0uy

        if isMuted then
            ()
        elif tri.timer = 0us then
            let nextStep = (tri.triangleStep + 1) % 32
            tri.timer <- tri.timerReloadValue
            tri.triangleStep <- nextStep
        else
            tri.timer <- tri.timer - 1us

        tri

    /// 三角波出力
    let output (tri: TriangleState) =
        let sample = Constants.triangleTable[tri.triangleStep] |> byte
        sample
