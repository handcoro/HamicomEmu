namespace HamicomEmu.Platform.Audio

open HamicomEmu
open HamicomEmu.Common
open HamicomEmu.Apu
open HamicomEmu.EmulatorCore

module Processor =
    
    /// 音声処理プロセッサの状態を管理する型
    type AudioProcessor = {
        sampleRate: int
        oversampleFactor: int
        maxSamplesPerUpdate: int
        cycleAcc: float
    }

    /// 音声プロセッサを初期化
    let create (sampleRate: int) (oversampleFactor: int) : AudioProcessor =
        {
            sampleRate = sampleRate
            oversampleFactor = oversampleFactor
            maxSamplesPerUpdate = sampleRate / 30
            cycleAcc = 0.0
        }

    /// 1フレーム分のオーバーサンプリング→平均化された音声を生成
    /// 戻り値: (更新済み processor, 新 emu, サンプル配列)
    let generateFrame
        (processor: AudioProcessor)
        (emu: EmulatorCore.EmulatorState)
        (traceFn: EmulatorCore.EmulatorState -> unit)
        (desiredSamples: int)
        : AudioProcessor * EmulatorCore.EmulatorState * float32[] =
        
        let samplesPerFrame =
            if desiredSamples > processor.maxSamplesPerUpdate then
                min desiredSamples processor.maxSamplesPerUpdate
            else
                desiredSamples

        let cpuClock = Constants.cpuClockNTSC
        let cyclesPerSubSample = cpuClock / float (processor.sampleRate * processor.oversampleFactor)
        let samples = ResizeArray<float32>()
        let mutable cycleAcc = processor.cycleAcc
        let mutable currentEmu = emu

        // CPU サイクル処理：1サブサンプルぶん実行
        let processCycles() =
            while cycleAcc >= 1.0 do
                let emu', used = EmulatorCore.tick currentEmu traceFn
                currentEmu <- emu'
                cycleAcc <- cycleAcc - float used

        // 1サンプル分を生成（オーバーサンプリング適用）
        let generateSample() =
            let mutable sum = 0.0f
            for _ in 1..processor.oversampleFactor do
                cycleAcc <- cycleAcc + cyclesPerSubSample
                processCycles()
                sum <- sum + Apu.mix currentEmu.bus.apu // 蓄積
            sum / float32 processor.oversampleFactor // ここで平均化

        // フレーム中の全サンプルを生成
        if samplesPerFrame > 0 then
            for _ in 1..samplesPerFrame do
                samples.Add(generateSample())

        let updatedProcessor = { processor with cycleAcc = cycleAcc }
        updatedProcessor, currentEmu, samples |> Seq.toArray

