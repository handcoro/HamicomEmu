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

    /// 1フレーム分のオーバーサンプリング→平均化された音声を生成 (最適化版)
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

        let samples = Array.zeroCreate<float32> samplesPerFrame
        let mutable cycleAcc = processor.cycleAcc
        let mutable currentEmu = emu
        let mutable sampleIdx = 0

        // CPU サイクル処理：1サブサンプルぶん実行
        let inline processCycles() =
            while cycleAcc >= 1.0 do
                let emu', used = EmulatorCore.tick currentEmu traceFn
                currentEmu <- emu'
                cycleAcc <- cycleAcc - float used

        // 1サンプル分を生成（オーバーサンプリング適用）
        let inline generateSample() =
            let mutable sum = 0.0f
            for _ in 1..processor.oversampleFactor do
                cycleAcc <- cycleAcc + cyclesPerSubSample
                processCycles()
                sum <- sum + Apu.mix currentEmu.bus.apu // 蓄積
            sum / float32 processor.oversampleFactor  // ここで平均化

        // フレーム中の全サンプルを生成 (Array直接代入で最適化)
        if samplesPerFrame > 0 then
            for i in 0..samplesPerFrame - 1 do
                samples[i] <- generateSample()

        let updatedProcessor = { processor with cycleAcc = cycleAcc }
        updatedProcessor, currentEmu, samples

