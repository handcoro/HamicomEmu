namespace HamicomEmu.Platform.Audio

open HamicomEmu
open HamicomEmu.Common
open HamicomEmu.Apu
open HamicomEmu.EmulatorCore

module Processor =
    
    /// 音声処理プロセッサの状態を管理する型
    type AudioProcessor = {
        sampleRate: int
        oversamplingRatio: int
        maxSamplesPerUpdate: int
        cycleAcc: float
    }

    /// 音声プロセッサを初期化
    let create (sampleRate: int) (oversamplingRatio: int) : AudioProcessor =
        {
            sampleRate = sampleRate
            oversamplingRatio = oversamplingRatio
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
        let oversamplingRatio = processor.oversamplingRatio
        let cyclesPerSubSample = cpuClock / float (processor.sampleRate * oversamplingRatio)
        let invOversamplingRatio = 1.0f / float32 oversamplingRatio

        let samples = Array.zeroCreate<float32> samplesPerFrame
        let mutable cycleAcc = processor.cycleAcc
        let mutable currentEmu = emu

        // CPU サイクル処理：1サブサンプルぶん実行
        let inline processCycles() =
            while cycleAcc >= 1.0 do
                let emu', used = EmulatorCore.tick currentEmu traceFn
                currentEmu <- emu'
                cycleAcc <- cycleAcc - float used

        // 1サンプル分を生成（オーバーサンプリング適用）
        let inline generateSample() =
            let mutable sum = 0.0f
            for _ in 1..oversamplingRatio do
                cycleAcc <- cycleAcc + cyclesPerSubSample
                processCycles()
                sum <- sum + Apu.mix currentEmu.bus.apu // 蓄積
            sum * invOversamplingRatio // 高速化のため除算の代わりに乗算で平均化

        // フレーム中の全サンプルを生成 (Array直接代入で最適化)
        // 内側ループを 8 ずつ回して外側ループのオーバーヘッドを減らす
        if samplesPerFrame > 0 then
            let batchSize = 8
            let mutable i = 0
            while i < samplesPerFrame do
                let remaining = samplesPerFrame - i
                let count = if remaining >= batchSize then batchSize else remaining
                for j in 0..count - 1 do
                    samples[i + j] <- generateSample()
                i <- i + count

        let updatedProcessor = { processor with cycleAcc = cycleAcc }
        updatedProcessor, currentEmu, samples

