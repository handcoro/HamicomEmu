module AudioProcessorBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkHelpers
open HamicomEmu.EmulatorCore.EmulatorCore
open HamicomEmu.Platform.Audio

/// オーディオプロセッサの詳細ベンチマーク
[<MemoryDiagnoser>]
type AudioProcessorDetailBenchmark() =
    let mutable processor = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
        processor <- Processor.create 44100 1  // 1xオーバーサンプリング
    
    [<IterationSetup>]
    member _.IterationSetup() =
        emu <- powerOn cart
        processor <- Processor.create 44100 1
    
    /// 1サンプル生成（約41 CPUサイクル）(1,000回反復)
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.GenerateSingleSample() =
        for _ in 1..1000 do
            let _, emu', samples = Processor.generateFrame processor emu (fun _ -> ()) 1
            emu <- emu'
            processor <- { processor with cycleAcc = 0.0 }

/// オーバーサンプリング各倍率の詳細比較
[<MemoryDiagnoser>]
type OversamplingDetailBenchmark() =
    let mutable processor1x = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable processor2x = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable processor4x = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable processor16x = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
        processor1x <- Processor.create 44100 1
        processor2x <- Processor.create 44100 2
        processor4x <- Processor.create 44100 4
        processor16x <- Processor.create 44100 16
    
    [<IterationSetup>]
    member _.IterationSetup() =
        emu <- powerOn cart
    
    /// 100サンプル生成（各オーバーサンプリング倍率）
    [<Benchmark(Baseline = true)>]
    member _.Generate100Samples_1x() =
        let _, emu', _ = Processor.generateFrame processor1x emu (fun _ -> ()) 100
        emu <- emu'
    
    [<Benchmark>]
    member _.Generate100Samples_2x() =
        let _, emu', _ = Processor.generateFrame processor2x emu (fun _ -> ()) 100
        emu <- emu'
    
    [<Benchmark>]
    member _.Generate100Samples_4x() =
        let _, emu', _ = Processor.generateFrame processor4x emu (fun _ -> ()) 100
        emu <- emu'
    
    [<Benchmark>]
    member _.Generate100Samples_16x() =
        let _, emu', _ = Processor.generateFrame processor16x emu (fun _ -> ()) 100
        emu <- emu'

/// APU Mixingのみの詳細ベンチマーク
[<MemoryDiagnoser>]
type ApuMixingBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
        
        // APUを動作させるため、いくつかのフレームを実行
        for _ in 1..1000 do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // APU状態はそのまま維持
        ()
    
    /// APU Mix単体の性能（100,000回反復）
    [<Benchmark(OperationsPerInvoke = 100_000)>]
    member _.ApuMixOnly() =
        for _ in 1..100_000 do
            let _ = HamicomEmu.Apu.Apu.mix emu.bus.apu
            ()
    
    /// APU Mix + EmulatorCore tick（1,000回反復）
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.ApuMixWithTick() =
        for _ in 1..1000 do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'
            let _ = HamicomEmu.Apu.Apu.mix emu.bus.apu
            ()

/// サンプルレート別の性能比較
[<MemoryDiagnoser>]
type SampleRateComparisonBenchmark() =
    let mutable processor22k = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable processor44k = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable processor48k = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
        processor22k <- Processor.create 22050 1
        processor44k <- Processor.create 44100 1
        processor48k <- Processor.create 48000 1
    
    [<IterationSetup>]
    member _.IterationSetup() =
        emu <- powerOn cart
    
    /// 1フレーム分（約735サンプル@44.1kHz）の生成
    [<Benchmark>]
    member _.Generate1Frame_22kHz() =
        let _, emu', _ = Processor.generateFrame processor22k emu (fun _ -> ()) 368
        emu <- emu'
    
    [<Benchmark(Baseline = true)>]
    member _.Generate1Frame_44kHz() =
        let _, emu', _ = Processor.generateFrame processor44k emu (fun _ -> ()) 735
        emu <- emu'
    
    [<Benchmark>]
    member _.Generate1Frame_48kHz() =
        let _, emu', _ = Processor.generateFrame processor48k emu (fun _ -> ()) 800
        emu <- emu'

/// CPU サイクル処理の詳細プロファイリング
[<MemoryDiagnoser>]
type CpuCycleProcessingBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        emu <- powerOn cart
    
    /// CPU tick単体のホットパス (1M回反復)
    [<Benchmark(OperationsPerInvoke = 1_000_000)>]
    member _.CpuTickOnly() =
        for _ in 1..1_000_000 do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'
    
    /// 1フレーム分のCPU実行 (フレーム数)
    [<Benchmark>]
    member _.Execute1Frame() =
        let mutable cycleCount = 0u
        while cycleCount < 29_781u do
            let emu', used = tick emu (fun _ -> ())
            emu <- emu'
            cycleCount <- cycleCount + uint32 used

/// オーバーサンプリング精密化 (32倍検証用)
[<MemoryDiagnoser>]
type OversamplingPrecisionBenchmark() =
    let mutable processor8x = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable processor16x = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable processor32x = Unchecked.defaultof<Processor.AudioProcessor>
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
        processor8x <- Processor.create 44100 8
        processor16x <- Processor.create 44100 16
        processor32x <- Processor.create 44100 32
    
    [<IterationSetup>]
    member _.IterationSetup() =
        emu <- powerOn cart
    
    /// 8倍オーバーサンプリング
    [<Benchmark(OperationsPerInvoke = 150)>]
    member _.Generate100Samples_8x() =
        for _ in 1..150 do
            let _, emu', _ = Processor.generateFrame processor8x emu (fun _ -> ()) 100
            emu <- emu'
    
    /// 16倍オーバーサンプリング
    [<Benchmark(Baseline = true, OperationsPerInvoke = 150)>]
    member _.Generate100Samples_16x() =
        for _ in 1..150 do
            let _, emu', _ = Processor.generateFrame processor16x emu (fun _ -> ()) 100
            emu <- emu'
    
    /// 32倍オーバーサンプリング (新規検証)
    [<Benchmark(OperationsPerInvoke = 150)>]
    member _.Generate100Samples_32x() =
        for _ in 1..150 do
            let _, emu', _ = Processor.generateFrame processor32x emu (fun _ -> ()) 100
            emu <- emu'

/// Expecto テストとしてベンチマークを登録
[<Tests>]
let tests =
    testSequenced <| testList "Audio Processor Benchmarks" [
        test "Audio Processor Detail Benchmark" {
            benchmark<AudioProcessorDetailBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Oversampling Detail Benchmark" {
            benchmark<OversamplingDetailBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "APU Mixing Benchmark" {
            benchmark<ApuMixingBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Sample Rate Comparison Benchmark" {
            benchmark<SampleRateComparisonBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "CPU Cycle Processing Benchmark" {
            benchmark<CpuCycleProcessingBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Oversampling Precision Benchmark" {
            benchmark<OversamplingPrecisionBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
    ]
