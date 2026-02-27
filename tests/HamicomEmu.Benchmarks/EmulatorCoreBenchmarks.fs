module EmulatorCoreBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkHelpers
open HamicomEmu.EmulatorCore.EmulatorCore

/// エミュレータコアの1サイクル実行ベンチマーク
[<MemoryDiagnoser>]
type EmulatorTickBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前に状態をリセット
        emu <- powerOn cart
    
    /// 1サイクルの実行速度を測定
    [<Benchmark>]
    member _.SingleTick() =
        let emu', _ = tick emu (fun _ -> ())
        emu <- emu'
    
    /// 100サイクルの連続実行速度を測定
    [<Benchmark(OperationsPerInvoke = 100)>]
    member _.HundredTicks() =
        for _ in 1..100 do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'

/// エミュレータの1フレーム実行ベンチマーク
[<MemoryDiagnoser>]
type OneFrameBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前に状態をリセット
        emu <- powerOn cart
    
    /// 1フレーム分（29,781サイクル）の実行速度を測定
    /// 理論値: 60Hz = 16.7ms/frame
    [<Benchmark>]
    member _.EmulateOneFrame() =
        // NES の CPU クロック: 1.789773 MHz
        // 1/60秒 ≈ 29,781 CPU cycles
        for _ in 1..cyclesPerFrame do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'
    
    /// 10フレーム分の実行速度を測定
    [<Benchmark>]
    member _.EmulateTenFrames() =
        for _ in 1..cyclesPer10Frames do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'

/// 実際の命令パターンでのベンチマーク
[<MemoryDiagnoser>]
type RealisticWorkloadBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createAddressingModeTestRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前に状態をリセット
        emu <- powerOn cart
    
    /// 各種アドレッシングモードを含む1フレーム分（29,781サイクル）の実行
    [<Benchmark>]
    member _.RealisticOneFrame() =
        for _ in 1..cyclesPerFrame do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'

/// Expecto テストとしてベンチマークを登録
[<Tests>]
let tests =
    testSequenced <| testList "EmulatorCore Benchmarks" [
        test "Emulator Tick Benchmark" {
            benchmark<EmulatorTickBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "One Frame Benchmark" {
            benchmark<OneFrameBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Realistic Workload Benchmark" {
            benchmark<RealisticWorkloadBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
    ]
