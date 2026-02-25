module CpuBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkHelpers
open HamicomEmu.EmulatorCore.EmulatorCore

/// CPU命令実行のベンチマーク
[<MemoryDiagnoser>]
type CpuInstructionBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createSimpleInstructionRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前に状態をリセット
        emu <- powerOn cart
    
    /// 単一命令の実行速度を測定（300,000回反復）
    [<Benchmark(OperationsPerInvoke = 300_000)>]
    member _.SingleInstruction() =
        for _ in 1..300_000 do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'

/// 各種アドレッシングモードのベンチマーク
[<MemoryDiagnoser>]
type AddressingModeBenchmark() =
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
    
    /// 各種アドレッシングモードの命令を連続実行（100,000回反復）
    [<Benchmark(OperationsPerInvoke = 100_000)>]
    member _.MixedAddressingModes() =
        for _ in 1..100_000 do
            let emu', _ = tick emu (fun _ -> ())
            emu <- emu'

/// Expecto テストとしてベンチマークを登録
[<Tests>]
let tests =
    testSequenced <| testList "CPU Benchmarks" [
        test "CPU Instruction Benchmark" {
            benchmark<CpuInstructionBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Addressing Mode Benchmark" {
            benchmark<AddressingModeBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
    ]
