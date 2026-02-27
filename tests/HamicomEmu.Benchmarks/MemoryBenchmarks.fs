module MemoryBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkHelpers
open HamicomEmu.EmulatorCore.EmulatorCore
open HamicomEmu.Bus

/// メモリアクセス（読み込み）のベンチマーク
[<MemoryDiagnoser>]
type MemoryReadBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前に状態をリセット（メモリリーク防止）
        emu <- powerOn cart
    
    /// RAM ($0000-$07FF) からの読み込み（500,000回反復）
    [<Benchmark(OperationsPerInvoke = 500_000)>]
    member _.ReadFromRam() =
        for _ in 1..500_000 do
            let _, _ = Bus.memRead 0x0100us emu.bus
            ()
    
    /// PPU レジスタ ($2002 Status) からの読み込み（500,000回反復）
    [<Benchmark(OperationsPerInvoke = 500_000)>]
    member _.ReadFromPpuStatus() =
        for _ in 1..500_000 do
            let _, _ = Bus.memRead 0x2002us emu.bus
            ()
    
    /// PRG ROM ($8000-$FFFF) からの読み込み（500,000回反復）
    [<Benchmark(OperationsPerInvoke = 500_000)>]
    member _.ReadFromPrgRom() =
        for _ in 1..500_000 do
            let _, _ = Bus.memRead 0x8000us emu.bus
            ()
    
    /// APU ステータス ($4015) からの読み込み（500,000回反復）
    [<Benchmark(OperationsPerInvoke = 500_000)>]
    member _.ReadFromApuStatus() =
        for _ in 1..500_000 do
            let _, _ = Bus.memRead 0x4015us emu.bus
            ()

/// メモリアクセス（書き込み）のベンチマーク
[<MemoryDiagnoser>]
type MemoryWriteBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前に状態をリセット（メモリリーク防止）
        emu <- powerOn cart
    
    /// RAM ($0000-$07FF) への書き込み（500,000回反復）
    [<Benchmark(OperationsPerInvoke = 500_000)>]
    member _.WriteToRam() =
        for _ in 1..500_000 do
            let _ = Bus.memWrite 0x0100us 0x42uy emu.bus
            ()
    
    /// PPU レジスタ ($2000 Controller) への書き込み（100,000回反復）
    [<Benchmark(OperationsPerInvoke = 100_000)>]
    member _.WriteToPpuController() =
        for _ in 1..100_000 do
            let _ = Bus.memWrite 0x2000us 0x80uy emu.bus
            ()
    
    /// PPU レジスタ ($2007 Data) への書き込み（50,000回反復）
    [<Benchmark(OperationsPerInvoke = 50_000)>]
    member _.WriteToPpuData() =
        for _ in 1..50_000 do
            let _ = Bus.memWrite 0x2007us 0x42uy emu.bus
            ()
    
    /// APU レジスタ ($4000 Pulse1 Control) への書き込み（100,000回反復）
    [<Benchmark(OperationsPerInvoke = 100_000)>]
    member _.WriteToApuPulse1() =
        for _ in 1..100_000 do
            let _ = Bus.memWrite 0x4000us 0x80uy emu.bus
            ()
    
    /// APU ステータス ($4015) への書き込み（100,000回反復）
    [<Benchmark(OperationsPerInvoke = 100_000)>]
    member _.WriteToApuStatus() =
        for _ in 1..100_000 do
            let _ = Bus.memWrite 0x4015us 0x0Fuy emu.bus
            ()
    
    /// APU Frame Counter ($4017) への書き込み（100,000回反復）
    [<Benchmark(OperationsPerInvoke = 100_000)>]
    member _.WriteToApuFrameCounter() =
        for _ in 1..100_000 do
            let _ = Bus.memWrite 0x4017us 0x40uy emu.bus
            ()

/// 16bit メモリ読み込みのベンチマーク
[<MemoryDiagnoser>]
type Memory16BitReadBenchmark() =
    let mutable emu = Unchecked.defaultof<EmulatorState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        emu <- powerOn cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前に状態をリセット（メモリリーク防止）
        emu <- powerOn cart
    
    /// 16bit 読み込み (通常)（100,000回反復）
    [<Benchmark(OperationsPerInvoke = 100_000)>]
    member _.Read16Bit() =
        for _ in 1..100_000 do
            let _, _ = Bus.memRead16 0x0100us emu.bus
            ()
    
    /// 16bit 読み込み (Zero Page)（100,000回反復）
    [<Benchmark(OperationsPerInvoke = 100_000)>]
    member _.Read16BitZeroPage() =
        for _ in 1..100_000 do
            let _, _ = Bus.memRead16ZeroPage 0x10uy emu.bus
            ()

/// Expecto テストとしてベンチマークを登録
[<Tests>]
let tests =
    testSequenced <| testList "Memory Benchmarks" [
        test "Memory Read Benchmark" {
            benchmark<MemoryReadBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Memory Write Benchmark" {
            benchmark<MemoryWriteBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "16-bit Memory Read Benchmark" {
            benchmark<Memory16BitReadBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
    ]
