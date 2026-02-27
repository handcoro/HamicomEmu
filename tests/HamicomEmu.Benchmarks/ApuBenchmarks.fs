module ApuBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open HamicomEmu.Apu.Types
open BenchmarkHelpers

// モジュールエイリアス（tick/mix関数の曖昧さ回避）
module Apu = HamicomEmu.Apu.Apu

/// APU 1サイクル実行のベンチマーク
[<MemoryDiagnoser>]
type ApuTickBenchmark() =
    let mutable apu = Unchecked.defaultof<ApuState>
    
    [<GlobalSetup>]
    member _.Setup() =
        apu <- createApuWithChannels (true, true, true, true, false)
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前にAPU状態をリセット
        apu <- createApuWithChannels (true, true, true, true, false)
    
    /// 全チャンネル有効で1サイクル実行
    [<Benchmark>]
    member _.ApuTickAllChannelsEnabled() =
        let result = Apu.tick apu
        apu <- result.apu
    
    /// 全チャンネル無効で1サイクル実行
    [<Benchmark>]
    member _.ApuTickSilent() =
        apu <- createApuWithChannels (false, false, false, false, false)
        let result = Apu.tick apu
        apu <- result.apu


/// APU レジスタ書き込みのベンチマーク
[<MemoryDiagnoser>]
type ApuRegisterWriteBenchmark() =
    let mutable apu = Unchecked.defaultof<ApuState>
    
    [<GlobalSetup>]
    member _.Setup() =
        apu <- createApuWithChannels (true, true, true, true, false)
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前にAPU状態をリセット
        apu <- createApuWithChannels (true, true, true, true, false)
    
    /// Pulse1 レジスタ ($4000) へ 1000回書き込み
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.WriteToPulse1Control() =
        for i in 0..999 do
            apu <- Apu.write 0x4000us (byte i) apu
    
    /// Status レジスタ ($4015) へ 1000回書き込み
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.WriteToStatus() =
        for i in 0..999 do
            apu <- Apu.write 0x4015us (byte (i % 0x1F)) apu
    
    /// Frame Counter ($4017) へ 1000回書き込み
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.WriteToFrameCounter() =
        for i in 0..999 do
            apu <- Apu.write 0x4017us (byte (i % 0x80)) apu
    
    /// Triangle レジスタ ($4008) へ 1000回書き込み
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.WriteToTriangleControl() =
        for i in 0..999 do
            apu <- Apu.write 0x4008us (byte i) apu
    
    /// Noise レジスタ ($400C) へ 1000回書き込み
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.WriteToNoiseControl() =
        for i in 0..999 do
            apu <- Apu.write 0x400Cus (byte i) apu


/// APU レジスタ読み込みのベンチマーク
[<MemoryDiagnoser>]
type ApuRegisterReadBenchmark() =
    let mutable apu = Unchecked.defaultof<ApuState>
    
    [<GlobalSetup>]
    member _.Setup() =
        apu <- createApuWithChannels (true, true, true, true, false)
        // Status を適切に設定（各チャンネル有効化）
        apu <- Apu.write 0x4015us 0x0Fuy apu
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前にAPU状態をリセット
        apu <- createApuWithChannels (true, true, true, true, false)
        apu <- Apu.write 0x4015us 0x0Fuy apu
    
    /// Status レジスタ ($4015) から 1000回読み込み
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.ReadFromStatus() =
        for _ in 0..999 do
            let _, apu' = Apu.read 0x4015us apu
            apu <- apu'


/// Expecto テストとしてベンチマークを登録
[<Tests>]
let tests =
    testSequenced <| testList "APU Benchmarks" [
        test "APU Tick Benchmark" {
            benchmark<ApuTickBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "APU Register Write Benchmark" {
            benchmark<ApuRegisterWriteBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "APU Register Read Benchmark" {
            benchmark<ApuRegisterReadBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
    ]
