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


/// Expecto テストとしてベンチマークを登録
[<Tests>]
let tests =
    testSequenced <| testList "APU Benchmarks" [
        test "APU Tick Benchmark" {
            benchmark<ApuTickBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
    ]
