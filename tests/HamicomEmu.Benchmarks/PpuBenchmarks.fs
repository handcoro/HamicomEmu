module PpuBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkHelpers
open HamicomEmu.Ppu.Types
open HamicomEmu.Ppu.Sprite

// モジュールエイリアス
module Ppu = HamicomEmu.Ppu.Ppu

/// PPU 1サイクル実行のベンチマーク
[<MemoryDiagnoser>]
type PpuTickBenchmark() =
    let mutable ppu = Unchecked.defaultof<PpuState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        ppu <- createPpuAtScanline 100us 100u cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前にPPU状態をリセット
        ppu <- createPpuAtScanline 100us 100u cart
    
    /// 描画ライン（0-239）で1サイクル実行（200,000回反復）
    [<Benchmark(OperationsPerInvoke = 200_000)>]
    member _.PpuTickVisibleLine() =
        for _ in 1..200_000 do
            ppu <- Ppu.tick ppu
    
    /// VBlankライン（240-260）で1サイクル実行（300,000回反復）
    [<Benchmark(OperationsPerInvoke = 300_000)>]
    member _.PpuTickVBlankLine() =
        for _ in 1..300_000 do
            ppu.scanline <- 245us
            ppu <- Ppu.tick ppu

/// PPU スキャンライン実行のベンチマーク
[<MemoryDiagnoser>]
type PpuScanlineBenchmark() =
    let mutable ppu = Unchecked.defaultof<PpuState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        ppu <- Ppu.init cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前にPPU状態をリセット
        ppu <- Ppu.init cart
        ppu.scanline <- 0us
        ppu.cycle <- 0u
    
    /// 1スキャンライン（341サイクル）の実行
    [<Benchmark>]
    member _.OneScanlineWithRendering() =
        ppu <- Ppu.tickN 341u ppu

/// スプライト評価のベンチマーク
[<MemoryDiagnoser>]
type SpriteEvaluationBenchmark() =
    let mutable ppu = Unchecked.defaultof<PpuState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前にPPU状態をリセット
        ppu <- Ppu.init cart
    
    /// スプライト0個のケース（10,000回反復）
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.EvaluateSprites0Active() =
        for _ in 1..10_000 do
            ppu <- Ppu.init cart
            let count = Sprite.evaluateForLineToEval 0 ppu
            ()
    
    /// スプライト8個（OAM上限）のケース（10,000回反復）
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.EvaluateSprites8Active() =
        for _ in 1..10_000 do
            ppu <- createPpuWithSprites 8 cart
            let count = Sprite.evaluateForLineToEval 0 ppu
            ()
    
    /// スプライト64個（OAM全体）のケース（10,000回反復）
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.EvaluateSprites64Full() =
        for _ in 1..10_000 do
            ppu <- createPpuWithSprites 64 cart
            let count = Sprite.evaluateForLineToEval 0 ppu
            ()

/// Background タイルロードのベンチマーク
[<MemoryDiagnoser>]
type BackgroundLoadBenchmark() =
    let mutable ppu = Unchecked.defaultof<PpuState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        ppu <- createPpuAtScanline 0us 0u cart
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // 各反復前にPPU状態をリセット
        ppu <- createPpuAtScanline 0us 0u cart
    
    /// 8サイクル分のタイルフェッチ処理
    [<Benchmark>]
    member _.LoadOneTile() =
        // 8サイクル実行（1タイル分のフェッチ）
        ppu <- Ppu.tickN 8u ppu

/// Expecto テストとしてベンチマークを登録
[<Tests>]
let tests =
    testSequenced <| testList "PPU Benchmarks" [
        test "PPU Tick Benchmark" {
            benchmark<PpuTickBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "PPU Scanline Benchmark" {
            benchmark<PpuScanlineBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Sprite Evaluation Benchmark" {
            benchmark<SpriteEvaluationBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Background Load Benchmark" {
            benchmark<BackgroundLoadBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
    ]
