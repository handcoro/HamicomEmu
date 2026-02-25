module RenderingBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkHelpers
open HamicomEmu.Ppu.Types

// モジュールエイリアス
module Ppu = HamicomEmu.Ppu.Ppu
module Renderer = HamicomEmu.Ppu.Renderer

/// PPU レンダリング（パレット変換）のベンチマーク
[<MemoryDiagnoser>]
type RendererBenchmark() =
    let mutable ppu = Unchecked.defaultof<PpuState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        ppu <- Ppu.init cart
        
        // frameBufferを適当なパターンで埋める（実際のゲーム画面を模擬）
        for i in 0..61439 do
            ppu.frameBuffer[i] <- byte (i % 64)  // パレット範囲内でパターン生成
    
    [<IterationSetup>]
    member _.IterationSetup() =
        // frameBufferの内容は維持（リセット不要）
        ()
    
    /// パレットインデックス→RGB変換（256×240ピクセル = 61,440要素）
    [<Benchmark(OperationsPerInvoke = 1000)>]
    member _.RenderFrameToRgb() =
        for _ in 1..1000 do
            let _ = Renderer.renderFrame ppu
            ()

/// フレーム全体のレンダリングパイプラインベンチマーク
[<MemoryDiagnoser>]
type FrameRenderingBenchmark() =
    let mutable ppu = Unchecked.defaultof<PpuState>
    let mutable cart = Unchecked.defaultof<_>
    
    [<GlobalSetup>]
    member _.Setup() =
        let program = createNopFilledRom ()
        cart <- createTestCartridge program
        ppu <- Ppu.init cart
        
        // 典型的なゲーム画面を模擬
        for i in 0..61439 do
            ppu.frameBuffer[i] <- byte ((i / 256 + i % 256) % 64)
    
    [<IterationSetup>]
    member _.IterationSetup() =
        ()
    
    /// 1フレーム分（262スキャンライン × 341サイクル）の完全実行
    [<Benchmark>]
    member _.CompleteFrameRendering() =
        // 1フレーム = 262スキャンライン × 341サイクル = 89,342サイクル
        let mutable currentPpu = Ppu.init cart
        for i in 0..61439 do
            currentPpu.frameBuffer[i] <- ppu.frameBuffer[i]  // コピー
        
        // 1フレーム分のPPU tick実行
        for _ in 1..89_342 do
            currentPpu <- Ppu.tick currentPpu
        
        // レンダリング
        let _ = Renderer.renderFrame currentPpu
        ()

/// RGB変換後のデータ処理ベンチマーク
[<MemoryDiagnoser>]
type ColorDataProcessingBenchmark() =
    let mutable rgbData = Unchecked.defaultof<(byte * byte * byte)[]>
    
    [<GlobalSetup>]
    member _.Setup() =
        // 典型的なRGBデータを準備
        rgbData <- Array.init 61440 (fun i ->
            let r = byte ((i / 256) % 256)
            let g = byte (i % 256)
            let b = byte ((i * 2) % 256)
            (r, g, b)
        )
    
    [<IterationSetup>]
    member _.IterationSetup() =
        ()
    
    /// RGB→Color配列への変換（MonoGame Colorオブジェクト生成をシミュレート）(100回反復)
    [<Benchmark(OperationsPerInvoke = 100)>]
    member _.ConvertRgbToColorArray() =
        for _ in 1..100 do
            let _ = rgbData |> Array.map (fun (r, g, b) -> struct (r, g, b))
            ()
    
    /// RGB配列の部分コピー（8×8タイル単位）(10,000回反復)
    [<Benchmark(OperationsPerInvoke = 10_000)>]
    member _.TileCopy() =
        for _ in 1..10_000 do
            let tile = Array.zeroCreate<byte * byte * byte> 64
            for ty in 0..7 do
                for tx in 0..7 do
                    let idx = ty * 256 + tx
                    tile[ty * 8 + tx] <- rgbData[idx]
            ()

/// Expecto テストとしてベンチマークを登録
[<Tests>]
let tests =
    testSequenced <| testList "Rendering Benchmarks" [
        test "Renderer Benchmark" {
            benchmark<RendererBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Frame Rendering Benchmark" {
            benchmark<FrameRenderingBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
        test "Color Data Processing Benchmark" {
            benchmark<ColorDataProcessingBenchmark> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ()) |> ignore
        }
    ]
