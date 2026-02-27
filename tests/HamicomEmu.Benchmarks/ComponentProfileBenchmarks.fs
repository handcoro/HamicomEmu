module ComponentProfileBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Engines
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Loggers
open BenchmarkDotNet.Environments
open Perfolizer.Horology
open HamicomEmu
open HamicomEmu.Cartridge
open HamicomEmu.EmulatorCore
open HamicomEmu.Bus
open HamicomEmu.Cpu
open HamicomEmu.Ppu
open HamicomEmu.Apu
open HamicomEmu.Platform.Audio
open BenchmarkHelpers

/// 安定した測定のためのカスタムベンチマーク設定
/// - GC 最適化で測定ノイズを最小化
/// - より長い iteration time で BimodalDistribution を回避
/// - warmup と iteration を固定して測定の再現性を向上
/// - MemoryDiagnoser 常時有効（alloc 分析用、CPU profiling は --profiler ETW で別途指定）
type StableConfig() as this =
    inherit ManualConfig()
    do
        let job = 
            Job.Default
                .WithWarmupCount(10)           // warmup 回数を増やして JIT 最適化を安定化
                .WithIterationCount(30)         // iteration 回数を固定
                .WithMinIterationTime(TimeInterval.FromMilliseconds(200.0))  // 最低 200ms 実行して短時間測定を回避
                .WithMaxIterationCount(50)      // 最大 iteration 制限
                .WithStrategy(RunStrategy.Monitoring)  // 自動調整を無効化し固定実行
                // GC 最適化: メモリ割り当て変動による測定ノイズを削減
                .WithEnvironmentVariable("DOTNET_GCHeapAffinitizeMask", "1")  // GC affinity を固定
                .WithEnvironmentVariable("DOTNET_GCServer", "0")              // Server GC 無効化
        this.AddJob(job) |> ignore
        
        // Memory allocation 分析は常時有効（GC 変動の原因特定用）
        this.AddDiagnoser(MemoryDiagnoser.Default) |> ignore

        // サマリー表の列を明示して出力する
        this.AddColumnProvider(DefaultColumnProviders.Instance) |> ignore
        
        // ロガーを追加して実行進度を表示
        this.AddLogger(ConsoleLogger.Default) |> ignore

/// コンポーネント単位で割り当てと実行時間を測定するベンチマーク
/// 14.4MB/frame の割り当て源泉と実行時間の両方を明らかにする
[<MemoryDiagnoser>]
type ComponentProfileBenchmarks() =
    let mutable emu: EmulatorCore.EmulatorState = Unchecked.defaultof<_>
    let mutable bus: Bus.BusState = Unchecked.defaultof<_>
    let mutable cpu: Cpu.CpuState = Unchecked.defaultof<_>
    let mutable processor: Processor.AudioProcessor = Unchecked.defaultof<_>
    let mutable cart: Cartridge = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member this.Setup() =
        // テスト用カートリッジを生成
        let program = BenchmarkHelpers.createNopFilledRom ()
        cart <- BenchmarkHelpers.createTestCartridge program
        let initialEmu = EmulatorCore.powerOn cart
        emu <- initialEmu
        bus <- initialEmu.bus
        cpu <- initialEmu.cpu
        processor <- Processor.create 44100 1  // 1xオーバーサンプリング

    [<IterationSetup>]
    member this.IterationSetup() =
        let initialEmu = EmulatorCore.powerOn cart
        emu <- initialEmu
        bus <- initialEmu.bus
        cpu <- initialEmu.cpu
        processor <- Processor.create 44100 1

    /// memRead を PPU アドレス範囲で 1000回呼び出し
    [<Benchmark(Description = "memRead x1000 (PPU range)", OperationsPerInvoke = 20)>]
    member this.MemReadPpuOnly1000() =
        let mutable b' = bus
        for _ in 1..20 do
            for i in 0..999 do
                let addr = 0x2000us + (uint16 (i % 8))
                let _, b'' = Bus.memRead addr b'
                b' <- b''
        b'

    /// memWrite を PPU アドレス範囲で 1000回呼び出し
    [<Benchmark(Description = "memWrite x1000 (PPU range)", OperationsPerInvoke = 20)>]
    member this.MemWritePpuOnly1000() =
        let mutable b' = bus
        for _ in 1..20 do
            for i in 0..999 do
                let addr = 0x2000us + (uint16 (i % 8))
                b' <- Bus.memWrite addr (byte i) b'
        b'

    /// memRead を CPU RAM で 1000回呼び出し（割り当てなし想定）
    [<Benchmark(Description = "memRead x1000 (RAM only)", OperationsPerInvoke = 20)>]
    member this.MemReadRamOnly1000() =
        let mutable b' = bus
        for _ in 1..20 do
            for i in 0..999 do
                let addr = uint16 (i % 0x800)  // RAM mirroring
                let _, b'' = Bus.memRead addr b'
                b' <- b''
        b'

    /// Bus.tick を 1000回実行の割り当て
    [<Benchmark(Description = "Bus.tick x1000", OperationsPerInvoke = 20)>]
    member this.BusTickOnly1000() =
        let mutable e = emu
        for _ in 1..20 do
            for _ in 1..1000 do
                e <- Bus.tick e.bus |> fun bus' -> { e with bus = bus' }
        e

    /// Ppu.tickN を 3000回呼び出し（Bus.tick内での呼び出し相当量）
    [<Benchmark(Description = "Ppu.tickN x3000", OperationsPerInvoke = 20)>]
    member this.PpuTickOnly3000() =
        let mutable ppu = bus.ppu
        for _ in 1..20 do
            for _ in 1..1000 do
                ppu <- Ppu.tickN 3u ppu
        ppu

    /// Apu.tick を 1000回呼び出し
    [<Benchmark(Description = "Apu.tick x1000", OperationsPerInvoke = 20)>]
    member this.ApuTickOnly1000() =
        let mutable apu = bus.apu
        for _ in 1..20 do
            for _ in 1..1000 do
                let result = Apu.tick apu
                apu <- result.apu
        apu

    /// Cpu.step を 100回実行の割り当て（Bus経由、CPU+PPU+APU複合）
    [<Benchmark(Description = "Cpu.step x100", OperationsPerInvoke = 200)>]
    member this.CpuStepOnly100() =
        let mutable e = emu
        for _ in 1..200 do
            for _ in 1..100 do
                let cpu', bus', _consumed = Cpu.step e.cpu e.bus
                e <- { cpu = cpu'; bus = bus' }
        e

    // PPU レジスタ書き込みテストは PpuRegisterWriteBenchmarks に統合済み

    /// Bus.memWrite を PPUアドレス 1回のみ
    [<Benchmark(Description = "memWrite x1 (PPU addr 0x2000)", OperationsPerInvoke = 5000)>]
    member this.MemWritePpuSingle() =
        let mutable b' = bus
        for _ in 1..5000 do
            b' <- Bus.memWrite 0x2000us 0x80uy b'
        b'

    /// Bus.memRead を PPUアドレス 1回のみ
    [<Benchmark(Description = "memRead x1 (PPU addr 0x2002)", OperationsPerInvoke = 5000)>]
    member this.MemReadPpuSingle() =
        let mutable b' = bus
        for _ in 1..5000 do
            let _, b'' = Bus.memRead 0x2002us b'
            b' <- b''
        b'

    /// EmulatorCore.tick を 1000回実行の割り当て（完全な流れ）
    [<Benchmark(Description = "EmulatorCore.tick x1000", OperationsPerInvoke = 20)>]
    member this.EmulatorCoreTick1000() =
        let mutable e = emu
        for _ in 1..20 do
            for _ in 1..1000 do
                let e', _ = EmulatorCore.tick e (fun _ -> ())
                e <- e'
        e

/// PPU レジスタ書き込みの割り当てを個別に測定
[<MemoryDiagnoser>]
type PpuRegisterWriteBenchmarks() =
    let mutable bus: Bus.BusState = Unchecked.defaultof<_>
    let mutable cart: Cartridge = Unchecked.defaultof<_>

    [<GlobalSetup>]
    member this.Setup() =
        let program = BenchmarkHelpers.createNopFilledRom ()
        cart <- BenchmarkHelpers.createTestCartridge program
        let initialEmu = EmulatorCore.powerOn cart
        bus <- initialEmu.bus

    [<IterationSetup>]
    member this.IterationSetup() =
        let initialEmu = EmulatorCore.powerOn cart
        bus <- initialEmu.bus

    [<Benchmark(Description = "Ppu.writeToControlRegister x1000", OperationsPerInvoke = 20)>]
    member this.PpuWriteControl1000() =
        let mutable ppu = bus.ppu
        for _ in 1..20 do
            for i in 0..999 do
                ppu <- Ppu.writeToControlRegister (byte i) ppu
        ppu

    [<Benchmark(Description = "Ppu.writeToMaskRegister x1000", OperationsPerInvoke = 20)>]
    member this.PpuWriteMask1000() =
        let mutable ppu = bus.ppu
        for _ in 1..20 do
            for i in 0..999 do
                ppu <- Ppu.writeToMaskRegister (byte i) ppu
        ppu

    [<Benchmark(Description = "Ppu.writeToScrollRegister x1000", OperationsPerInvoke = 20)>]
    member this.PpuWriteScroll1000() =
        let mutable ppu = bus.ppu
        for _ in 1..20 do
            for i in 0..999 do
                ppu <- Ppu.writeToScrollRegister (byte i) ppu
        ppu

    [<Benchmark(Description = "Ppu.writeToAddressRegister x1000", OperationsPerInvoke = 20)>]
    member this.PpuWriteAddress1000() =
        let mutable ppu = bus.ppu
        for _ in 1..20 do
            for i in 0..999 do
                ppu <- Ppu.writeToAddressRegister (byte i) ppu
        ppu

    [<Benchmark(Description = "Ppu.writeToDataRegister x1000", OperationsPerInvoke = 20)>]
    member this.PpuWriteData1000() =
        let mutable ppu = bus.ppu
        for _ in 1..20 do
            for i in 0..999 do
                ppu <- Ppu.writeToDataRegister (byte i) ppu
        ppu

[<Tests>]
let tests =
    let stableConfig = StableConfig()
    testSequenced <| testList "Component Profile Benchmarks" [
        test "MemReadPpuOnly1000" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "MemWritePpuOnly1000" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "MemReadRamOnly1000" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "BusTickOnly1000" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "PpuTickOnly3000" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "ApuTickOnly1000" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "CpuStepOnly100" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        // PPU レジスタ書き込みテスト（削除 - PpuRegisterWriteBenchmarks に統合）
        test "MemWritePpuSingle" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "MemReadPpuSingle" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "EmulatorCoreTick1000" {
            benchmark<ComponentProfileBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "PpuWriteControl1000" {
            benchmark<PpuRegisterWriteBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "PpuWriteMask1000" {
            benchmark<PpuRegisterWriteBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "PpuWriteScroll1000" {
            benchmark<PpuRegisterWriteBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "PpuWriteAddress1000" {
            benchmark<PpuRegisterWriteBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
        test "PpuWriteData1000" {
            benchmark<PpuRegisterWriteBenchmarks> stableConfig (fun _ -> box ())
            |> ignore
        }
    ]


