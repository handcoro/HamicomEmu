module ComponentProfileBenchmarks

open Expecto
open Expecto.BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open HamicomEmu
open HamicomEmu.Cartridge
open HamicomEmu.EmulatorCore
open HamicomEmu.Bus
open HamicomEmu.Cpu
open HamicomEmu.Ppu
open HamicomEmu.Apu
open HamicomEmu.Platform.Audio
open BenchmarkHelpers

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

    /// Ppu.writeToControlRegister を 1000回直接呼び出し（PPU内部のみ）
    [<Benchmark(Description = "Ppu.writeToControlRegister x1000", OperationsPerInvoke = 20)>]
    member this.PpuWriteDirect1000() =
        let mutable ppu = bus.ppu
        for _ in 1..20 do
            for i in 0..999 do
                ppu <- Ppu.writeToControlRegister (byte i) ppu
        ppu

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

[<Tests>]
let tests =
    testSequenced <| testList "Component Profile Benchmarks" [
        test "MemReadPpuOnly1000" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "MemWritePpuOnly1000" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "MemReadRamOnly1000" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "BusTickOnly1000" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "PpuTickOnly3000" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "ApuTickOnly1000" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "CpuStepOnly100" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "PpuWriteDirect1000" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "MemWritePpuSingle" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "MemReadPpuSingle" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
        test "EmulatorCoreTick1000" {
            benchmark<ComponentProfileBenchmarks> BenchmarkDotNet.Configs.DefaultConfig.Instance (fun _ -> box ())
            |> ignore
        }
    ]


