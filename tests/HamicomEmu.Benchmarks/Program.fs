module Program

open Expecto

/// すべてのベンチマークテストを集約
let allTests =
    testList "HamicomEmu Benchmarks" [
        ComponentProfileBenchmarks.tests
        CpuBenchmarks.tests
        MemoryBenchmarks.tests
        EmulatorCoreBenchmarks.tests
        PpuBenchmarks.tests
        ApuBenchmarks.tests
        RenderingBenchmarks.tests
        AudioProcessorBenchmarks.tests
    ]

[<EntryPoint>]
let main args =
    // Expecto でベンチマークを実行
    // コマンドライン引数:
    //   dotnet run -c Release -- --sequenced : 順次実行
    //   dotnet run -c Release -- --filter <pattern> : 特定のベンチマークのみ実行
    //   dotnet run -c Release -- --help : ヘルプを表示
    runTestsWithCLIArgs [] args allTests
