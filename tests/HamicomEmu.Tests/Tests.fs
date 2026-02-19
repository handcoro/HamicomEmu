module Tests

open Expecto

open CpuTests
open ApuTests
open PpuTests

[<Tests>]
let tests =
    testList "All Tests" [
        cpuTests
        apuTests
        ppuTests
        ppuScrollTests
    ]

