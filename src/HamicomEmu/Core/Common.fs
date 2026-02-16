namespace HamicomEmu.Common

module Constants =
    let cpuClockNTSC = 1_789_773.0 // NTSC NES の CPU クロック

module BitUtils =
    let inline hasFlag flag p = p &&& flag <> 0uy
    let inline setFlag flag p = p ||| flag
    let inline clearFlag flag p = p &&& ~~~flag

    let updateFlag flag condition p : byte =
        if condition then setFlag flag p else clearFlag flag p
