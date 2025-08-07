module Tests

open System.IO

open Expecto
open HamicomEmu.Common.BitUtils
open HamicomEmu.Cpu
open HamicomEmu.Bus
open HamicomEmu.Ppu.Scroll
open HamicomEmu.Ppu
open HamicomEmu.Apu
open HamicomEmu.Apu.Types
open HamicomEmu.Cartridge
open HamicomEmu.Trace

let initialDmcState = {
    irqEnabled = false
    isLoop = false
    rateIndex = 0uy

    startAddress = 0uy
    sampleLength = 0uy

    currentAddress = 0xC000us
    bytesRemaining = 0us
    buffer = Some 0b1111_0000uy

    shiftRegister = 0uy
    bitsRemaining = 0

    outputLevel = 64uy
    timer = 0us
    isSilence = false
    irqRequested = false
    lastOutput = 0uy
}

let tests =
    testList "All Tests" [
        testList "APU Tests" [
            test "DMC tick consumes bit and updates outputLevel" {
                let state = {
                    initialDmcState with
                        buffer = Some 0b1111_0000uy
                        outputLevel = 64uy
                        bitsRemaining = 0
                        timer = 0us
                }

                let state1, _, _ = Dmc.tick state
                Expect.equal state1.shiftRegister 0b0111_1000uy "Shifted 1 bit"
                Expect.equal state1.bitsRemaining 7 "bitCounter updated"
                Expect.equal state1.outputLevel 62uy "Output decreased (bit=0)"
            }

            test "DMC processes 8 bits of sampleBuffer and updates outputLevel" {
                let init = {
                    initialDmcState with
                        buffer = Some 0b1111_0000uy
                        outputLevel = 64uy
                        bitsRemaining = 0
                        timer = 0us
                }

                // tick 1: 0 → -2 → 62
                let s1, _, _ = Dmc.tick init
                Expect.equal s1.outputLevel 62uy "Tick 1: output -2"
                Expect.equal s1.bitsRemaining 7 "Tick 1: bitsRemaining 7"
                Expect.equal s1.shiftRegister 0b0111_1000uy "Tick 1: shifted once"

                // tick 2: 0 → -2 → 60
                let s2, _, _ = Dmc.tick { s1 with timer = 0us }
                Expect.equal s2.outputLevel 60uy "Tick 2: output -2"
                Expect.equal s2.bitsRemaining 6 "Tick 2: bitsRemaining 6"
                Expect.equal s2.shiftRegister 0b0011_1100uy "Tick 2: shifted"

                // tick 3: 0 → -2 → 58
                let s3, _, _ = Dmc.tick { s2 with timer = 0us }
                Expect.equal s3.outputLevel 58uy "Tick 3: output -2"
                Expect.equal s3.bitsRemaining 5 "Tick 3: bitsRemaining 5"

                // tick 4: 0 → -2 → 56
                let s4, _, _ = Dmc.tick { s3 with timer = 0us }
                Expect.equal s4.outputLevel 56uy "Tick 4: output -2"
                Expect.equal s4.bitsRemaining 4 "Tick 4: bitsRemaining 4"

                // tick 5: 1 → +2 → 58
                let s5, _, _ = Dmc.tick { s4 with timer = 0us }
                Expect.equal s5.outputLevel 58uy "Tick 5: output +2"
                Expect.equal s5.bitsRemaining 3 "Tick 5: bitsRemaining 3"

                // tick 6: 1 → +2 → 60
                let s6, _, _ = Dmc.tick { s5 with timer = 0us }
                Expect.equal s6.outputLevel 60uy "Tick 6: output +2"

                // tick 7: 1 → +2 → 62
                let s7, _, _ = Dmc.tick { s6 with timer = 0us }
                Expect.equal s7.outputLevel 62uy "Tick 7: output +2"

                // tick 8: 1 → +2 → 64
                let s8, _, _ = Dmc.tick { s7 with timer = 0us }
                Expect.equal s8.outputLevel 64uy "Tick 8: output +2"

                // tick 9: buffer=None かつ bitsRemaining=0 → silence=true になるはず
                let s9, _, _ = Dmc.tick { s8 with timer = 0us }
                Expect.isTrue s9.isSilence "Tick 9: silence flag should be set"
                Expect.equal s9.outputLevel 64uy "Tick 9: output stays same"
            }

            test "startSample correctly sets currentAddress and bytesRemaining" {
                let dmc = {
                    initialDmcState with
                        startAddress = 0x20uy  // = 0x20 * 64 = 0x800
                        sampleLength = 0x10uy  // = 0x10 * 16 + 1 = 257
                }

                let started = Dmc.startSample dmc

                let expectedAddr = 0xC000us + uint16 dmc.startAddress * 64us
                let expectedBytes = uint16 dmc.sampleLength * 16us + 1us

                Expect.equal started.currentAddress expectedAddr "currentAddress is correct"
                Expect.equal started.bytesRemaining expectedBytes "bytesRemaining is correct"
            }

            test "applySampleRead restarts sample when loop flag is set and bytesRemaining reaches 0" {
                let dmc = {
                    initialDmcState with
                        startAddress = 0x21uy  // 0xC000 + 0x840 = 0xC840
                        sampleLength = 0x12uy  // (0x12 * 16) + 1 = 289
                        currentAddress = 0xFFFFus
                        bytesRemaining = 1us
                        isLoop = true
                }

                let dmc' = Dmc.applySampleRead 0x55uy dmc

                let expectedAddr = 0xC000us + (uint16 dmc.startAddress <<< 6)
                let expectedRemaining = (uint16 dmc.sampleLength <<< 4) + 1us

                Expect.equal dmc'.currentAddress expectedAddr "Loop currentAddress reset correctly"
                Expect.equal dmc'.bytesRemaining expectedRemaining "Loop bytesRemaining reset correctly"
                Expect.equal dmc'.buffer (Some 0x55uy) "Buffer updated correctly"
            }

            test "applySampleRead does not restart sample when loop flag is false and bytesRemaining reaches 0" {
                let dmc = {
                    initialDmcState with
                        startAddress = 0x21uy
                        sampleLength = 0x12uy
                        currentAddress = 0x1234us
                        bytesRemaining = 1us
                        isLoop = false
                }

                let dmc' = Dmc.applySampleRead 0xAAuy dmc

                // currentAddress should just +1, not reset
                Expect.equal dmc'.currentAddress 0x1235us "currentAddress incremented"
                Expect.equal dmc'.bytesRemaining 0us "bytesRemaining decremented to 0"
                Expect.equal dmc'.buffer (Some 0xAAuy) "Buffer updated"
            }

            test "DMC requests IRQ when sample ends and loop is disabled" {
                let dmc = {
                    initialDmcState with
                        isLoop = false
                        irqEnabled = true
                        bytesRemaining = 0us  // すでに終わってる状態
                        buffer = None         // 読み込むサンプルが無い
                        bitsRemaining = 0     // シフトも終わってる
                        timer = 0us           // tick で処理が入る
                }

                let dmc', _, _ = Dmc.tick dmc

                Expect.isTrue dmc'.irqRequested "IRQ should be requested at end of sample"
            }

            test "applySampleRead clears irqRequested flag" {
                let dmc = {
                    initialDmcState with
                        irqRequested = true
                        buffer = None
                        currentAddress = 0x8000us
                        bytesRemaining = 1us
                }

                let dmc' = Dmc.applySampleRead 0xAAuy dmc

                Expect.equal dmc'.irqRequested false "IRQ should be cleared when new sample is read"
            }

            test "APU $4015 read reflects all channel statuses and IRQ flags" {
                let apu = {
                    Apu.init with
                        pulse1 = { Pulse.init One with lengthCounter = 5uy }
                        pulse2 = { Pulse.init Two with lengthCounter = 0uy }
                        triangle = { Triangle.init with lengthCounter = 2uy }
                        noise = { Noise.init with lengthCounter = 0uy }
                        dmc = { Dmc.init with bytesRemaining = 10us; irqRequested = true }
                        frameCounter.irqRequested = true
                        status =
                            StatusFlags.pulse1Enable |||
                            StatusFlags.pulse2Enable |||
                            StatusFlags.triangleEnable |||
                            StatusFlags.noiseEnable |||
                            StatusFlags.dmcEnable |||
                            StatusFlags.frameInterrupt
                }

                let result, apu' = Apu.read 0x4015us apu

                // bit 7 = DMC IRQ → 1
                Expect.isTrue (result &&& 0b1000_0000uy <> 0uy) "DMC IRQ should be set"

                // bit 6 = Frame IRQ → 1 (and should be cleared in apu')
                Expect.isTrue (result &&& 0b0100_0000uy <> 0uy) "Frame IRQ should be set"
                Expect.isFalse (hasFlag StatusFlags.frameInterrupt apu'.status) "Frame IRQ should be cleared after read"

                // bit 4 = DMC active (bytesRemaining > 0) → 1
                Expect.isTrue (result &&& 0b0001_0000uy <> 0uy) "DMC should be active"

                // bit 3 = Noise channel lengthCounter = 0 → 0
                Expect.isFalse (result &&& 0b0000_1000uy <> 0uy) "Noise should be inactive"

                // bit 2 = Triangle channel lengthCounter = 2 → 1
                Expect.isTrue (result &&& 0b0000_0100uy <> 0uy) "Triangle should be active"

                // bit 1 = Pulse2 channel lengthCounter = 0 → 0
                Expect.isFalse (result &&& 0b0000_0010uy <> 0uy) "Pulse2 should be inactive"

                // bit 0 = Pulse1 channel lengthCounter = 5 → 1
                Expect.isTrue (result &&& 0b0000_0001uy <> 0uy) "Pulse1 should be active"
            }


        ]

        testList "PPU Tests" [
            test "PPU Register Test" {
                let ppuAddr = 0x2000us
                let value = 0x77uy

                let bus0 = Bus.init (testCartridge [||])
                let mutable ppu = bus0.ppu

                // コントロールレジスタ設定
                // printfn $"before: {ppu.scroll}"
                ppu <- Ppu.writeToControlRegister 0x00uy ppu
                // アドレス設定
                ppu <- Ppu.writeToAddressRegister 0x20uy ppu
                ppu <- Ppu.writeToAddressRegister 0x00uy ppu
                // printfn $"1: {ppu.scroll}"

                // $2007 に書き込み
                ppu <- Ppu.writeToDataRegister value ppu
                // printfn $"2: {ppu.scroll}"

                // 再びアドレス設定（VRAM 読みは1バイトずれている可能性あり）
                ppu <- Ppu.writeToAddressRegister 0x20uy ppu
                ppu <- Ppu.writeToAddressRegister 0x00uy ppu
                // printfn $"3: {ppu.scroll}"

                // 読み捨て
                let _, ppu = Ppu.readFromDataRegister ppu
                // 実データ読み
                let actual, _ = Ppu.readFromDataRegister ppu

                Expect.equal actual value "Should correctly read back the value written to PPU memory"
            }
            // 初期値がすべて0か
            test "init values are all zero" {
                let ppu = Ppu.init (testCartridge [||])
                Expect.equal ppu.scroll.v 0us "v should be zero"
                Expect.equal ppu.scroll.t 0us "t should be zero"
                Expect.equal ppu.scroll.x 0uy "x should be zero"
                Expect.equal ppu.scroll.w false "w should be false"
            }

            // $2006 最初の書き込みで t の上位バイトが更新され、w が true になる
            test "writeToAddressRegister (first) updates t hi and sets w=true" {
                let ppu = Ppu.init (testCartridge [||])
                let ppu' = Ppu.writeToAddressRegister 0x21uy ppu
                Expect.equal ppu'.scroll.t 0x2100us "t hi byte should be set"
                Expect.equal ppu'.scroll.w true "w should be true"
            }

            // $2006 2回目の書き込みで t が完成し、v にコピーされ、w が false になる
            test "writeToAddressRegister (second) completes t, copies t to v, sets w=false" {
                let ppu = Ppu.init (testCartridge [||]) |> Ppu.writeToAddressRegister 0x21uy
                let ppu' = Ppu.writeToAddressRegister 0x34uy ppu
                Expect.equal ppu'.scroll.t 0x2134us "t should be combined"
                Expect.equal ppu'.scroll.v 0x2134us "v should be updated"
                Expect.equal ppu'.scroll.w false "w should be false"
            }

            // $2005 書き込みで x, t, w が正しく変化するか
            test "writeToScrollRegister updates x, t, and w" {
                let ppu = Ppu.init (testCartridge [||])
                let ppu' = Ppu.writeToScrollRegister 0xE3uy ppu
                // x = 3, t下位5ビット = 0x1C, w = true
                Expect.equal ppu'.scroll.x 0x3uy "x should be 3"
                Expect.equal (ppu'.scroll.t &&& 0x1Fus) 0x1Cus "t lower 5 bits should be 0x1C"
                Expect.equal ppu'.scroll.w true "w should be true"
            }

            // $2002 ステータスレジスタ読み出しで w がリセットされるか
            test "Status read resets w" {
                let ppu = Ppu.init (testCartridge [||]) |> Ppu.writeToScrollRegister 0x12uy
                let _, ppu' = Ppu.readFromStatusRegister ppu
                Expect.equal ppu'.scroll.w false "w should be reset to false"
            }

            // アドレスは必ず 0x3FFF でマスクされているか
            test "Address should always be masked to 0x3FFF" {
                let ppu = Ppu.init (testCartridge [||])
                let ppu' = { ppu with scroll = { ppu.scroll with v = 0x5555us } }
                let _, ppu'' = Ppu.readFromDataRegister ppu'
                Expect.isTrue (ppu''.scroll.v <= 0x3FFFus) "v should be masked"
            }
        
            test "write and read VRAM at $2000" {
                // PPUの初期化
                let ppu0 = Ppu.init (testCartridge [||])

                // $2005: Xスクロールを設定
                let ppu1 = Ppu.writeToScrollRegister 0x23uy ppu0
                // $2005: Yスクロールを設定
                let ppu2 = Ppu.writeToScrollRegister 0x45uy ppu1

                // $2006: アドレス上位 ($2000の上位バイト)
                let ppu3 = Ppu.writeToAddressRegister 0x20uy ppu2
                // $2006: アドレス下位 ($2000の下位バイト)
                let ppu4 = Ppu.writeToAddressRegister 0x00uy ppu3

                // $2007: VRAM（$2000）へ書き込み
                let ppu5 = Ppu.writeToDataRegister 0xABuy ppu4

                // 再び $2000 をアドレスセット（オートインクリメント対策）
                let ppu6 = Ppu.writeToAddressRegister 0x20uy ppu5
                let ppu7 = Ppu.writeToAddressRegister 0x00uy ppu6

                // $2007: VRAMから2回読み込み（1回目はバッファ、2回目が本体の値）
                let _, ppu8 = Ppu.readFromDataRegister ppu7  // バッファ値（無視）
                let value, _ = Ppu.readFromDataRegister ppu8 // 実値

                // アドレスやスクロールレジスタもついでに確認
                Expect.equal ppu4.scroll.v 0x2000us "v should be set to 0x2000"
                Expect.equal ppu4.scroll.t 0x2000us "t should be set to 0x2000"
                Expect.equal ppu2.scroll.x 0x03uy "x scroll should be set to 0x03"
                Expect.isFalse ppu4.scroll.w "w should be false after second write"

                // VRAM内容の検証
                Expect.equal value 0xABuy "VRAM should contain 0xAB at $2000"
            }

            test "increment by 1 when increment bit is 0" {
                // PPU初期化
                let ppu0 = Ppu.init (testCartridge [||])

                // $2000: コントロールレジスタ (インクリメント=1)
                let ppu1 = Ppu.writeToControlRegister 0x00uy ppu0

                // $2006: アドレス $3000 をセット
                let ppu2 = Ppu.writeToAddressRegister 0x30uy ppu1
                let ppu3 = Ppu.writeToAddressRegister 0x00uy ppu2

                // $2007: データ書き込み（ここでアドレスが+1進む）
                let ppu4 = Ppu.writeToDataRegister 0xAAuy ppu3

                // v（内部アドレスレジスタ）が$3001になっているか
                Expect.equal ppu4.scroll.v 0x3001us "v should increment by 1"
            }

            test "increment by 32 when increment bit is set" {
                let ppu0 = Ppu.init (testCartridge [||])

                // $2000: コントロールレジスタ (インクリメント=32)
                let ppu1 = Ppu.writeToControlRegister 0x04uy ppu0

                // $2006: アドレス $3000 をセット
                let ppu2 = Ppu.writeToAddressRegister 0x30uy ppu1
                let ppu3 = Ppu.writeToAddressRegister 0x00uy ppu2

                // $2007: データ書き込み（ここでアドレスが+32進む）
                let ppu4 = Ppu.writeToDataRegister 0xAAuy ppu3

                // v（内部アドレスレジスタ）が$3020になっているか
                Expect.equal ppu4.scroll.v 0x3020us "v should increment by 32"
            }

            test "$2005 twice then $2006 twice sets v and x correctly (NESDev accurate)" {
                // PPUの初期化
                let ppu0 = Ppu.init (testCartridge [||])

                // $2005: Xスクロール（下位3ビットがx, 上位5ビットがcoarse X）
                let ppu1 = Ppu.writeToScrollRegister 0x12uy ppu0
                // $2005: Yスクロール（下位3ビットがfine Y, 上位5ビットがcoarse Y）
                let ppu2 = Ppu.writeToScrollRegister 0x34uy ppu1

                // $2006: アドレス上位（0x56 & 0x3F = 0x16, <<8で0x1600がtにセットされる）
                let ppu3 = Ppu.writeToAddressRegister 0x56uy ppu2
                // $2006: アドレス下位（0x78）
                let ppu4 = Ppu.writeToAddressRegister 0x78uy ppu3

                // === 検証 ===
                // NESDev仕様に基づくと
                //   - $2005 0x12, 0x34 → x = 0x02, tのY部分/fineY/coarseYは0x34でセットされる
                //   - $2006 0x56, 0x78 → t = 0x1600 | 0x78 = 0x1678
                //     最終的にv = t = 0x1678, x = 0x02
                Expect.equal ppu4.scroll.v 0x1678us "v should be set to $1678"
                Expect.equal ppu4.scroll.t 0x1678us "t should be set to $1678"
                Expect.equal ppu2.scroll.x 0x02uy "x scroll should be set to 0x02"
                Expect.isFalse ppu4.scroll.w "w should be false"
            }

            test "alternate $2005/$2006 changes w and x (NESDev accurate)" {
                let ppu0 = Ppu.init (testCartridge [||])

                // $2005: Xスクロール w = false
                let ppu1 = Ppu.writeToScrollRegister 0x16uy ppu0
                // $2006: アドレス w = true
                let ppu2 = Ppu.writeToAddressRegister 0x23uy ppu1
                // $2005: Yスクロール w = false
                let ppu3 = Ppu.writeToScrollRegister 0x2Fuy ppu2
                // $2006: アドレス w = true
                let ppu4 = Ppu.writeToAddressRegister 0x80uy ppu3

                // x: $2005 0x16 & 0x07 = 0x06 -> $2005 0x2F & 0x07 = 0x07
                // t: このような順番だとマスクされて最終的に 0x80 がセットされる
                // v: $2006 への書き込み完了で w ラッチが false になることで 0x80 が t からコピーされる
                Expect.equal ppu4.scroll.x 0x07uy "x scroll should be set to 0x07"
                Expect.equal ppu4.scroll.v 0x80us "v should be set to $80"
                Expect.isFalse ppu4.scroll.w "w should be false"
            }
        ]

        testList "PPU Scroll.v tests" [

            test "coarseX increment" {
                let v = 0us ||| 1us  // coarseX = 1
                let result = incrementCoarseX v
                let expected = v + 1us
                Expect.equal result expected "coarseX should increment"
            }

            test "coarseX wrap and switch horizontal nametable" {
                let v = coarseXMask // coarseX = 31
                let result = incrementCoarseX v
                let expected = 0us ^^^ horizontalNTMask
                Expect.equal result expected "coarseX wrap and horizontal nametable toggle"
            }

            test "fineY increment (fineY < 7)" {
                for fy in 0us .. 6us do
                    let v = fy <<< 12
                    let result = incrementY v
                    let expected = v + 0x1000us
                    Expect.equal result expected $"fineY={fy}: fineY should increment"
            }

            test "fineY wrap and coarseY++" {
                let fineY7 = 7us <<< 12
                let coarseY3 = 3us <<< 5
                let v = fineY7 ||| coarseY3
                let result = incrementY v
                let expected = (v &&& ~~~fineYMask) + 0x0020us
                Expect.equal result expected "fineY wrap and coarseY increment"
            }

            test "coarseY = 29 → wrap and toggle vertical nametable" {
                let fineY7 = 7us <<< 12
                let coarseY29 = 29us <<< 5
                let v = fineY7 ||| coarseY29
                let result = incrementY v
                let expected =
                    ((v &&& ~~~fineYMask) &&& ~~~coarseYMask) ^^^ verticalNTMask
                Expect.equal result expected "coarseY=29 triggers vertical nametable switch"
            }

            test "coarseY = 31 → wrap without toggle" {
                let fineY7 = 7us <<< 12
                let coarseY31 = 31us <<< 5
                let v = fineY7 ||| coarseY31
                let result = incrementY v
                let expected =
                    (v &&& ~~~fineYMask)
                    &&& ~~~coarseYMask
                Expect.equal result expected "coarseY=31 wraps without toggle"
            }

        ]
    ]

