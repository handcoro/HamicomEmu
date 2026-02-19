module PpuTests

open Expecto
open HamicomEmu.Common.BitUtils
open HamicomEmu.Bus
open HamicomEmu.Ppu.Scroll
open HamicomEmu.Ppu
open HamicomEmu.Cartridge

let ppuTests =
    testList "PPU Tests" [
        test "PPU Register Test" {
            let ppuAddr = 0x2000us
            let value = 0x77uy

            let bus0 = Bus.init (testCartridge [||])
            let mutable ppu = bus0.ppu

            // コントロールレジスタ設定
            ppu <- Ppu.writeToControlRegister 0x00uy ppu
            // アドレス設定
            ppu <- Ppu.writeToAddressRegister 0x20uy ppu
            ppu <- Ppu.writeToAddressRegister 0x00uy ppu

            // $2007 に書き込み
            ppu <- Ppu.writeToDataRegister value ppu

            // 再びアドレス設定（VRAM 読みは1バイトずれている可能性あり）
            ppu <- Ppu.writeToAddressRegister 0x20uy ppu
            ppu <- Ppu.writeToAddressRegister 0x00uy ppu

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

let ppuScrollTests =
    testList "PPU Scroll tests" [

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