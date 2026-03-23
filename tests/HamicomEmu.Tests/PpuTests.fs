module PpuTests

open Expecto
open HamicomEmu.Bus
open HamicomEmu.Cartridge
open HamicomEmu.Common.BitUtils
open HamicomEmu.Mapper
open HamicomEmu.Mapper.Common
open HamicomEmu.Mapper.Types
open HamicomEmu.Ppu.Scroll
open HamicomEmu.Ppu
open TestHelpers

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

        // $2000: generateNmi の立ち上がりで vblank 中は NMI が発生するか
        test "writeToControlRegister triggers NMI on rising edge during vblank" {
            let ppu = Ppu.init (testCartridge [||])
            ppu.status <- setFlag StatusFlags.vblank ppu.status

            let ppu' = Ppu.writeToControlRegister ControlFlags.generateNmi ppu
            Expect.equal ppu'.nmiInterrupt (Some 1uy) "nmiInterrupt should be set on rising edge"
        }

        // $2000: vblank ではない場合は NMI が発生しないか
        test "writeToControlRegister does not trigger NMI outside vblank" {
            let ppu = Ppu.init (testCartridge [||])
            ppu.status <- clearFlag StatusFlags.vblank ppu.status

            let ppu' = Ppu.writeToControlRegister ControlFlags.generateNmi ppu
            Expect.isNone ppu'.nmiInterrupt "nmiInterrupt should remain None outside vblank"
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

        test "readFromDataRegister updates buffer and increments v" {
            let ppu0 = Ppu.init (testCartridge [||])

            // $2000 を指すように設定
            let ppu1 = Ppu.writeToAddressRegister 0x20uy ppu0
            let ppu2 = Ppu.writeToAddressRegister 0x00uy ppu1

            // 事前に VRAM に値を投入
            let ppu3 = Ppu.writeToDataRegister 0x5Auy ppu2

            // $2000 を再設定（読み出しのバッファ動作を検証）
            let ppu4 = Ppu.writeToAddressRegister 0x20uy ppu3
            let ppu5 = Ppu.writeToAddressRegister 0x00uy ppu4

            let buffered, ppu6 = Ppu.readFromDataRegister ppu5
            let actual, ppu7 = Ppu.readFromDataRegister ppu6

            // 1回目はバッファ値、2回目が実データ
            Expect.equal buffered 0uy "first read should return buffered value"
            Expect.equal actual 0x5Auy "second read should return VRAM value"

            // v がインクリメントされていることを確認
            Expect.equal ppu7.scroll.v 0x2002us "v should increment by 1 twice"
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

            // 後続の書き込みで変更される前に退避
            let vAtSet = ppu4.scroll.v
            let tAtSet = ppu4.scroll.t
            let wAtSet = ppu4.scroll.w
            let xAtSet = ppu2.scroll.x

            // $2007: VRAM（$2000）へ書き込み
            let ppu5 = Ppu.writeToDataRegister 0xABuy ppu4

            // 再び $2000 をアドレスセット（オートインクリメント対策）
            let ppu6 = Ppu.writeToAddressRegister 0x20uy ppu5
            let ppu7 = Ppu.writeToAddressRegister 0x00uy ppu6

            // $2007: VRAMから2回読み込み（1回目はバッファ、2回目が本体の値）
            let _, ppu8 = Ppu.readFromDataRegister ppu7  // バッファ値（無視）
            let value, _ = Ppu.readFromDataRegister ppu8 // 実値

            // アドレスやスクロールレジスタもついでに確認
            Expect.equal vAtSet 0x2000us "v should be set to 0x2000"
            Expect.equal tAtSet 0x2000us "t should be set to 0x2000"
            Expect.equal xAtSet 0x03uy "x scroll should be set to 0x03"
            Expect.isFalse wAtSet "w should be false after second write"

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

        test "sprite evaluation clears stale secondary OAM entries" {
            let ppu = Ppu.init (testCartridge [||])

            // スキャンライン先頭へ移動（1-64 の secondary OAM clear も通す）
            ppu.scanline <- 10us
            ppu.cycle <- 0u

            // 1つだけ有効なスプライトを OAM に配置
            ppu.oam[0] <- 10uy
            ppu.oam[1] <- 0x22uy
            ppu.oam[2] <- 0x00uy
            ppu.oam[3] <- 20uy

            // 前のラインのゴミを模擬
            ppu.secondarySpritesRender[1].y <- 3uy
            ppu.secondarySpritesRender[1].tile <- 4uy
            ppu.secondarySpritesRender[1].attr <- 5uy
            ppu.secondarySpritesRender[1].x <- 6uy

            let mutable ppu' = ppu
            for _ in 1..260 do
                ppu' <- Ppu.tick ppu'

            Expect.equal ppu'.secondarySpritesRenderCount 1 "one sprite should be selected"
            Expect.equal ppu'.secondarySpritesRender[0].y 10uy "first selected sprite should remain"
            Expect.equal ppu'.secondarySpritesRender[1].y 0xFFuy "stale sprite data should be cleared"
            Expect.equal ppu'.secondarySpritesRender[1].tile 0xFFuy "stale sprite tile should be cleared"
            Expect.equal ppu'.secondarySpritesRender[1].attr 0xFFuy "stale sprite attr should be cleared"
            Expect.equal ppu'.secondarySpritesRender[1].x 0xFFuy "stale sprite x should be cleared"
        }

        test "visible phase does not mutate secondary OAM before cycle 257" {
            let ppu = Ppu.init (testCartridge [||])

            ppu.scanline <- 20us
            ppu.cycle <- 1u
            ppu.secondarySpritesRenderCount <- 1
            ppu.secondarySpritesRender[0].index <- 0
            ppu.secondarySpritesRender[0].y <- 20uy
            ppu.secondarySpritesRender[0].tile <- 0x12uy
            ppu.secondarySpritesRender[0].attr <- 0x01uy
            ppu.secondarySpritesRender[0].x <- 40uy
            ppu.secondarySpritesRender[0].tileLo <- 0xAAuy
            ppu.secondarySpritesRender[0].tileHi <- 0x55uy

            let mutable ppu' = ppu
            for _ in 1..200 do
                ppu' <- Ppu.tick ppu'

            Expect.equal ppu'.cycle 201u "test should stay before cycle 257"
            Expect.equal ppu'.secondarySpritesRenderCount 1 "sprite count should be preserved during visible phase"
            Expect.equal ppu'.secondarySpritesRender[0].tile 0x12uy "sprite tile should not be cleared mid-line"
            Expect.equal ppu'.secondarySpritesRender[0].x 40uy "sprite x should remain intact"
            Expect.equal ppu'.secondarySpritesRender[0].tileLo 0xAAuy "prefetched low tile byte should remain intact"
            Expect.equal ppu'.secondarySpritesRender[0].tileHi 0x55uy "prefetched high tile byte should remain intact"
        }

        test "MMC3 A12 rising edge clocks IRQ counter" {
            let prg = Array.create (8 * 1024 * 4) 0uy
            let chr = Array.create (1024 * 8) 0uy
            let mmc3 = Mmc3.init prg chr Vertical

            Mmc3.cpuWrite 0xC000 2uy prg chr mmc3 |> ignore
            Mmc3.cpuWrite 0xC001 0uy prg chr mmc3 |> ignore
            Mmc3.cpuWrite 0xE001 0uy prg chr mmc3 |> ignore

            let pulseA12 () =
                for _ in 1..8 do
                    Mmc3.onPpuFetch 0x0000 mmc3
                Mmc3.onPpuFetch 0x1000 mmc3

            pulseA12 () // reload -> 2
            pulseA12 () // 2 -> 1
            pulseA12 () // 1 -> 0 and pending

            Expect.isTrue (Mmc3.pollIrq mmc3) "IRQ should be pending after third qualified rising edge"
        }

        test "MMC3 ignores A12 rise without enough low cycles" {
            let prg = Array.create (8 * 1024 * 4) 0uy
            let chr = Array.create (1024 * 8) 0uy
            let mmc3 = Mmc3.init prg chr Vertical

            Mmc3.cpuWrite 0xC000 1uy prg chr mmc3 |> ignore
            Mmc3.cpuWrite 0xC001 0uy prg chr mmc3 |> ignore
            Mmc3.cpuWrite 0xE001 0uy prg chr mmc3 |> ignore

            for _ in 1..7 do
                Mmc3.onPpuFetch 0x0000 mmc3

            Mmc3.onPpuFetch 0x1000 mmc3

            Expect.isFalse (Mmc3.pollIrq mmc3) "A12 low duration below threshold must not clock IRQ counter"
        }

        test "MMC3 reload value 0 can still assert IRQ on qualified edge" {
            let prg = Array.create (8 * 1024 * 4) 0uy
            let chr = Array.create (1024 * 8) 0uy
            let mmc3 = Mmc3.init prg chr Vertical

            Mmc3.cpuWrite 0xC000 0uy prg chr mmc3 |> ignore
            Mmc3.cpuWrite 0xC001 0uy prg chr mmc3 |> ignore
            Mmc3.cpuWrite 0xE001 0uy prg chr mmc3 |> ignore

            for _ in 1..8 do
                Mmc3.onPpuFetch 0x0000 mmc3

            Mmc3.onPpuFetch 0x1000 mmc3

            Expect.isTrue (Mmc3.pollIrq mmc3) "reload=0 should be able to assert IRQ when edge is qualified"
        }

        test "sprite evaluation scans all 64 Primary OAM entries within c=66-256" {
            let ppu = Ppu.init (testCartridge [||])

            // 64個全てのエントリにスプライトを配置（全てライン範囲外にして不一致にする）
            for i in 0..63 do
                ppu.oam[i * 4] <- 240uy  // Y: すべてライン範囲外

            ppu.scanline <- 10us
            ppu.cycle <- 0u

            // c=1 から c=260 までティック（評価フェーズ全体をカバー）
            let mutable ppu' = ppu
            for _ in 1..260 do
                ppu' <- Ppu.tick ppu'

            // 64個全てのエントリが評価されたが、どれもライン一致しなかった
            Expect.equal ppu'.secondarySpritesRenderCount 0 "no sprites should match when all are out of range"
            // evalPrimaryIdxが64を超えていない（配列外参照が起きていない）
            Expect.isTrue (ppu'.evalPrimaryIdx <= 64) "evalPrimaryIdx should not exceed 64 (boundary protection)"
        }

        test "sprite evaluation copies all 4 bytes correctly on line match" {
            let ppu = Ppu.init (testCartridge [||])

            // スプライトを3つ配置（全て同じラインに一致させる）
            ppu.oam[0 * 4 + 0] <- 10uy   // Y
            ppu.oam[0 * 4 + 1] <- 0xAAuy // Tile
            ppu.oam[0 * 4 + 2] <- 0xBBuy // Attr
            ppu.oam[0 * 4 + 3] <- 20uy   // X

            ppu.oam[1 * 4 + 0] <- 10uy
            ppu.oam[1 * 4 + 1] <- 0xCCuy
            ppu.oam[1 * 4 + 2] <- 0xDDuy
            ppu.oam[1 * 4 + 3] <- 30uy

            ppu.oam[2 * 4 + 0] <- 10uy
            ppu.oam[2 * 4 + 1] <- 0xEEuy
            ppu.oam[2 * 4 + 2] <- 0xFFuy
            ppu.oam[2 * 4 + 3] <- 40uy

            ppu.scanline <- 15us  // Y=10, size=8 → line 10..17 のため15は範囲内
            ppu.cycle <- 0u

            let mutable ppu' = ppu
            for _ in 1..260 do
                ppu' <- Ppu.tick ppu'

            Expect.equal ppu'.secondarySpritesRenderCount 3 "three sprites should match"
            
            // 1つ目のスプライト: Y/Tile/Attr/X 全て正しくコピーされている
            Expect.equal ppu'.secondarySpritesRender[0].y 10uy "first sprite Y should be copied"
            Expect.equal ppu'.secondarySpritesRender[0].tile 0xAAuy "first sprite Tile should be copied"
            Expect.equal ppu'.secondarySpritesRender[0].attr 0xBBuy "first sprite Attr should be copied"
            Expect.equal ppu'.secondarySpritesRender[0].x 20uy "first sprite X should be copied"

            // 2つ目のスプライト
            Expect.equal ppu'.secondarySpritesRender[1].y 10uy "second sprite Y should be copied"
            Expect.equal ppu'.secondarySpritesRender[1].tile 0xCCuy "second sprite Tile should be copied"
            Expect.equal ppu'.secondarySpritesRender[1].attr 0xDDuy "second sprite Attr should be copied"
            Expect.equal ppu'.secondarySpritesRender[1].x 30uy "second sprite X should be copied"

            // 3つ目のスプライト
            Expect.equal ppu'.secondarySpritesRender[2].y 10uy "third sprite Y should be copied"
            Expect.equal ppu'.secondarySpritesRender[2].tile 0xEEuy "third sprite Tile should be copied"
            Expect.equal ppu'.secondarySpritesRender[2].attr 0xFFuy "third sprite Attr should be copied"
            Expect.equal ppu'.secondarySpritesRender[2].x 40uy "third sprite X should be copied"
        }

        test "Pattern Table fetch does not occur during c=66-256 (MMC3 A12 protection)" {
            let prg = Array.create (8 * 1024 * 4) 0uy
            let chr = Array.create (1024 * 8) 0uy
            let mmc3 = Mmc3.init prg chr Vertical

            // MMC3 IRQ カウンタを reload=0 に設定（敏感にIRQ発生する状態）
            Mmc3.cpuWrite 0xC000 0uy prg chr mmc3 |> ignore
            Mmc3.cpuWrite 0xC001 0uy prg chr mmc3 |> ignore
            Mmc3.cpuWrite 0xE001 0uy prg chr mmc3 |> ignore

            let cart = {
                prgRom = prg
                chrRom = chr
                chrRam = [||]
                mapper = MMC3 mmc3
                screenMirroring = Vertical
            }
            let ppu = Ppu.init cart

            // スプライトを1つ配置（ライン一致させる）
            ppu.oam[0] <- 10uy
            ppu.oam[1] <- 0x12uy
            ppu.oam[2] <- 0x00uy
            ppu.oam[3] <- 20uy

            ppu.scanline <- 15us
            ppu.cycle <- 0u

            // c=1 から c=256 まで実行（評価フェーズ全体）
            let mutable ppu' = ppu
            for _ in 1..256 do
                ppu' <- Ppu.tick ppu'

            // c=66-256 中は Pattern Table フェッチが行われていないため、
            // MMC3 IRQ カウンタは減らない（A12 rising edge が発生していない）
            Expect.isFalse (Mmc3.pollIrq mmc3) "IRQ should not be asserted during c=66-256 (confirms no Pattern Table fetch during sprite evaluation)"
        }

        test "sprite overflow flag is set when 9th in-range sprite exists" {
            let ppu = Ppu.init (testCartridge [||])

            // 9 本を同一ラインに配置
            for i in 0..8 do
                ppu.oam[i * 4 + 0] <- 10uy
                ppu.oam[i * 4 + 1] <- byte i
                ppu.oam[i * 4 + 2] <- 0uy
                ppu.oam[i * 4 + 3] <- byte (16 + i)

            ppu.scanline <- 15us
            ppu.cycle <- 0u

            let mutable ppu' = ppu
            for _ in 1..260 do
                ppu' <- Ppu.tick ppu'

            Expect.isTrue (hasFlag StatusFlags.spriteOverflow ppu'.status) "sprite overflow flag should be set when more than 8 sprites are in range"
            Expect.equal ppu'.secondarySpritesRenderCount 8 "only first 8 sprites should be kept in secondary OAM"
        }

        test "sprite overflow diagonal scan can cause false positive" {
            let ppu = Ppu.init (testCartridge [||])

            // 最初の 8 本だけが本来の in-range
            for i in 0..7 do
                ppu.oam[i * 4 + 0] <- 10uy
                ppu.oam[i * 4 + 1] <- byte i
                ppu.oam[i * 4 + 2] <- 0uy
                ppu.oam[i * 4 + 3] <- byte (16 + i)

            // 9 本目は out-of-range
            ppu.oam[8 * 4 + 0] <- 240uy
            ppu.oam[8 * 4 + 1] <- 0uy
            ppu.oam[8 * 4 + 2] <- 0uy
            ppu.oam[8 * 4 + 3] <- 0uy

            // 10 本目も Y は out-of-range だが、Tile を in-range 値にする
            // 斜め進みで OAM[n+1][1] が Y として評価されると false positive になる
            ppu.oam[9 * 4 + 0] <- 240uy
            ppu.oam[9 * 4 + 1] <- 10uy
            ppu.oam[9 * 4 + 2] <- 0uy
            ppu.oam[9 * 4 + 3] <- 0uy

            ppu.scanline <- 15us
            ppu.cycle <- 0u

            let mutable ppu' = ppu
            for _ in 1..260 do
                ppu' <- Ppu.tick ppu'

            Expect.equal ppu'.secondarySpritesRenderCount 8 "secondary OAM should still keep only 8 sprites"
            Expect.isTrue (hasFlag StatusFlags.spriteOverflow ppu'.status) "diagonal scan should be able to set overflow from non-Y bytes"
        }

        test "sprite overflow flag is cleared at pre-render line cycle 1" {
            let ppu = Ppu.init (testCartridge [||])
            ppu.status <- setFlag StatusFlags.spriteOverflow ppu.status
            ppu.scanline <- 261us
            ppu.cycle <- 1u

            let ppu' = Ppu.tick ppu

            Expect.isFalse (hasFlag StatusFlags.spriteOverflow ppu'.status) "sprite overflow should be cleared at pre-render line cycle 1"
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