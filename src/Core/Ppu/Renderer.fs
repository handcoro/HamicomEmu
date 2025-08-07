namespace HamicomEmu.Ppu

module Renderer =

    open HamicomEmu.Common.BitUtils
    open HamicomEmu.Ppu.Screen
    open HamicomEmu.Ppu.Palette
    open HamicomEmu.Ppu.Types
    open HamicomEmu.Ppu
    open HamicomEmu.Mapper

    /// 画面に見える範囲
    type Rect = {
        x1: uint
        y1: uint
        x2: uint
        y2: uint
    }

    /// 画面に見える範囲の指定
    let initialRect x1 y1 x2 y2 = { x1 = x1; y1 = y1; x2 = x2; y2 = y2 }

    let backgroundPalette ppu (attrTable: byte[]) tileCol tileRow =
        let attrTableIdx = tileRow / 4 * 8 + tileCol / 4
        let attrByte = attrTable[attrTableIdx]

        let paletteIdx =
            match tileCol % 4 / 2, tileRow % 4 / 2 with
            | 0, 0 -> attrByte &&& 0b11uy
            | 1, 0 -> (attrByte >>> 2) &&& 0b11uy
            | 0, 1 -> (attrByte >>> 4) &&& 0b11uy
            | 1, 1 -> (attrByte >>> 6) &&& 0b11uy
            | _, _ -> failwith "should not happen"

        let paletteStart = 1 + int paletteIdx * 4

        [|
            ppu.pal[0]
            ppu.pal[paletteStart]
            ppu.pal[paletteStart + 1]
            ppu.pal[paletteStart + 2]
        |]

    let spritePalette ppu idx =
        let start = 0x11 + int idx * 4
        [| 0uy; ppu.pal[start]; ppu.pal[start + 1]; ppu.pal[start + 2] |]

    type SpriteSize =
        | Mode8x8
        | Mode8x16

    let spriteOverBackground priority backgroundPaletteIndex =
        match priority, backgroundPaletteIndex with
        | 0uy, _ -> true // スプライトが前面
        | 1uy, 0uy -> true // 背景がパレット 0（透明）ならスプライト
        | _ -> false // それ以外は背景

    /// 画面左端 8 ピクセルの位置
    let inline inLeftmostRange x = x >= 0 && x <= 7

    let inline isHideSpritesInLeftmostAndInRange x ppu = Ppu.isHideSpritesInLeftmost ppu && inLeftmostRange x

    let inline isHideBackgroundInLeftmostAndInRange x ppu = Ppu.isHideBackgroundInLeftmost ppu && inLeftmostRange x

    /// タイル指定のスプライト描画
    let drawSpriteTile ppu snapshots tileStart tileX tileY attr frame =
        if tileY >= 240 then
            frame
        else
            let mutable frameAcc = frame

            let flipVertical = SpriteAttributes.flipVertical attr
            let flipHorizontal = SpriteAttributes.flipHorizontal attr

            let priority = SpriteAttributes.priority attr
            let paletteIdx = SpriteAttributes.paletteIndex attr
            let sprPal = spritePalette ppu paletteIdx

            let tile = Mapper.ppuReadRange tileStart (tileStart + 15) ppu.cartridge snapshots.mapperPerScanline[tileY]

            for y = 0 to 7 do
                let upper = tile[y]
                let lower = tile[y + 8]

                for x = 0 to 7 do
                    let bit0 = (upper >>> (7 - x)) &&& 1uy
                    let bit1 = (lower >>> (7 - x)) &&& 1uy
                    let value = (bit1 <<< 1) ||| bit0

                    if value <> 0uy then
                        let rgb = nesPalette[int sprPal[int value]]

                        let px, py =
                            match flipHorizontal, flipVertical with
                            | false, false -> tileX + x, tileY + y
                            | true, false -> tileX + 7 - x, tileY + y
                            | false, true -> tileX + x, tileY + 7 - y
                            | true, true -> tileX + 7 - x, tileY + 7 - y

                        if
                            isValidPixel (uint px) (uint py) &&
                            not(isHideSpritesInLeftmostAndInRange px ppu)
                        then
                            let pos = py * Frame.width + px
                            let bgIdx = frame.bgPaletteIdx[pos]

                            if spriteOverBackground priority bgIdx then
                                frameAcc <- setSpritePixel (uint px) (uint py) rgb frameAcc

            frameAcc

    let swap (left: 'a byref) (right: 'a byref) =
        let temp = left
        left <- right
        right <- temp

    /// 全スプライト描画
    let drawSprites (ppu: PpuState) snapshots (frame: Frame) : Frame =
        let mutable frameAcc = frame

        let mode =
            if hasFlag ControlFlags.spriteSize ppu.ctrl then
                Mode8x16
            else
                Mode8x8

        for i = ppu.oam.Length - 4 downto 0 do
            if i % 4 = 0 then // スプライトごとに4バイト
                let tileIdx = ppu.oam[i + 1] |> int
                let tileX = ppu.oam[i + 3] |> int
                let tileY = ppu.oam[i] + 1uy |> int // スプライトは 1 スキャンライン分遅れて表示される

                let attr = ppu.oam[i + 2]

                if tileY < 240 then
                    match mode with
                    | Mode8x8 ->
                        let bank = Ppu.spritePatternAddr snapshots.ctrlPerScanline[tileY] |> int
                        let tileStart = bank + tileIdx * 0x10
                        frameAcc <- drawSpriteTile ppu snapshots tileStart tileX tileY attr frameAcc

                    | Mode8x16 ->
                        let bank = (tileIdx &&& 1) * 0x1000
                        let flipVertical = SpriteAttributes.flipVertical attr
                        let mutable tileTopStart = bank + (tileIdx &&& 0xFE) * 0x10
                        let mutable tileBottomStart = tileTopStart + 0x10
                        // 8x16 スプライトモードの上下反転は上下のスプライトも入れ替える
                        if flipVertical then
                            swap &tileTopStart &tileBottomStart

                        frameAcc <-
                            frameAcc
                            |> drawSpriteTile ppu snapshots tileTopStart tileX tileY attr
                            |> drawSpriteTile ppu snapshots tileBottomStart tileX (tileY + 8) attr

        frameAcc

    let renderFrame ppu =
        ppu.frameBuffer |> Array.map (fun idx -> nesPalette[int idx])
