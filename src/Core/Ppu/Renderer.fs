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
                        let color = nesPalette[int sprPal[int value]]

                        let px, py =
                            match flipHorizontal, flipVertical with
                            | false, false -> tileX + x, tileY + y
                            | true, false -> tileX + 7 - x, tileY + y
                            | false, true -> tileX + x, tileY + 7 - y
                            | true, true -> tileX + 7 - x, tileY + 7 - y

                        let pos = py * Frame.width + px

                        if isValidPixel (uint px) (uint py) then
                            let bgIdx = frame.bgPaletteIdx[pos]

                            if spriteOverBackground priority bgIdx then
                                frameAcc <- setSpritePixel (uint px) (uint py) color frameAcc

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

    let renderNameTableScanline
        (ppu: PpuState)
        (snapshots: PpuPublicState)
        (nameTable: byte[])
        (viewPort: Rect)
        (shiftX: int)
        (shiftY: int)
        (scanline: int)
        drawLines
        (frame: Frame)
        : Frame =

        if nameTable.Length < 1 then
            frame
        else

            let attrTable = nameTable[0x3C0..0x3FF]
            let mutable frameAcc = frame

            // スキャンラインに該当する行だけ描画
            for i = 0 to 0x3BF do
                let tileX = i % 32
                let tileY = i / 32
                let y = tileY * 8

                if scanline >= y && scanline < y + drawLines then
                    let bank = Ppu.backgroundPatternAddr snapshots.ctrlPerScanline[scanline] |> int
                    let tileIdx = nameTable[i] |> int

                    let tile =
                        Mapper.ppuReadRange
                            (bank + tileIdx * 16)
                            (bank + tileIdx * 16 + 15)
                            ppu.cartridge
                            ppu.mapperPerScanline[y]

                    let palette = backgroundPalette ppu attrTable tileX tileY

                    for y = 0 to 7 do
                        let upper = tile[y]
                        let lower = tile[y + 8]

                        for x = 0 to 7 do
                            // CHR データの最後尾が画面上の左
                            let bit0 = (upper >>> (7 - x)) &&& 1uy
                            let bit1 = (lower >>> (7 - x)) &&& 1uy
                            let value = (bit1 <<< 1) ||| bit0
                            let rgb = nesPalette[int palette[int value]]
                            let px = tileX * 8 + x
                            let py = tileY * 8 + y

                            if
                                px >= int viewPort.x1 && px < int viewPort.x2 &&
                                py >= int viewPort.y1 && py < int viewPort.y2
                            then
                                frameAcc <-
                                    setBackgroundPixel (uint (shiftX + px)) (uint (shiftY + py)) rgb value frameAcc

            frameAcc


    let getScrollXY (scroll: ScrollRegisters) : int * int =
        let coarseX = int (scroll.v &&& 0b1_1111us)
        let coarseY = int ((scroll.v >>> 5) &&& 0b1_1111us)
        let fineY = int ((scroll.v >>> 12) &&& 0b111us)
        let x = coarseX * 8 + int scroll.x
        let y = coarseY * 8 + fineY
        x, y

    let screenW = Frame.width |> uint
    let screenH = Frame.height |> uint

    let drawLines = 8

    /// 分割スクロールのためのスキャンラインごとの描画
    /// TODO: ネームテーブルが 4 つ表示される場合に対応したい
    let renderScanlineBased (ppu: PpuState) (snapshots: PpuPublicState) (frame: Frame) : Frame =
        let mutable frameAcc = frame
        let n = 240 / drawLines - 1 // スキャンライン一本ずつだと非効率なのである程度まとめて描いてみる

        for y = 0 to n do
            let drawStartY = y * drawLines

            let scrlX, scrlY = snapshots.scrollPerScanline[drawStartY] |> getScrollXY

            let correctedAddr = Ppu.getNameTableAddress snapshots.scrollPerScanline[drawStartY]
            let mainNT, subNTH, subNTV, subNTVH = Ppu.getVisibleNameTables ppu correctedAddr

            let needSubH = scrlX > 0
            let needSubV = scrlY + drawLines > 0

            // 4 画面分の描画範囲
            // rect1 が現在のメイン背景担当
            // パフォーマンスのためには狭めることも必要になるかも
            // +-------+-------+
            // | rect1 | rect2 |
            // +-------+-------+
            // | rect3 | rect4 |
            // +-------+-------+
            let rect1 = initialRect (uint scrlX) (uint scrlY) screenW screenH
            let rect2 = initialRect 0u (uint scrlY) (uint scrlX) screenH
            let rect3 = initialRect (uint scrlX) 0u screenW (uint scrlY)
            let rect4 = initialRect 0u 0u (uint scrlX) (uint scrlY)

            // メイン背景描画
            frameAcc <-
                renderNameTableScanline ppu snapshots mainNT rect1 (-scrlX) (-scrlY) (int drawStartY) drawLines frameAcc
            // サブ背景
            if needSubH then
                frameAcc <-
                    renderNameTableScanline
                        ppu
                        snapshots
                        subNTH
                        rect2
                        (int screenW - scrlX)
                        (-scrlY)
                        (int drawStartY)
                        drawLines
                        frameAcc

            if needSubV then
                frameAcc <-
                    renderNameTableScanline
                        ppu
                        snapshots
                        subNTV
                        rect3
                        (-scrlX)
                        (int screenH - scrlY)
                        (int drawStartY)
                        drawLines
                        frameAcc

            if needSubH && needSubV then
                frameAcc <-
                    renderNameTableScanline
                        ppu
                        snapshots
                        subNTVH
                        rect4
                        (int screenW - scrlX)
                        (int screenH - scrlY)
                        (int drawStartY)
                        drawLines
                        frameAcc

        drawSprites ppu snapshots frameAcc
