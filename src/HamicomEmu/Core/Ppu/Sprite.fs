namespace HamicomEmu.Ppu

module Sprite =

    open HamicomEmu.Common.BitUtils
    open HamicomEmu.Mapper
    open HamicomEmu.Ppu.Common
    open HamicomEmu.Ppu.Types

    type Size =
        | Mode8x8
        | Mode8x16

    let parseSize ctrl =
        if hasFlag ControlFlags.spriteSize ctrl then
            Mode8x16
        else
            Mode8x8

    let parsePatternAddr ctrl =
        if
            not (hasFlag ControlFlags.spriteSize ctrl)
            && hasFlag ControlFlags.spritePatternAddress ctrl
        then
            0x1000us
        else
            0x0000us

    let loadTilesInfo idx ppu =
        // 属性ビット展開
        let si = ppu.secondarySprites
        let horiMirror = SpriteAttributes.flipHorizontal si[idx].attr
        let vertMirror = SpriteAttributes.flipVertical si[idx].attr

        let size = parseSize ppu.ctrl

        // 行オフセット計算
        let offset = byte ppu.scanline - si[idx].y |> int
        let height = if size = Mode8x16 then 16 else 8
        let lineOffset =
            if vertMirror then
                height - 1 - offset 
            else
                offset

        // パターンテーブルアドレス計算
        let tileIdx = int si[idx].tile
        let tileAddr =
            match size with
            | Mode8x16 ->
                let tableBase = if tileIdx &&& 0x01 <> 0 then 0x1000 else 0x0000 // 8x16 の場合は 0 ビット目がタイルのバンク
                let tileBase  = (tileIdx &&& 0xFE) <<< 4
                tableBase + tileBase + if lineOffset >= 8 then lineOffset + 8 else lineOffset
            | Mode8x8 ->
                (tileIdx <<< 4)
                + int (parsePatternAddr ppu.ctrl)
                + lineOffset


        Mapper.onPpuFetch tileAddr ppu.cartridge.mapper
        Mapper.onPpuFetch (tileAddr + 8) ppu.cartridge.mapper

        // VRAM 読み出し
        let lo = Mapper.ppuRead tileAddr ppu.vram ppu.cartridge
        let hi = Mapper.ppuRead (tileAddr + 8) ppu.vram ppu.cartridge

        // 反転を考慮
        si[idx].tileLo <- if horiMirror then reverseBits lo else lo
        si[idx].tileHi <- if horiMirror then reverseBits hi else hi


    /// スキャンライン上でスプライトがある X 座標をマーク
    let updateSpriteExistence idx ppu =
        let si = ppu.secondarySprites
        for xOff in 0..7 do
            let px = int si[idx].x + xOff
            if px >= 0 && px < 256 then
                ppu.hasSprite[px] <- true

    let evaluateForLine line ppu =
        let mutable count = 0
        let si = ppu.secondarySprites
        // OAM: 64 エントリー x 4 バイト
        for i in 0..63 do
            if count < 8 then
                let baseIdx = i * 4
                let y = int ppu.oam[baseIdx]
                let size = if parseSize ppu.ctrl = Mode8x16 then 16 else 8
                // スプライトの Y 座標がスキャンラインにかかっている場合
                if y <= 0xEF && y <= line && line < y + size then
                    si[count].index <- baseIdx // スプライト 0 ヒット判定のため
                    si[count].y     <- ppu.oam[baseIdx]
                    si[count].tile  <- ppu.oam[baseIdx + 1]
                    si[count].attr  <- ppu.oam[baseIdx + 2]
                    si[count].x     <- ppu.oam[baseIdx + 3]
                    count <- count + 1
        count

    let inline getPaletteIndex shift si =
        let t0 = si.tileLo <<< shift &&& 0x80uy >>> 7
        let t1 = si.tileHi <<< shift &&& 0x80uy >>> 6
        let idx = t1 ||| t0 |> int
        idx

    let inline prioritizeOverBackground bgPaletteIndex sprPaletteIndex bgPriority =
        match bgPaletteIndex <> 0, sprPaletteIndex <> 0, bgPriority <> 0uy with
        | false, false, _ -> false // 背景前面 NOTE: 本来は拡張出力
        | false,  true, _ -> true // スプライト前面
        |  true, false, _ -> false // 背景前面
        |  true,  true, false -> true // スプライト前面
        |  true,  true, true -> false // 背景前面
