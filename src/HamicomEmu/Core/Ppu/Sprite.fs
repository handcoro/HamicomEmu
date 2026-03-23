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

    let private clearSpriteInfo (si: SpriteInfo array) idx =
        si[idx].index <- 0
        si[idx].y <- 0xFFuy
        si[idx].tile <- 0xFFuy
        si[idx].attr <- 0xFFuy
        si[idx].x <- 0xFFuy
        si[idx].tileLo <- 0uy
        si[idx].tileHi <- 0uy

    let clearEvalBuffer ppu =
        let si = ppu.secondarySpritesEval
        for i in 0..si.Length - 1 do
            clearSpriteInfo si i

    let copyPrimaryToEval primaryIdx evalIdx ppu =
        let si = ppu.secondarySpritesEval
        let baseIdx = primaryIdx * 4
        si[evalIdx].index <- baseIdx // スプライト 0 ヒット判定のため
        si[evalIdx].y <- ppu.oam[baseIdx]
        si[evalIdx].tile <- ppu.oam[baseIdx + 1]
        si[evalIdx].attr <- ppu.oam[baseIdx + 2]
        si[evalIdx].x <- ppu.oam[baseIdx + 3]

    let loadEvalTilesInfo idx ppu =
        // 属性ビット展開
        let si = ppu.secondarySpritesEval
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
                let tileBase = (tileIdx &&& 0xFE) <<< 4
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

    /// スキャンライン上でスプライトがある X 座標をマーク（評価バッファ側）
    let updateEvalSpriteExistence idx ppu =
        let si = ppu.secondarySpritesEval
        for xOff in 0..7 do
            let px = int si[idx].x + xOff
            if px >= 0 && px < 256 then
                ppu.hasSpriteEval[px] <- true

    let evaluateForLineToEval line ppu =
        let mutable count = 0
        // OAM: 64 エントリー x 4 バイト
        for i in 0..63 do
            if count < 8 then
                let baseIdx = i * 4
                let y = int ppu.oam[baseIdx]
                let size = if parseSize ppu.ctrl = Mode8x16 then 16 else 8
                // スプライトの Y 座標がスキャンラインにかかっている場合
                if y <= 0xEF && y <= line && line < y + size then
                    copyPrimaryToEval i count ppu
                    count <- count + 1
        count

    /// c=65 で評価フェーズを初期化する
    let initEvalPhase ppu =
        System.Array.Clear(ppu.hasSpriteEval, 0, Screen.width)
        clearEvalBuffer ppu
        ppu.evalPrimaryIdx <- 0
        ppu.evalSecondaryIdx <- 0
        ppu.evalReadData <- 0uy
        ppu.evalOddCycle <- true   // 次は奇数（read）サイクル
        ppu.evalActive <- true
        ppu.evalBytePhase <- 0
        ppu.evalLatchY <- 0uy
        ppu.evalLatchTile <- 0uy
        ppu.evalLatchAttr <- 0uy
        ppu.evalLatchX <- 0uy

    /// c=65-256 で毎サイクル呼ぶ逐次評価ステップ
    /// 奇数サイクル: Primary OAM から Y を読み出し
    /// 偶数サイクル: line hit 判定後、4バイトをラッチして Secondary OAM へ書き込み
    /// NOTE: ここでは Pattern Table フェッチは行わない（A12 位相保護）
    let performEvalCycleStep line ppu =
        if ppu.evalOddCycle then
            // === 奇数サイクル: Primary OAM から Y バイトを読み出す ===
            if ppu.evalPrimaryIdx < 64 then
                let baseIdx = ppu.evalPrimaryIdx * 4
                ppu.evalReadData <- ppu.oam[baseIdx]
        else
            // === 偶数サイクル: line hit を評価し、必要なら 4 バイトを採用 ===
            if ppu.evalPrimaryIdx < 64 then
                let size = if parseSize ppu.ctrl = Mode8x16 then 16 else 8
                if ppu.evalSecondaryIdx < 8 then
                    let y = int ppu.evalReadData

                    if y <= 0xEF && y <= line && line < y + size then
                        let baseIdx = ppu.evalPrimaryIdx * 4
                        ppu.evalLatchY <- ppu.oam[baseIdx]
                        ppu.evalLatchTile <- ppu.oam[baseIdx + 1]
                        ppu.evalLatchAttr <- ppu.oam[baseIdx + 2]
                        ppu.evalLatchX <- ppu.oam[baseIdx + 3]

                        let si = ppu.secondarySpritesEval
                        let idx = ppu.evalSecondaryIdx
                        si[idx].index <- baseIdx
                        si[idx].y <- ppu.evalLatchY
                        si[idx].tile <- ppu.evalLatchTile
                        si[idx].attr <- ppu.evalLatchAttr
                        si[idx].x <- ppu.evalLatchX
                        ppu.evalSecondaryIdx <- idx + 1

                    // 次の Primary OAM エントリーへ（2サイクル/エントリー）
                    ppu.evalPrimaryIdx <- ppu.evalPrimaryIdx + 1
                else
                    // 8 本見つかった後はオーバーフロー判定へ移行
                    // 実機バグを再現するため、評価バイト位相を 0..3 と進める
                    let baseIdx = ppu.evalPrimaryIdx * 4
                    let yLike = int ppu.oam[baseIdx + ppu.evalBytePhase]

                    if yLike <= 0xEF && yLike <= line && line < yLike + size then
                        ppu.status <- setFlag StatusFlags.spriteOverflow ppu.status
                        // 1 スプライト分の検出後は評価バイト位相を戻す
                        ppu.evalPrimaryIdx <- ppu.evalPrimaryIdx + 1
                        ppu.evalBytePhase <- 0
                    else
                        // 検出できなかった場合は評価バイト位相も進める（without carry 相当）
                        ppu.evalPrimaryIdx <- ppu.evalPrimaryIdx + 1
                        ppu.evalBytePhase <- (ppu.evalBytePhase + 1) &&& 0x03
        
        ppu.evalOddCycle <- not ppu.evalOddCycle

    /// c=257 で評価フェーズを終わらせ、結果を render へコミット
    let finalizeEvalPhase ppu =
        ppu.evalActive <- false
        ppu.secondarySpritesEvalCount <- ppu.evalSecondaryIdx

        // Pattern Table フェッチは 257 以降で実施する（65-256 は OAM 評価のみ）
        for i = 0 to ppu.secondarySpritesEvalCount - 1 do
            loadEvalTilesInfo i ppu
            updateEvalSpriteExistence i ppu
        
        // 描画側/評価側を入れ替えてコミットする
        let renderSprites = ppu.secondarySpritesRender
        ppu.secondarySpritesRender <- ppu.secondarySpritesEval
        ppu.secondarySpritesEval <- renderSprites

        let renderHasSprite = ppu.hasSpriteRender
        ppu.hasSpriteRender <- ppu.hasSpriteEval
        ppu.hasSpriteEval <- renderHasSprite

        ppu.secondarySpritesRenderCount <- ppu.secondarySpritesEvalCount
        ppu.secondarySpritesEvalCount <- 0
        ppu.evalPrimaryIdx <- 0
        ppu.evalSecondaryIdx <- 0
        ppu.evalBytePhase <- 0

    let inline getPaletteIndex shift si =
        let t0 = si.tileLo <<< shift &&& 0x80uy >>> 7
        let t1 = si.tileHi <<< shift &&& 0x80uy >>> 6
        let idx = t1 ||| t0 |> int
        idx

    let inline prioritizeOverBackground bgPaletteIndex sprPaletteIndex bgPriority =
        match bgPaletteIndex <> 0, sprPaletteIndex <> 0, bgPriority <> 0uy with
        | false, false, _ -> false // 背景前面 NOTE: 本来は拡張出力
        | false, true, _ -> true // スプライト前面
        | true, false, _ -> false // 背景前面
        | true, true, false -> true // スプライト前面
        | true, true, true -> false // 背景前面
