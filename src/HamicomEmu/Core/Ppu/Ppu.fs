namespace HamicomEmu.Ppu

module Ppu =

    open HamicomEmu.Common.BitUtils
    open HamicomEmu.Cartridge
    open HamicomEmu.Mapper
    open HamicomEmu.Ppu.Types
    open HamicomEmu.Ppu.Common

    let createScroll () = { v = 0us; t = 0us; x = 0uy; w = false }

    let createShiftRegisters () = {
        patternLow = 0us
        patternHigh = 0us
        attrLow = 0us
        attrHigh = 0us
    }

    let createLatches () = {
        tile = 0uy
        attr = 0uy
        patternLow = 0uy
        patternHigh = 0uy
    }

    let initialSpriteInfo = {
        index = 0
        y = 0xFFuy
        tile = 0xFFuy
        attr = 0xFFuy
        x = 0xFFuy
        tileLo = 0uy
        tileHi = 0uy
    }

    let init (cart: Cartridge) = {
        cartridge = cart
        pal = Array.zeroCreate 32 // パレットテーブルは32バイト
        vram = Array.zeroCreate Common.vramSize // 背景情報
        oam = Array.zeroCreate 256 // OAM データは256バイト
        oamAddr = 0uy
        secondarySpritesRender = Array.init 8 (fun _ -> initialSpriteInfo) // 描画側セカンダリ OAM
        secondarySpritesRenderCount = 0
        secondarySpritesEval = Array.init 8 (fun _ -> initialSpriteInfo) // 評価側セカンダリ OAM
        secondarySpritesEvalCount = 0
        hasSpriteRender = Array.create Screen.width false
        hasSpriteEval = Array.create Screen.width false
        scroll = createScroll ()
        ctrl = 0uy // 初期状態では制御レジスタは0
        mask = 0b0001_0000uy
        status = 0uy
        buffer = 0uy
        scanline = 0us
        cycle = 0u
        nmiInterrupt = None
        clearNmiInterrupt = false
        // レンダリング用のバッファ
        workBuffer = Array.zeroCreate (Screen.width * Screen.height)
        frameBuffer = Array.zeroCreate (Screen.width * Screen.height)
        regs = createShiftRegisters ()
        latches = createLatches ()
        frameIsOdd = false
        // 逐次評価状態の初期化
        evalPrimaryIdx = 0
        evalSecondaryIdx = 0
        evalReadData = 0uy
        evalOddCycle = true  // c=65 は奇数サイクルから開始
        evalActive = false
        evalBytePhase = 0
        evalLatchY = 0uy
        evalLatchTile = 0uy
        evalLatchAttr = 0uy
        evalLatchX = 0uy
    }

    let writeToAddressRegister (value: byte) ppu =
        // 2 回に分けて hi/lo を t に書き込む w = false のとき hi
        let w = ppu.scroll.w

        let t =
            if not w then
                (uint16 value &&& 0x3Fus <<< 8) ||| (ppu.scroll.t &&& 0x00FFus)
            else
                (ppu.scroll.t &&& 0xFF00us) ||| uint16 value

        let w' = not w
        // 2 回目の書き込みで v <- t
        let v' = if w' = false then t else ppu.scroll.v

        ppu.scroll.t <- t
        ppu.scroll.w <- w'
        ppu.scroll.v <- v'

        ppu

    /// ネームテーブル情報は t レジスタに移動させる
    let updateControl data ppu =
        let nt = data &&& ControlMasks.nameTable |> uint16
        ppu.scroll.t <- ppu.scroll.t &&& ~~~ScrollMasks.nameTable ||| (nt <<< 10)
        ppu.ctrl <- data &&& ~~~0b11uy
        ppu

    let writeToControlRegister value ppu =
        let beforeNmiStatus = hasFlag ControlFlags.generateNmi ppu.ctrl
        let ppu' = updateControl value ppu
        let afterNmi = hasFlag ControlFlags.generateNmi ppu'.ctrl

        if not beforeNmiStatus && afterNmi && hasFlag StatusFlags.vblank ppu'.status then
            ppu'.nmiInterrupt <- Some 1uy

        ppu'

    let private vramAddressIncrement cr =
        if hasFlag ControlFlags.vramAddIncrement cr then
            32uy
        else
            1uy

    let incrementVramAddress ppu =
        let inc = vramAddressIncrement ppu.ctrl |> uint16
        ppu.scroll.v <- (ppu.scroll.v + inc) &&& 0x3FFFus // 15bit wrap
        ppu

    let paletteAddrToIndex addr = addr &&& 0x1F

    let writePaletteRam addr value ppu =
        let index = paletteAddrToIndex addr
        let mirrored = index |> mirrorPaletteIndex
        let pv = value &&& 0x3Fuy // パレットの色数でマスク

        ppu.pal[index] <- pv
        // 0x10, 0x14, 0x18, 0x1C への書き込みはミラーリングされる
        if index <> mirrored then ppu.pal[mirrored] <- pv

        ppu

    let readFromDataRegister ppu =
        let addr = ppu.scroll.v &&& 0x3FFFus |> int
        // アドレスをインクリメント
        let ppu' = incrementVramAddress ppu
        let mirrored = addr |> mirrorVramAddr ppu'.cartridge.screenMirroring ppu'.cartridge.mapper

        match addr with
        | addr when addr <= 0x1FFF ->
            let result = ppu'.buffer
            let chr = Mapper.ppuRead addr ppu'.vram ppu'.cartridge
            ppu'.buffer <- chr
            result, ppu'

        | addr when addr <= 0x3EFF ->
            let result = ppu'.buffer
            let nt = Mapper.ppuReadNametable mirrored ppu'.vram ppu'.cartridge
            ppu'.buffer <- nt
            result, ppu'

        | addr when addr <= 0x3FFF ->
            // NOTE: 新しい PPU はバッファを介さないらしい？ そしてバッファにはネームテーブルのミラーが入る
            // TODO: 上位 2 bit にオープンバス情報を含める
            let result = ppu'.pal[addr |> paletteAddrToIndex |> mirrorPaletteIndex]
            ppu'.buffer <- ppu'.vram[mirrored]
            result, ppu'

        | _ -> failwithf "Invalid PPU address: %04X" addr

    let writeToDataRegister value ppu =
        let addr = ppu.scroll.v &&& 0x3FFFus |> int
        let ppu' = incrementVramAddress ppu
        let mirrored = addr |> mirrorVramAddr ppu'.cartridge.screenMirroring ppu'.cartridge.mapper

        match addr with
        | addr when addr <= 0x1FFF ->
            let cart = Mapper.ppuWrite addr value ppu'.vram ppu'.cartridge
            ppu'.cartridge <- cart
            ppu'

        | addr when addr <= 0x3EFF ->
            let cart = Mapper.ppuWriteNametable mirrored value ppu'.vram ppu'.cartridge
            ppu'.cartridge <- cart
            ppu'

        | addr when addr <= 0x3FFF ->
            ppu' |> writePaletteRam addr value
        | _ -> failwithf "Invalid PPU address: %04X" addr

    let resetVblankStatus status = clearFlag StatusFlags.vblank status

    /// コントロールレジスタ読み込みでラッチと VBlank が初期化され、次回の NMI が抑制されるらしい
    /// FIXME: 正しくは 2 PPU サイクルの間抑制される？
    let readFromStatusRegister ppu =
        let afterSt = resetVblankStatus ppu.status
        let clearNmi = true

        ppu,
        {
            ppu with
                scroll.w = false
                status = afterSt
                clearNmiInterrupt = clearNmi
        }

    let writeToMaskRegister value ppu =
        ppu.mask <- value
        ppu

    let writeToOamAddress value ppu =
        ppu.oamAddr <- value
        ppu

    let readFromOamData ppu = ppu.oam[int ppu.oamAddr]

    let writeToOamData value ppu =
        ppu.oam[int ppu.oamAddr] <- value
        let nextAddr = ppu.oamAddr + 1uy // 書き込み後インクリメント
        { ppu with oamAddr = nextAddr }

    let writeToOamDma (values: byte[]) ppu = { ppu with oam = values }

    let writeToScrollRegister value ppu =
        // w = false: x (fine X), t[0:4] (coarse X)
        // w = true:  t[5:9] (coarse Y) t[12:14] (fine Y)
        let w = ppu.scroll.w
        let t = ppu.scroll.t

        let x, t' =
            if not w then
                value &&& 0b111uy, (t &&& 0b111_1111_1110_0000us) ||| uint16 (value >>> 3)
            else
                let fineY = uint16 (value &&& 0b111uy) <<< 12
                let coarseY = uint16 (value >>> 3) <<< 5
                ppu.scroll.x, (t &&& 0b000_1100_0001_1111us) ||| coarseY ||| fineY

        let w' = not w
        ppu.scroll.x <- x
        ppu.scroll.t <- t'
        ppu.scroll.w <- w'
        ppu

    let inline isHideBackgroundInLeftmost ppu = hasFlag MaskFlags.showBackgroundInLeftmost ppu.mask |> not

    let inline isHideSpritesInLeftmost ppu = hasFlag MaskFlags.showSpritesInLeftmost ppu.mask |> not

    let inline isRenderingBackgroundEnabled ppu = hasFlag MaskFlags.backgroundRendering ppu.mask
    let inline isRenderingSpritesEnabled ppu = hasFlag MaskFlags.spriteRendering ppu.mask

    /// 簡易実装のための暫定対応
    let inline private spriteFetchPatternBase ppu =
        if hasFlag ControlFlags.spriteSize ppu.ctrl || hasFlag ControlFlags.spritePatternAddress ppu.ctrl then
            0x1000
        else
            0x0000

    let inline private isRenderingEnabled ppu =
        isRenderingBackgroundEnabled ppu || isRenderingSpritesEnabled ppu

    let private swapBuffers ppu =
        let tmp = ppu.frameBuffer
        ppu.frameBuffer <- ppu.workBuffer
        ppu.workBuffer <- tmp
        Array.fill ppu.workBuffer 0 (Screen.width * Screen.height) ppu.pal[0]

    /// PPU を n サイクル進める（1スキャンラインをまたぐときは精密タイミング処理を行う）
    /// tick 内で内部レンダリングを行う
    /// TODO: コードをもうちょっと見やすいように整理する
    let tick ppu =
        let c, s = ppu.cycle, ppu.scanline
        let newCycle = c + 1u
        let mutable skipRest = false

        if c = 0u then
            ppu.cycle <- 1u
            ppu
        else

            if isRenderingEnabled ppu then
                if s < 240us then
                    // === 描画範囲内の処理 ===
                    if c >= 1u && c <= 256u then

                        Background.loadTiles c ppu

                        if c &&& 0x07u = 0u then
                            ppu.scroll.v <- Scroll.incrementCoarseX ppu.scroll.v // Coarse X のインクリメント

                        // === 背景ピクセル描画 ===
                        let x = int c - 1
                        let y = int s

                        let pal = ppu.pal
                        let work = ppu.workBuffer

                        // 背景色を計算（スプライトゼロヒット判定で必要）
                        // レンダリングOFFまたは左端非表示の場合は bgColor = 0（透明）
                        let shouldRenderBackground = 
                            isRenderingBackgroundEnabled ppu 
                            && not (isHideBackgroundInLeftmost ppu && inLeftmostRange x)

                        let bgColor =
                            if shouldRenderBackground then
                                Background.getPaletteIndexFromRegs ppu
                            else
                                0
                        let bgColorMirrored = bgColor |> mirrorTransparentColorIndex

                        if shouldRenderBackground then
                            work[idx x y] <- pal[bgColorMirrored]

                        // === スプライト描画 ===
                        if isHideSpritesInLeftmost ppu && inLeftmostRange x then
                            ()
                        elif isRenderingSpritesEnabled ppu && ppu.hasSpriteRender[x] then
                            // スプライトの描画候補を決める
                            let mutable candidateIndex = -1
                            let mutable candidateColor = 0
                            for i = 0 to ppu.secondarySpritesRenderCount - 1 do
                                if candidateIndex = -1 then
                                    let si = ppu.secondarySpritesRender[i]
                                    let shift = x - int si.x
                                    if si.x < 0xF9uy && shift >= 0 && shift <= 7 then
                                        let spColor = Sprite.getPaletteIndex shift si
                                        if spColor <> 0 then
                                            candidateIndex <- i
                                            candidateColor <- spColor

                            if candidateIndex <> -1 then
                                let si = ppu.secondarySpritesRender[candidateIndex]
                                let spColor = candidateColor
                                let priority = SpriteAttributes.priority si.attr
                                if Sprite.prioritizeOverBackground bgColorMirrored spColor priority then
                                    let palOffset = SpriteAttributes.paletteIndex si.attr <<< 2 ||| 0x10uy |> int // 5 ビット目はスプライトのパレットを表す
                                    work[idx x y] <- pal[spColor + palOffset]
                                // === スプライトゼロヒット ===
                                if si.index = 0 && bgColor <> 0 && spColor <> 0 then
                                    ppu.status <- setFlag StatusFlags.spriteZeroHit ppu.status

                        // === シフト ===
                        Background.shiftRegisters 1 ppu.regs

                    // === 65-256 での逐次スプライト評価（4バイトOAMDATA） ===
                    if c = 65u && isRenderingEnabled ppu then
                        Sprite.initEvalPhase ppu
                    elif c >= 66u && c <= 256u && isRenderingEnabled ppu then
                        Sprite.performEvalCycleStep (int s) ppu

                    // === スキャンラインの終わりに Y をインクリメント ===
                    if c = 256u then
                        ppu.scroll.v <- Scroll.incrementY ppu.scroll.v

                    elif c >= 257u && c <= 320u then
                        // 簡略スプライト評価でも MMC3 の A12 立ち上がり判定が成立するように、
                        // 257-320 のスプライトフェッチ位相を明示的に反映する。
                        Mapper.onPpuFetch (spriteFetchPatternBase ppu) ppu.cartridge.mapper

                        if c = 257u then
                            // v <- t: 水平スクロール位置コピー
                            ppu.scroll.v <- Scroll.getHorizontalPosition ppu.scroll.v ppu.scroll.t
                            // 逐次評価結果を確定し、タイルをロードしてコミット
                            Sprite.finalizeEvalPhase ppu
                    // === 次のスキャンラインの冒頭 2 タイル先読み ===
                    elif c >= 321u && c <= 336u then
                        Background.loadTiles c ppu
                        if c = 328u || c = 336u then
                            Background.shiftRegisters 8 ppu.regs
                            ppu.scroll.v <- Scroll.incrementCoarseX ppu.scroll.v // Coarse X のインクリメント

                // === プリレンダーライン ===
                if s = 261us then
                    // v <- t: 垂直スクロール位置コピー
                    if c >= 280u && c <= 304u then
                        ppu.scroll.v <- Scroll.getVerticalPosition ppu.scroll.v ppu.scroll.t

                    // === 0 番スキャンラインの冒頭 2 タイル先読み ===
                    elif c >= 321u && c <= 336u then
                        Background.loadTiles c ppu
                        if c = 328u || c = 336u then
                            Background.shiftRegisters 8 ppu.regs
                            ppu.scroll.v <- Scroll.incrementCoarseX ppu.scroll.v // Coarse X のインクリメント

                    // === 奇数フレームスキップ（261,339） ===
                    elif c = 339u && ppu.frameIsOdd then
                        skipRest <- true
            // ここからレンダリング状態関係なし
            // === VBlank 開始（scanline 241 開始時）===
            if s = 241us && c = 1u then
                ppu.status <- setFlag StatusFlags.vblank ppu.status

                if hasFlag ControlFlags.generateNmi ppu.ctrl then
                    ppu.nmiInterrupt <- Some 1uy

            // === フラグの初期化：プリレンダーライン開始（scanline 261, cycle 1）===
            if s = 261us && c = 1u then
                ppu.status <-
                    ppu.status
                    |> clearFlag StatusFlags.vblank
                    |> clearFlag StatusFlags.spriteOverflow
                    |> clearFlag StatusFlags.spriteZeroHit

                ppu.nmiInterrupt <- None

            // === 高速パス（スキャンライン内に収まる） ===
            if c < 340u && not skipRest then // 奇数フレームスキップで強制スキャンライン跨ぎへ
                let oamAddr =
                    if c >= 257u && c <= 320u then
                        0uy
                    else
                        ppu.oamAddr

                ppu.cycle <- newCycle
                ppu.oamAddr <- oamAddr

                ppu

            // === スキャンライン跨ぎ ===
            else
                let nextCycle = 0u
                let nextScanline = s + 1us

                let mutable status = ppu.status
                let mutable nmi = ppu.nmiInterrupt
                let mutable newScanline = nextScanline
                let mutable newCycle = nextCycle
                let mutable frameIsOdd = ppu.frameIsOdd

                // === フレーム終了（scanline >= 262）===
                if nextScanline >= 262us then
                    newScanline <- 0us
                    newCycle <- nextCycle
                    frameIsOdd <- not ppu.frameIsOdd
                    // フレームバッファをスワップ
                    swapBuffers ppu

                // === スクロールとコントロールレジスタの記録（描画ラインのみ）===
                ppu.cycle <- newCycle
                ppu.scanline <- newScanline

                ppu.status <- status
                ppu.nmiInterrupt <- nmi
                ppu.frameIsOdd <- frameIsOdd

                ppu

    let rec tickN n ppu =
        if n <= 0u then
            ppu
        else
            let ppu' = tick ppu
            tickN (n - 1u) ppu'
