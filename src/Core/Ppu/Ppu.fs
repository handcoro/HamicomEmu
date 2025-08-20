namespace HamicomEmu.Ppu

module Ppu =

    open HamicomEmu.Common.BitUtils
    open HamicomEmu.Cartridge
    open HamicomEmu.Mapper
    open HamicomEmu.Ppu.Types
    open HamicomEmu.Ppu.Common

    let initialScroll = { v = 0us; t = 0us; x = 0uy; w = false }

    let initialShiftRegisters = {
        patternLow = 0us
        patternHigh = 0us
        attrLow = 0us
        attrHigh = 0us
    }

    let initialLatches = {
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
        vram = Array.zeroCreate 0x2000 // PPU VRAM は8KB
        oam = Array.zeroCreate 256 // OAM データは256バイト
        oamAddr = 0uy
        secondarySprites = Array.init 8 (fun _ -> initialSpriteInfo) // セカンダリ OAM
        secondarySpritesCount = 0
        hasSprite = Array.create Screen.width false
        scroll = initialScroll
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
        regs = initialShiftRegisters
        latches = initialLatches
        frameIsOdd = false
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

        let ppu = {
            ppu with
                scroll.t = t
                scroll.w = w'
                scroll.v = v'
        }

        ppu

    /// ネームテーブル情報は t レジスタに移動させる
    let updateControl data ppu =
        let nt = data &&& ControlMasks.nameTable |> uint16
        let t = ppu.scroll.t &&& ~~~ScrollMasks.nameTable ||| (nt <<< 10)
        let ctrl = data &&& ~~~0b11uy
        { ppu with scroll.t = t; ctrl = ctrl }

    let writeToControlRegister value ppu =
        let beforeNmiStatus = hasFlag ControlFlags.generateNmi ppu.ctrl
        let ppu' = updateControl value ppu
        let afterNmi = hasFlag ControlFlags.generateNmi ppu'.ctrl

        if not beforeNmiStatus && afterNmi && hasFlag StatusFlags.vblank ppu'.status then
            { ppu' with nmiInterrupt = Some 1uy }
        else
            ppu'

    let private vramAddressIncrement cr =
        if hasFlag ControlFlags.vramAddIncrement cr then
            32uy
        else
            1uy

    let incrementVramAddress ppu =
        let inc = vramAddressIncrement ppu.ctrl |> uint16
        let v' = (ppu.scroll.v + inc) &&& 0x3FFFus // 15bit wrap
        { ppu with scroll.v = v' }

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
            let chr = Mapper.ppuRead addr ppu'.cartridge
            result, { ppu' with buffer = chr }

        | addr when addr <= 0x3EFF ->
            let result = ppu'.buffer

            result,
            { ppu' with buffer = ppu'.vram[mirrored] }

        | addr when addr <= 0x3FFF ->
            // NOTE: 新しい PPU はバッファを介さないらしい？ そしてバッファにはネームテーブルのミラーが入る
            // TODO: 上位 2 bit にオープンバス情報を含める
            let result = ppu'.pal[addr |> paletteAddrToIndex |> mirrorPaletteIndex]

            result,
            { ppu' with buffer = ppu'.vram[mirrored] }
        | _ -> failwithf "Invalid PPU address: %04X" addr

    let writeToDataRegister value ppu =
        let addr = ppu.scroll.v &&& 0x3FFFus |> int
        let ppu' = incrementVramAddress ppu
        let mirrored = addr |> mirrorVramAddr ppu'.cartridge.screenMirroring ppu'.cartridge.mapper

        match addr with
        | addr when addr <= 0x1FFF ->
            let cart = Mapper.ppuWrite addr value ppu'.cartridge
            { ppu' with cartridge = cart }

        | addr when addr <= 0x3EFF ->
            ppu'.vram[mirrored] <- value
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

    let writeToMaskRegister value ppu = { ppu with mask = value }

    let writeToOamAddress value ppu = { ppu with oamAddr = value }

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

        {
            ppu with
                scroll.x = x
                scroll.t = t'
                scroll.w = w'
        }

    let inline isHideBackgroundInLeftmost ppu = hasFlag MaskFlags.showBackgroundInLeftmost ppu.mask |> not

    let inline isHideSpritesInLeftmost ppu = hasFlag MaskFlags.showSpritesInLeftmost ppu.mask |> not

    let inline isRenderingBackgroundEnabled ppu = hasFlag MaskFlags.backgroundRendering ppu.mask
    let inline isRenderingSpritesEnabled ppu = hasFlag MaskFlags.spriteRendering ppu.mask

    let inline private isRenderingEnabled ppu =
        isRenderingBackgroundEnabled ppu || isRenderingSpritesEnabled ppu

    let private swapBuffers ppu =
        let tmp = ppu.frameBuffer
        ppu.frameBuffer <- ppu.workBuffer
        ppu.workBuffer <- tmp

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

                        if c % 8u = 0u then
                            ppu.scroll.v <- Scroll.incrementCoarseX ppu.scroll.v // Coarse X のインクリメント

                        // === 背景ピクセル描画 ===
                        let x = int c - 1
                        let y = int s

                        let pal = ppu.pal
                        let work = ppu.workBuffer

                        let bgColor =
                            if not (isRenderingBackgroundEnabled ppu)
                                || isHideBackgroundInLeftmost ppu && inLeftmostRange x
                            then
                                0
                            else
                                Background.getPaletteIndexFromRegs ppu
                        let bgColorMirrored = bgColor |> mirrorTransparentColorIndex

                        work[idx x y] <- pal[bgColorMirrored]

                        // === スプライト描画 ===
                        if isHideSpritesInLeftmost ppu && inLeftmostRange x then
                            ()
                        elif isRenderingSpritesEnabled ppu && ppu.hasSprite[x] then
                            // スプライトの描画候補を決める
                            let mutable candidateIndex = -1
                            let mutable candidateColor = 0
                            for i = 0 to ppu.secondarySpritesCount - 1 do
                                if candidateIndex = -1 then
                                    let si = ppu.secondarySprites[i]
                                    let shift = x - int si.x
                                    if si.x < 0xF9uy && shift >= 0 && shift <= 7 then
                                        let spColor = Sprite.getPaletteIndex shift si
                                        if spColor <> 0 then
                                            candidateIndex <- i
                                            candidateColor <- spColor

                            if candidateIndex <> -1 then
                                let si = ppu.secondarySprites[candidateIndex]
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

                    // === スキャンラインの終わりに Y をインクリメント ===
                    if c = 256u then
                        ppu.scroll.v <- Scroll.incrementY ppu.scroll.v

                    elif c >= 257u && c <= 320u then
                        if c = 257u then
                            // v <- t: 水平スクロール位置コピー
                            // v[0:4] <- t[0:4] (coarse X)
                            // v[10]  <- t[10]  (nametable X)
                            // v[0:4] <- t[0:4], v[10] <- t[10]
                            // ....A.. ...BCDEF
                            let mask = 0b000_01_00000_11111us
                            ppu.scroll.v <- (ppu.scroll.v &&& ~~~mask) ||| (ppu.scroll.t &&& mask)

                            System.Array.Clear(ppu.hasSprite, 0, Screen.width) // スプライトの存在位置をすべて false で初期化
                            // === スプライト評価 ===
                            // NOTE: 本当は違うタイミングで行うところをかんたん実装で済ませている
                            // https://www.nesdev.org/w/images/default/4/4f/Ppu.svg
                            ppu.secondarySpritesCount <- Sprite.evaluateForLine (int s) ppu

                            for i = 0 to ppu.secondarySpritesCount - 1 do
                                Sprite.loadTilesInfo i ppu
                                Sprite.updateSpriteExistence i ppu
                        
                        // スキャンラインカウンタ 簡易実装
                        if c = 260u then
                            Mapper.scanlineCounter ppu.cartridge.mapper

                    // === 次のスキャンラインの冒頭 2 タイル先読み ===
                    elif c >= 321u && c <= 336u then
                        Background.loadTiles c ppu
                        if c = 328u || c = 336u then
                            Background.shiftRegisters 8 ppu.regs
                            ppu.scroll.v <- Scroll.incrementCoarseX ppu.scroll.v // Coarse X のインクリメント

                // === プリレンダーライン ===
                if s = 261us then
                    // v ← t: 垂直スクロール位置コピー
                    if c >= 280u && c <= 304u then
                        // GHIA.BC DEF.....
                        let mask = 0b111_10_11111_00000us
                        ppu.scroll.v <- (ppu.scroll.v &&& ~~~mask) ||| (ppu.scroll.t &&& mask)

                    // === 0 番スキャンラインの冒頭 2 タイル先読み ===
                    elif c >= 321u && c <= 336u then
                        Background.loadTiles c ppu
                        if c = 328u || c = 336u then
                            Background.shiftRegisters 8 ppu.regs
                            ppu.scroll.v <- Scroll.incrementCoarseX ppu.scroll.v // Coarse X のインクリメント

                    // === 奇数フレームスキップ（261,339） ===
                    elif c = 339u && ppu.frameIsOdd then
                        skipRest <- true

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
                    |> clearFlag StatusFlags.spriteZeroHit

                ppu.nmiInterrupt <- None

            // === 高速パス（スキャンライン内に収まる） ===
            if c < 340u && not skipRest then
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
