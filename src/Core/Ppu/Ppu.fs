namespace HamicomEmu.Ppu

module Ppu =

    open HamicomEmu.Common.BitUtils
    open HamicomEmu.Cartridge
    open HamicomEmu.Mapper
    open HamicomEmu.Mapper.Common
    open HamicomEmu.Ppu.Types

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

    let width = 256
    let height = 240

    let init (cart: Cartridge) = {
        cartridge = cart
        pal = Array.create 32 0uy // パレットテーブルは32バイト
        vram = Array.create 0x2000 0uy // PPU VRAM は8KB
        oam = Array.create 256 0uy // OAM データは256バイト
        oamAddr = 0uy
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
        workBuffer = Array.zeroCreate (width * height)
        frameBuffer = Array.zeroCreate (width * height)
        regs = initialShiftRegisters
        latches = initialLatches
        scrollPerScanline = Array.init 240 (fun _ -> initialScroll)
        ctrlPerScanline = Array.zeroCreate 240
        mapperPerScanline = Array.init 240 (fun _ -> cart.mapper)
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

    let spritePatternAddr ctrl =
        if
            not (hasFlag ControlFlags.spriteSize ctrl)
            && hasFlag ControlFlags.spritePatternAddress ctrl
        then
            0x1000us
        else
            0x0000us

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

    // Horizontal:
    //   [ A ] [ a ]
    //   [ B ] [ b ]
    // Vertical:
    //   [ A ] [ B ]
    //   [ a ] [ b ]

    let mirrorVramAddr mirror mapper addr =
        let mirroredVram = addr &&& 0b10_1111_1111_1111 // 0x3000 - 0x3EFF を 0x2000 - 0x2EFF にミラーリング
        let vramIndex = mirroredVram - 0x2000 // VRAM ベクター
        let nameTable = vramIndex / 0x400

        // VRC1 などのマッパーミラーリングも取得
        let mirror = Mapper.getMirroring mirror mapper

        match mirror, nameTable with
        | Vertical, 2
        | Vertical, 3 -> vramIndex - 0x800 // a b -> A B
        | Horizontal, 2 -> vramIndex - 0x400 // B -> B
        | Horizontal, 1 -> vramIndex - 0x400 // a -> A
        | Horizontal, 3 -> vramIndex - 0x800 // b -> B
        | _ -> vramIndex // それ以外はそのまま

    /// baseAddr は $2000, $2400, $2800, $2C00 のいずれか
    let getVisibleNameTables ppu baseAddr =
        let baseIndex =
            match baseAddr &&& 0x0FFF with
            | n when n < 0x400 -> 0
            | n when n < 0x800 -> 1
            | n when n < 0xC00 -> 2
            | _ -> 3

        // ミラーリング結果に基づいて VRAM のスライスを取得
        let getTable i =
            let addr = 0x2000 + (i * 0x400)
            let index = mirrorVramAddr ppu.cartridge.screenMirroring ppu.cartridge.mapper addr
            ppu.vram[index .. index + 0x3FF]

        let main = baseIndex |> getTable
        let right = (baseIndex + 1) % 4 |> getTable
        let below = (baseIndex + 2) % 4 |> getTable
        let belowRight = (baseIndex + 3) % 4 |> getTable
        main, right, below, belowRight

    let getNameTableAddress scroll =
        let nt = scroll.v &&& ScrollMasks.nameTable >>> 10

        match nt with
        | 0us -> 0x2000
        | 1us -> 0x2400
        | 2us -> 0x2800
        | 3us -> 0x2C00
        | _ -> failwith "can't be"

    let paletteAddrToIndex addr = addr &&& 0x1F

    let mirrorPaletteIndex index =
        match index with
        | 0x10
        | 0x14
        | 0x18
        | 0x1C -> index - 0x10
        | _ -> index

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

    /// TODO: 0 番スプライトにスキャンラインが引っかかったか判定（多分まだ不十分）
    let isSpriteZeroHit cycle ppu =
        let y = ppu.oam[0] |> uint
        let x = ppu.oam[3] |> uint

        y = uint ppu.scanline
        && x <= cycle
        && hasFlag MaskFlags.spriteRendering ppu.mask

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

    /// === 背景描画関係処理 ===
    /// TODO: モジュール分割する
    let shiftBackgroundRegisters n (regs: ShiftRegisters) =
        regs.patternLow <- regs.patternLow <<< n
        regs.patternHigh <- regs.patternHigh <<< n
        regs.attrLow <- regs.attrLow <<< n
        regs.attrHigh <- regs.attrHigh <<< n

    let backgroundPatternAddr ctrl =
        if hasFlag ControlFlags.backgroundPatternAddress ctrl then
            0x1000us
        else
            0x0000us


    let fetchTileLatch ppu =
        let v = ppu.scroll.v
        let addr =
            Scroll.getTileIndexAddress v
            |> int
            |> mirrorVramAddr ppu.cartridge.screenMirroring ppu.cartridge.mapper
        ppu.latches.tile <- ppu.vram[addr]

    let fetchAttrLatch ppu =
        let v = ppu.scroll.v
        let addr =
            Scroll.getAttributeAddress v
            |> int
            |> mirrorVramAddr ppu.cartridge.screenMirroring ppu.cartridge.mapper

        let attrByte = ppu.vram[addr]
        let v = int v
        // 該当シフト量 0: 左上タイル 2: 右上タイル 4: 左下タイル 6: 右下タイル
        let shift = ((v >>> 4) &&& 0b100) ||| (v &&& 0b10)
        ppu.latches.attr <- (attrByte >>> shift) &&& 0b11uy

    let getPatternAddress ppu =
        let patternBase = backgroundPatternAddr ppu.ctrl |> int
        let tile = ppu.latches.tile |> int
        let fineY = (ppu.scroll.v &&& ScrollMasks.fineY) >>> 12 |> int
        let addr = patternBase + tile * 16 + fineY
        addr


    let fetchPatternLowLatch ppu =
        let addr = getPatternAddress ppu
        ppu.latches.patternLow <- Mapper.ppuRead addr ppu.cartridge

    let fetchPatternHighLatch ppu =
        let addr = getPatternAddress ppu + 8 // 上位バイト
        ppu.latches.patternHigh <- Mapper.ppuRead addr ppu.cartridge

    let loadRegsFromLatches ppu =
        // patternLow/High の上位8ビットを新規ロード
        let regs = ppu.regs
        regs.patternLow  <- regs.patternLow  ||| uint16 ppu.latches.patternLow
        regs.patternHigh <- regs.patternHigh ||| uint16 ppu.latches.patternHigh
        // 属性ラッチを 8bit 拡張し reg にセット（属性はラッチの 2 ビット分を 8 ピクセルに展開）
        let attr = ppu.latches.attr
        let attrLow  = if attr &&& 0b01uy <> 0uy then 0xFFus else 0x00us
        let attrHigh = if attr &&& 0b10uy <> 0uy then 0xFFus else 0x00us
        regs.attrLow  <- regs.attrLow  ||| attrLow
        regs.attrHigh <- regs.attrHigh ||| attrHigh

    let getPaletteIndexFromRegs ppu =
        let regs = ppu.regs
        let offset = int ppu.scroll.x
        let p0 = (regs.patternLow  <<< offset) &&& 0x8000us >>> 15
        let p1 = (regs.patternHigh <<< offset) &&& 0x8000us >>> 15
        let a0 = (regs.attrLow  <<< offset) &&& 0x8000us >>> 15
        let a1 = (regs.attrHigh <<< offset) &&& 0x8000us >>> 15
        (a1 <<< 3) ||| (a0 <<< 2) ||| (p1 <<< 1) ||| p0 |> byte

    let loadTiles cycle ppu =
        // 8 サイクルごとに fetch & load
        match cycle % 8u |> int with
        | 1 ->
            fetchTileLatch ppu // ネームテーブル
            loadRegsFromLatches ppu // 各ラッチ -> シフトレジスタ
        | 3 -> fetchAttrLatch ppu // 属性テーブル
        | 5 -> fetchPatternLowLatch ppu // パターンテーブル下位
        | 7 -> fetchPatternHighLatch ppu // パターンテーブル上位
        | _ -> ()

    let inline isHideBackgroundInLeftmost ppu = hasFlag MaskFlags.showBackgroundInLeftmost ppu.mask |> not

    let inline isHideSpritesInLeftmost ppu = hasFlag MaskFlags.showSpritesInLeftmost ppu.mask |> not

    let inline isRenderingBackgroundEnabled ppu = hasFlag MaskFlags.backgroundRendering ppu.mask
    let inline isRenderingSpritesEnabled ppu = hasFlag MaskFlags.spriteRendering ppu.mask

    let inline private isRenderingEnabled ppu =
        isRenderingBackgroundEnabled ppu || isRenderingSpritesEnabled ppu

    let inline idx x y = y * width + x

    let inline mirrorTransparentColorIndex i =
        if i % 4 = 0 then i &&& 0x10 else i

    /// PPU を n サイクル進める（1スキャンラインをまたぐときは精密タイミング処理を行う）
    /// tick 内で内部レンダリングを行う
    /// TODO: スプライトのレンダリング
    let tick ppu =
        let c, s = ppu.cycle, ppu.scanline
        let newCycle = c + 1u

        if c = 0u then
            ppu.cycle <- 1u
            ppu
        else

            if isRenderingEnabled ppu then
                if s < 240us then

                    // === 描画範囲内の処理 ===
                    if c >= 1u && c <= 256u then

                        loadTiles c ppu

                        if c % 8u = 0u then
                            ppu.scroll.v <- Scroll.incrementCoarseX ppu.scroll.v // Coarse X のインクリメント

                        // === 背景ピクセル描画 ===
                        let x = int c - 1
                        let y = int s
                        let paletteIndex = getPaletteIndexFromRegs ppu |> int |> mirrorTransparentColorIndex
                        ppu.workBuffer[idx x y] <- ppu.pal[paletteIndex]

                        // === シフト ===
                        shiftBackgroundRegisters 1 ppu.regs

                    // === スキャンラインの終わりに Y をインクリメント ===
                    if c = 256u then
                        ppu.scroll.v <- Scroll.incrementY ppu.scroll.v

                    // v <- t: 水平スクロール位置コピー
                    elif c = 257u then
                        // v[0:4] <- t[0:4] (coarse X)
                        // v[10]  <- t[10]  (nametable X)
                        // v[0:4] <- t[0:4], v[10] <- t[10]
                        // ....A.. ...BCDEF
                        let mask = 0b000_01_00000_11111us
                        ppu.scroll.v <- (ppu.scroll.v &&& ~~~mask) ||| (ppu.scroll.t &&& mask)
                    
                    // === 次のスキャンラインの冒頭 2 タイル先読み ===
                    elif c >= 321u && c <= 336u then
                        loadTiles c ppu
                        if c = 328u || c = 336u then
                            shiftBackgroundRegisters 8 ppu.regs
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
                        loadTiles c ppu
                        if c = 328u || c = 336u then
                            shiftBackgroundRegisters 8 ppu.regs
                            ppu.scroll.v <- Scroll.incrementCoarseX ppu.scroll.v // Coarse X のインクリメント

                    // === 奇数フレームスキップ（261,339） ===
                    elif c = 339u && ppu.frameIsOdd then
                        ppu.cycle <- 0u
                        ppu.scanline <- 0us
                        ppu.frameIsOdd <- not ppu.frameIsOdd

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
            if c < 341u then
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
                let nextCycle = newCycle - 341u
                let nextScanline = s + 1us

                let mutable status = ppu.status
                let mutable nmi = ppu.nmiInterrupt
                let mutable newScanline = nextScanline
                let mutable newCycle = nextCycle
                let mutable frameIsOdd = ppu.frameIsOdd

                // === スプライトゼロヒット検出：描画ライン終端（scanline < 240） ===
                if s < 240us then
                    status <- updateFlag StatusFlags.spriteZeroHit (isSpriteZeroHit c ppu) status

                // === フレーム終了（scanline >= 262）===
                if nextScanline >= 262us then
                    newScanline <- 0us
                    newCycle <- nextCycle
                    frameIsOdd <- not ppu.frameIsOdd
                    // フレームバッファにコピー
                    Array.blit ppu.workBuffer 0 ppu.frameBuffer 0 (width * height)

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
