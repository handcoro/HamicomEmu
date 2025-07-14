namespace HamicomEmu.Ppu

module Ppu =

    open HamicomEmu.Common.BitUtils
    open HamicomEmu.Cartridge
    open HamicomEmu.Mapper
    open HamicomEmu.Mapper.Types
    open HamicomEmu.Ppu.Types

    let initialScroll = { v = 0us; t = 0us; x = 0uy; w = false }

    let initial (cart: Cartridge) = {
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
        // スクロール情報のスナップショット
        scrollPerScanline = Array.init 240 (fun _ -> initialScroll)
        ctrlPerScanline = Array.zeroCreate 240
        mapperPerScanline = Array.init 240 (fun _ -> cart.mapper)
        frameIsOdd = false
        frameJustCompleted = false
    }

    let private ppuAddressMask = 0x3FFFus

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

    let backgroundPatternAddr ctrl =
        if hasFlag ControlFlags.backgroundPatternAddress ctrl then
            0x1000us
        else
            0x0000us

    /// 多分スプライトサイズの扱いがまだ不十分
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

    let mirrorPaletteAddr addr =
        let index = addr &&& 0x1F

        match index with
        | 0x10
        | 0x14
        | 0x18
        | 0x1C -> index - 0x10
        | _ -> index
    
    let writePaletteRam addr value ppu =
        let index = addr &&& 0x1F
        let mirrored = index |> mirrorPaletteAddr
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
            let chr = Mapper.ppuRead addr ppu'.cartridge ppu'.cartridge.mapper
            result, { ppu' with buffer = chr }

        | addr when addr <= 0x3EFF ->
            let result = ppu'.buffer

            result,
            { ppu' with buffer = ppu'.vram[mirrored] }

        | addr when addr <= 0x3FFF ->
            // NOTE: 新しい PPU はバッファを介さないらしい？ そしてバッファにはネームテーブルのミラーが入る
            // TODO: 上位 2 bit にオープンバス情報を含める
            let result = ppu'.pal[addr |> mirrorPaletteAddr]

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

    let inline private renderingEnabled ppu =
        hasFlag MaskFlags.backgroundRendering ppu.mask
        || hasFlag MaskFlags.spriteRendering ppu.mask

    /// PPU を n サイクル進める（1スキャンラインをまたぐときは精密タイミング処理を行う）
    /// TODO: 再現度向上のためにはレンダリングを tick 内で行うようにしたい
    /// TODO: Coarse X や Y のインクリメント
    let tick ppu =
        let c, s = ppu.cycle, ppu.scanline
        let newCycle = c + 1u

        if renderingEnabled ppu then
            // v ← t: 水平スクロール位置コピー
            if s < 240us && newCycle = 257u then
                // v[0:4] ← t[0:4] (coarse X)
                // v[10]  ← t[10]  (nametable X)
                // v[0:4] ← t[0:4], v[10] ← t[10]
                // ....A.. ...BCDEF
                let mask = 0b0000_0100_0001_1111us
                ppu.scroll.v <- (ppu.scroll.v &&& ~~~mask) ||| (ppu.scroll.t &&& mask)

            // v ← t: 垂直スクロール位置コピー
            if s = 261us && newCycle = 304u then
                // GHIA.BC DEF.....
                let mask = 0b0111_1011_1110_0000us
                ppu.scroll.v <- (ppu.scroll.v &&& ~~~mask) ||| (ppu.scroll.t &&& mask)

            // === 奇数フレームスキップ（261,339） ===
            if s = 261us && newCycle = 339u && ppu.frameIsOdd then
                ppu.cycle <- 0u
                ppu.scanline <- 0us
                ppu.frameIsOdd <- not ppu.frameIsOdd
                ppu.frameJustCompleted <- true

        // === VBlank 開始（scanline 241 開始時）===
        if s = 241us && newCycle = 1u then
            ppu.status <- setFlag StatusFlags.vblank ppu.status

            if hasFlag ControlFlags.generateNmi ppu.ctrl then
                ppu.nmiInterrupt <- Some 1uy

        // === フラグの初期化：プリレンダーライン開始（scanline 261, cycle 1）===
        if s = 261us && newCycle = 1u then
            ppu.status <-
                ppu.status
                |> clearFlag StatusFlags.vblank
                |> clearFlag StatusFlags.spriteZeroHit

            ppu.nmiInterrupt <- None

        // === 高速パス（スキャンライン内に収まる） ===
        if newCycle < 341u then
            let oamAddr =
                if newCycle >= 257u && newCycle <= 320u then
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
            let mutable frameJustCompleted = ppu.frameJustCompleted

            // === スプライトゼロヒット検出：描画ライン終端（scanline < 240） ===
            if s < 240us then
                status <- updateFlag StatusFlags.spriteZeroHit (isSpriteZeroHit c ppu) status

            // === フレーム終了（scanline >= 262）===
            if nextScanline >= 262us then
                newScanline <- 0us
                newCycle <- nextCycle
                frameIsOdd <- not ppu.frameIsOdd
                frameJustCompleted <- true

            // === スクロールとコントロールレジスタの記録（描画ラインのみ）===
            ppu.cycle <- newCycle
            ppu.scanline <- newScanline

            ppu.oamAddr <-
                if newCycle >= 257u && newCycle <= 320u then
                    0uy
                else
                    ppu.oamAddr

            ppu.status <- status
            ppu.nmiInterrupt <- nmi
            ppu.frameIsOdd <- frameIsOdd
            ppu.frameJustCompleted <- frameJustCompleted

            if nextScanline < 240us then
                let i = int s
                ppu.scrollPerScanline[i] <- ppu.scroll
                ppu.ctrlPerScanline[i] <- ppu.ctrl
                ppu.mapperPerScanline[i] <- ppu.cartridge.mapper

            ppu

    let rec tickN n ppu =
        if n <= 0u then
            ppu
        else
            let ppu' = tick ppu
            tickN (n - 1u) ppu'

module PpuPublicState =

    open HamicomEmu.Ppu.Types

    let initial mapper = {
        scrollPerScanline = Array.init 240 (fun _ -> Ppu.initialScroll)
        ctrlPerScanline = Array.zeroCreate 240
        mapperPerScanline = Array.init 240 (fun _ -> mapper)
    }

    let fromPpu (ppu: PpuState) : PpuPublicState = {
        scrollPerScanline = Array.copy ppu.scrollPerScanline
        ctrlPerScanline = Array.copy ppu.ctrlPerScanline
        mapperPerScanline = Array.copy ppu.mapperPerScanline
    }
