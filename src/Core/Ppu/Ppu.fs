namespace HamicomEmu.Ppu

module Ppu =

  open HamicomEmu.Cartridge
  open HamicomEmu.Ppu.Registers
  open HamicomEmu.Ppu.Types

  let initial (rom: Rom) = {
    chr = rom.chrRom
    chrRam = rom.chrRam
    pal = Array.create 32 0uy // パレットテーブルは32バイト
    vram = Array.create 0x2000 0uy // PPU VRAM は8KB
    oam = Array.create 256 0uy // OAM データは256バイト
    oamAddr = 0uy
    mirror = rom.screenMirroring
    addrReg = initialAddressRegister
    scrlReg = initialScrollRegister
    ctrl = 0uy // 初期状態では制御レジスタは0
    mask = 0b0001_0000uy
    status = 0uy
    buffer = 0uy
    scanline = 0us
    cycle = 0u
    nmiInterrupt = None
    clearNmiInterrupt = false
    latch = true
    // スクロール情報のキャッシュ
    scrollPerScanline = Array.init 240 (fun _ -> { xy = (0uy, 0uy) })
  }

  // let initialRenderCache = {
  //   scrollPerScanline = Array.init 240 (fun _ -> { xy = (0uy, 0uy) })
  // }

  let hasFlag flag r = r &&& flag <> 0uy
  let setFlag flag r = r ||| flag
  let clearFlag flag r = r &&& (~~~flag)
  let updateFlag flag condition b =
    if condition then setFlag flag b else clearFlag flag b

  let private setAddrRegValue (data: uint16) =
    (byte (data >>> 8), byte data)

  let getAddrRegValue ar =
    ar.value |> fun (hi, lo) -> (uint16 hi <<< 8 ||| uint16 lo)

  let private ppuAdressMask = 0x3FFFus

  let private updateAddressRegister (data: byte) ppu ar =
    // latch を目印に 2 回に分けて書き込む
    let ar' = if ppu.latch then
                { ar with value = (data, snd ar.value) }
              else
                { ar with value = (fst ar.value, data) }

    let v = getAddrRegValue ar'
    let vt' = if v > ppuAdressMask then
                setAddrRegValue (v &&& 0b11_1111_1111_1111us)
              else
                ar'.value

    let toggled = not ppu.latch
    { ppu with latch = toggled }, { ar' with value = vt' }

  let private incrementAddressRegister inc ar =
    let lo = snd ar.value
    let hi = fst ar.value

    let lo' = lo + inc
    let hi' = hi + if lo' < lo then 1uy else 0uy // 桁上り
    let ar' = { ar with value = (hi', lo') }
    let v = getAddrRegValue ar'
    let vt = if v > ppuAdressMask then setAddrRegValue (v &&& 0b11_1111_1111_1111us) else ar'.value
    { ar' with value = vt }

  let private resetInternalLatch ppu = { ppu with latch = true }

  let writeToAddressRegister value ppu =
    let ppu', ar = updateAddressRegister value ppu ppu.addrReg
    { ppu' with addrReg = ar }

  /// -- ここらへんは Control 関係でまとめる？
  let private VramAddressIncrement cr =
    if hasFlag ControlFlags.VramAddIncrement cr then 32uy else 1uy

  let getNameTableAddress ctrl =
    match ctrl &&& (ControlFlags.Nametable1 ||| ControlFlags.Nametable2) with
    | 0uy -> 0x2000us
    | 1uy -> 0x2400us
    | 2uy -> 0x2800us
    | 3uy -> 0x2C00us
    | _ -> failwith "can't be"

  let backgroundPatternAddr ctrl =
    if hasFlag ControlFlags.BackgroundPatternAddress ctrl then 0x1000us else 0x0000us

  /// 多分スプライトサイズの扱いがまだ不十分
  let spritePatternAddr ctrl = 
    if not (hasFlag ControlFlags.SpriteSize ctrl) && hasFlag ControlFlags.SpritePatternAddress ctrl then
      0x1000us
    else
      0x0000us

  let updateControl data ppu = { ppu with ctrl = data}

  let writeToControlRegister value ppu =
    let beforeNmiStatus = hasFlag ControlFlags.GenerateNmi ppu.ctrl
    let ppu' = updateControl value ppu
    let afterNmi = hasFlag ControlFlags.GenerateNmi ppu'.ctrl
    if not beforeNmiStatus && afterNmi && hasFlag StatusFlags.Vblank ppu'.status then
      { ppu' with nmiInterrupt = Some 1uy }
    else
      ppu'

  let incrementVramAddress ppu =
    let inc = VramAddressIncrement ppu.ctrl
    { ppu with addrReg = ppu.addrReg |> incrementAddressRegister inc }

  // Horizontal:
  //   [ A ] [ a ]
  //   [ B ] [ b ]
  // Vertical:
  //   [ A ] [ B ]
  //   [ a ] [ b ]
  let mirrorVramAddr mirror addr =
    let mirroredVram = addr &&& 0b10_1111_1111_1111us // 0x3000 - 0x3EFF を 0x2000 - 0x2EFF にミラーリング
    let vramIndex = mirroredVram - 0x2000us // VRAM ベクター
    let nameTable = vramIndex / 0x400us // ネームテーブルのインデックス（0, 1, 2, 3）
    match mirror, nameTable with
    | Vertical, 2us | Vertical, 3us -> vramIndex - 0x800us // a b -> A B
    | Horizontal, 2us -> vramIndex - 0x400us // B -> B
    | Horizontal, 1us -> vramIndex - 0x400us // a -> A
    | Horizontal, 3us -> vramIndex - 0x800us // b -> B
    | _ -> vramIndex // それ以外はそのまま

  /// baseAddr は $2000, $2400, $2800, $2C00 のいずれか
  let getVisibleNameTables ppu baseAddr =
    let baseIndex =
      match baseAddr &&& 0x0FFFus with
      | n when n < 0x400us -> 0us
      | n when n < 0x800us -> 1us
      | n when n < 0xC00us -> 2us
      | _ -> 3us

    // ミラーリング結果に基づいて VRAM のスライスを取得
    let getTable i =
      let addr = 0x2000us + (i * 0x400us)
      let index = mirrorVramAddr ppu.mirror addr |> int
      ppu.vram[index .. index + 0x3FF]

    let main = baseIndex |> getTable
    let snd = (baseIndex + 1us) % 4us |> getTable
    main, snd

  let mirrorPaletteAddr addr =
    let index = addr &&& 0x1Fus
    match index with
      | 0x10us | 0x14us | 0x18us | 0x1Cus -> index - 0x10us
      | _ -> index

  let readFromDataRegister ppu =
    let addr = getAddrRegValue ppu.addrReg
    // アドレスをインクリメント
    let ppu' = incrementVramAddress ppu

    match addr with
    | addr when addr <= 0x1FFFus ->
      let result = ppu'.buffer
      let chr = if ppu.chr <> [||] then ppu'.chr[int addr] else ppu'.chrRam[int addr]
      result, { ppu' with buffer = chr }

    | addr when addr <= 0x3EFFus ->
      let result = ppu'.buffer
      result, { ppu' with buffer = ppu'.vram[addr |> mirrorVramAddr ppu'.mirror |> int] }

    | addr when addr <= 0x3FFFus ->
      let result = ppu'.buffer
      result, { ppu' with buffer = ppu'.pal[addr |> mirrorPaletteAddr |> int] }
    | _ -> failwithf "Invalid PPU address: %04X" addr

  let writeToDataRegister value ppu =
    let addr = getAddrRegValue ppu.addrReg
    let ppu' = incrementVramAddress ppu

    match addr with
    | addr when addr <= 0x1FFFus ->
      if ppu.chr <> [||] then
        ppu'
      else
        ppu'.chrRam[int addr] <- value
        ppu'

    | addr when addr <= 0x3EFFus ->
      ppu'.vram[addr |> mirrorVramAddr ppu'.mirror |> int] <- value
      ppu'

    | addr when addr <= 0x3FFFus ->
      ppu'.pal[addr |> mirrorPaletteAddr |> int] <- value
      ppu'
    | _ -> failwithf "Invalid PPU address: %04X" addr

  let resetVblankStatus status = clearFlag StatusFlags.Vblank status

  /// コントロールレジスタ読み込みでラッチと VBlank が初期化され、次回の NMI が抑制されるらしい
  let readFromStatusRegister ppu =
    let ppu' = resetInternalLatch ppu
    let afterSt = resetVblankStatus ppu'.status
    let clearNmi = true
    ppu, { ppu' with status = afterSt; clearNmiInterrupt = clearNmi }

  let writeToMaskRegister value ppu =
    { ppu with mask = value }

  let writeToOamAddress value ppu =
    {ppu with oamAddr = value}

  let readFromOamData ppu =
    ppu.oam[int ppu.oamAddr]

  let writeToOamData value ppu =
    ppu.oam[int ppu.oamAddr] <- value
    let nextAddr = ppu.oamAddr + 1uy // 書き込み後インクリメント
    { ppu with oamAddr = nextAddr }

  let writeToOamDma (values : byte[]) ppu =
    { ppu with oam = values }

  /// TODO: 0 番スプライトにスキャンラインが引っかかったか判定（多分まだ不十分）
  let isSpriteZeroHit cycle ppu =
    let y = ppu.oam[0] |> uint
    let x = ppu.oam[3] |> uint
    y = uint ppu.scanline && x <= cycle && hasFlag MaskFlags.SpriteRendering ppu.mask

  let private updateScrollRegister (data: byte) ppu sr =
    // latch を目印に 2 回に分けて書き込む
    let sr' = if ppu.latch then
                { sr with xy = (data, snd sr.xy) }
              else
                { sr with xy = (fst sr.xy, data) }

    let toggled = not ppu.latch
    { ppu with latch = toggled }, sr'

  let writeToScrollRegister value ppu =
    let ppu', sr = updateScrollRegister value ppu ppu.scrlReg
    { ppu' with scrlReg = sr }

  let tick cycle ppu =
    let cyc = ppu.cycle + uint cycle

    // 340 は画面のライン 1 本分
    let oamAddr = if cyc >= 257u && cyc <= 320u then 0uy else ppu.oamAddr // OAM アドレスを 0 にする処理がこれで合ってるのかよくわからない
    if cyc < 341u then
      { ppu with oamAddr = oamAddr; cycle = cyc}
    else
      let st = ppu.status |> updateFlag StatusFlags.SpriteZeroHit (isSpriteZeroHit cyc ppu)
      let cyc' = cyc - 341u
      let nextScanline = ppu.scanline + 1us

      match nextScanline with
      | 241us ->
        // VBlank 開始
        let st' = st |> setFlag StatusFlags.Vblank
        let ppu' =
          { ppu with
              scanline = nextScanline
              oamAddr = oamAddr
              cycle = cyc'
              status = st'
          }
        if hasFlag ControlFlags.GenerateNmi ppu.ctrl then
          { ppu' with nmiInterrupt = Some 1uy }
        else
          ppu'

      | s when s >= 262us ->
        // フレーム終了（VBlank 終了）
        let st' = st |> resetVblankStatus |> clearFlag StatusFlags.SpriteZeroHit
        { ppu with
            scanline = 0us
            status = st'
            oamAddr = oamAddr
            cycle = cyc'
            nmiInterrupt = None
        }
      | s when s < 241us ->
        ppu.scrollPerScanline[int ppu.scanline] <- ppu.scrlReg
        { ppu with
            scanline = nextScanline
            status   = st
            oamAddr  = oamAddr
            cycle   = cyc'
        }
      | _ ->
        // 通常のスキャンライン進行
        { ppu with
            scanline = nextScanline
            status   = st
            oamAddr  = oamAddr
            cycle   = cyc'
        }
