namespace HamicomEmu.Ppu

module Renderer =

  open HamicomEmu.Ppu.Screen
  open HamicomEmu.Ppu.Palette
  open HamicomEmu.Ppu.Types
  open HamicomEmu.Ppu
  open HamicomEmu.Ppu.Registers

  /// 画面に見える範囲
  type Rect = {
    x1: uint
    y1: uint
    x2: uint
    y2: uint
  }

  /// 画面に見える範囲の指定
  let initialRect x1 y1 x2 y2 = {
    x1 = x1
    y1 = y1
    x2 = x2
    y2 = y2
  }

  let backgroundPalette ppu (attrTable : byte[]) tileCol tileRow =
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
    [|
      0uy
      ppu.pal[start]
      ppu.pal[start + 1]
      ppu.pal[start + 2]
    |]

  let drawSprites (ppu: PpuState) (frame: Frame) : Frame =
    let mutable frameAcc = frame
    let bank = Ppu.spritePatternAddr ppu.ctrl |> int

    for i = ppu.oam.Length - 4 downto 0 do
      if i % 4 = 0 then // スプライトごとに4バイト
        let tileIdx = ppu.oam[i + 1] |> uint16
        let tileX = ppu.oam[i + 3] |> int
        let tileY = ppu.oam[i] |> int

        let attr = ppu.oam[i + 2]
        let flipVerical   = attr >>> 7 &&& 1uy = 1uy
        let flipHorizontal = attr >>> 6 &&& 1uy = 1uy

        let paletteIdx = attr &&& 0b11uy
        let sprPal = spritePalette ppu paletteIdx

        let bank = Ppu.spritePatternAddr ppu.ctrl |> int
        let tileStart = bank + (int tileIdx * 16)
        let tile =
          if ppu.chr <> [||] then
            ppu.chr[tileStart .. tileStart + 15]
          else
            ppu.chrRam[tileStart .. tileStart + 15]

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
                match flipHorizontal, flipVerical with
                | false, false -> tileX + x,     tileY + y
                | true,  false -> tileX + 7 - x, tileY + y
                | false, true  -> tileX + x,     tileY + 7 - y
                | true,  true  -> tileX + 7 - x, tileY + 7 - y
              frameAcc <- setPixel (uint px) (uint py) color frameAcc
    frameAcc

  let screenW = 256u
  let screenH = 240u

  let renderNameTableScanline
      (ppu: PpuState)
      (nameTable: byte[])
      (viewPort: Rect)
      (shiftX: int)
      (shiftY: int)
      (scanline: int)
      drawLines
      (frame: Frame) : Frame =

    if nameTable.Length < 1 then frame else

    let bank = Ppu.backgroundPatternAddr ppu.ctrl |> int
    let attrTable = nameTable[0x3C0 .. 0x3FF]
    let mutable frameAcc = frame

    // スキャンラインに該当する行だけ描画
    for i = 0 to 0x3BF do
      let tileX = i % 32
      let tileY = i / 32
      let y = tileY * 8
      
      if scanline >= y && scanline < y + drawLines then
        let tileIdx = nameTable[i] |> int
        let tile =
          if ppu.chr <> [||] then // CHR ROM と CHR RAM の暫定的な判定
            ppu.chr[(bank + tileIdx * 16) .. (bank + tileIdx * 16 + 15)]
          else
            ppu.chrRam[(bank + tileIdx * 16) .. (bank + tileIdx * 16 + 15)]
        let palette = backgroundPalette ppu attrTable tileX tileY

        for y = 0 to 7 do
          let upper = tile[y]
          let lower = tile[y + 8]

          for x = 0 to 7 do
            // CHR データの最後尾が画面上の左
            let bit0 = (upper >>> (7 - x)) &&& 1uy
            let bit1 = (lower >>> (7 - x)) &&& 1uy
            let value = (bit1 <<< 1) ||| bit0
            let rgb =
              nesPalette[int palette[int value]]
            let px = tileX * 8 + x
            let py = tileY * 8 + y
            if px >= int viewPort.x1 && px < int viewPort.x2 &&
               py >= int viewPort.y1 && py < int viewPort.y2 then
              frameAcc <- setPixel (uint (shiftX + px)) (uint(shiftY + py)) rgb frameAcc
    frameAcc

  //   let rect1 = initialRect (uint scrlX) (uint scrlY) screenW screenH
  //   let rect2 = initialRect 0u 0u (uint scrlX) screenH
  //   let rect3 = initialRect (uint scrlX) 0u screenW (uint scrlY)
  //   let rect4 = initialRect 0u (uint scrlY) (uint scrlX) screenH

  let getScrollXY (scroll: ScrollRegisters) : int * int =
    let coarseX = int (scroll.v &&& 0b1_1111us)
    let coarseY = int ((scroll.v >>> 5) &&& 0b1_1111us)
    let fineY   = int ((scroll.v >>> 12) &&& 0b111us)
    let x = coarseX * 8 + int scroll.x
    let y = coarseY * 8 + fineY
    x, y


  let drawLines = 8

  /// 分割スクロールのためのスキャンラインごとの描画
  /// TODO: ネームテーブルが 4 つ表示される場合に対応したい
  let renderScanlineBased (ppu: PpuState) (frame: Frame) : Frame =
    let mutable frameAcc = frame
    let n = 240 / drawLines - 1 // スキャンライン一本ずつだと非効率なのである程度まとめて描いてみる

    for y = 0 to n do
      let drawStartY = y * drawLines |> uint

      let scrollX, scrollY = ppu.scrollPerScanline[int drawStartY] |> getScrollXY
      let scrlX, scrlY = int scrollX, int scrollY

      let correctedAddr = Ppu.getNameTableAddress ppu.ctrlPerScanline[int drawStartY]
      let mainNameTable, subNameTable =
        Ppu.getVisibleNameTables ppu correctedAddr

      // drawLines 分の描画範囲
      // パフォーマンスのためには狭めることも必要になるかも
      let rect1 = initialRect (uint scrlX) (drawStartY + uint scrlY) screenW (drawStartY + uint drawLines)
      let rect2 = initialRect 0u drawStartY (uint scrlX) (drawStartY + uint drawLines)

      // 背景描画
      let frame1 =
        renderNameTableScanline ppu mainNameTable rect1 (-scrlX) (-scrlY) (int drawStartY) drawLines frameAcc

      let needSub =
        scrlX > 0
      // サブ背景
      frameAcc <-
        if needSub then
          renderNameTableScanline ppu subNameTable rect2 (int screenW - int scrlX) 0 (int drawStartY) drawLines frame1
        else
          frame1
    drawSprites ppu frameAcc


  (* あとあと機能としてあったらいいかもしれない
  let showTiles (ppu : NesPpu) (tiles: int list) =
    let tileSize = 8
    let tilesPerRow = Frame.Width / tileSize
    let rows = Frame.Height / tileSize
    let frame = initialFrame

    [0 .. 1]
    |> List.fold ( fun frameAcc b ->
      let bank = b * 0x1000
      [0 .. min (tiles.Length - 1) (tilesPerRow * rows - 1)]
      |> List.fold ( fun fr i ->
        let tileN = tiles[i]
        let tile = ppu.chr[(bank + tileN * 16)..(bank + tileN * 16 + 15)]

        let tileX = i % tilesPerRow
        let tileY = i / tilesPerRow
        let palette = backgroundPalette ppu tileX tileY

        [0 .. 7]
        |> List.fold ( fun fr' y ->
          let upper = tile[y]
          let lower = tile[y + 8]
          [0 .. 7]
          |> List.fold ( fun fr'' x ->
            let value =
              let bit0 = (upper >>> (7 - x)) &&& 1uy
              let bit1 = (lower >>> (7 - x)) &&& 1uy
              (bit1 <<< 1) ||| bit0
            let rgb =
              match value with
              | 0uy -> nesPalette[int ppu.pal[0]]
              | _ -> nesPalette[int palette[int value]]
            let px = tileX * tileSize + x
            let py = tileY * tileSize + y
            setPixel px py rgb fr''
          ) fr'
        ) fr
      ) frameAcc
    ) frame
  *)