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

  let renderNameTable (ppu : PpuState) (nameTable : byte[]) viewPort shiftX shiftY frame =
    if nameTable |> Array.length < 1 then
      frame
    else
      let bank = Ppu.backgroundPatternAddr ppu.ctrl |> int
      let attrTable = nameTable[0x3C0 .. 0x3FF]

      [0 .. 0x3BF]
      |> List.fold (fun frameAcc i ->
        let tileX = i % 32
        let tileY = i / 32
        let tileIdx = nameTable[i] |> int
        let tile =
          if ppu.chr <> [||] then // CHR ROM と CHR RAM の暫定的な判定
            ppu.chr[(bank + tileIdx * 16) .. (bank + tileIdx * 16 + 15)]
          else
            ppu.chrRam[(bank + tileIdx * 16) .. (bank + tileIdx * 16 + 15)]
        let palette = backgroundPalette ppu attrTable tileX tileY
        [0 .. 7]
        |> List.fold (fun fr y ->
          let upper = tile[y]
          let lower = tile[y + 8]
          [0 .. 7]
          |> List.fold (fun fr' x ->
            let value = // CHR データの最後尾が画面上の左
              let bit0 = (upper >>> (7 - x)) &&& 1uy
              let bit1 = (lower >>> (7 - x)) &&& 1uy
              (bit1 <<< 1) ||| bit0
            let rgb =
              nesPalette[int palette[int value]]
            let px = tileX * 8 + x
            let py = tileY * 8 + y
            if px >= int viewPort.x1
              && px < int viewPort.x2
              && py >= int viewPort.y1
              && py < int viewPort.y2 then
              setPixel (uint (shiftX + px)) (uint(shiftY + py)) rgb fr'
            else
              fr'
          ) fr
        ) frameAcc
      ) frame

  let drawSprites (ppu: PpuState) (frame: Frame) : Frame =
    let oamIndices =
      [0 .. ppu.oam.Length - 4]
      |> List.filter (fun i -> i % 4 = 0) // スプライトごとに4バイト
      |> List.rev

    oamIndices
    |> List.fold (fun frameAcc i ->
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

      // タイルを描画して frameAcc を更新する
      [0 .. 7]
      |> List.fold (fun fr y ->
        let upper = tile[y]
        let lower = tile[y + 8]

        [0 .. 7]
        |> List.fold (fun fr' x ->
          let value =
            let bit0 = (upper >>> (7 - x)) &&& 1uy
            let bit1 = (lower >>> (7 - x)) &&& 1uy
            (bit1 <<< 1) ||| bit0

          match value with
          | 0uy -> fr'
          | _ ->
            let color = nesPalette[int sprPal[int value]]
            let px, py =
              match flipHorizontal, flipVerical with
              | false, false -> tileX + x,     tileY + y
              | true,  false -> tileX + 7 - x, tileY + y
              | false, true  -> tileX + x,     tileY + 7 - y
              | true,  true  -> tileX + 7 - x, tileY + 7 - y
            setPixel (uint px) (uint py) color fr'
        ) fr
      ) frameAcc
    ) frame

  let screenW = 256u
  let screenH = 240u

  let renderNameTableScanline
      (ppu: PpuState)
      (nameTable: byte[])
      (viewPort: Rect)
      (shiftX: int)
      (shiftY: int)
      (scanline: int)
      (frame: Frame) : Frame =

    if nameTable.Length < 1 then
      frame
    else
      let bank = Ppu.backgroundPatternAddr ppu.ctrl |> int
      let attrTable = nameTable[0x3C0 .. 0x3FF]

      // スキャンラインに該当する行だけ描画
      [0 .. 0x3BF]
      |> List.filter (fun i ->
        let tileY = i / 32
        let y = tileY * 8
        scanline >= y && scanline < y + 8
      )
      |> List.fold (fun frameAcc i ->
        let tileX = i % 32
        let tileY = i / 32
        let tileIdx = nameTable[i] |> int
        let tile =
          if ppu.chr <> [||] then // CHR ROM と CHR RAM の暫定的な判定
            ppu.chr[(bank + tileIdx * 16) .. (bank + tileIdx * 16 + 15)]
          else
            ppu.chrRam[(bank + tileIdx * 16) .. (bank + tileIdx * 16 + 15)]
        let palette = backgroundPalette ppu attrTable tileX tileY
        [0 .. 7]
        |> List.fold (fun fr y ->
          let upper = tile[y]
          let lower = tile[y + 8]
          [0 .. 7]
          |> List.fold (fun fr' x ->
            let value = // CHR データの最後尾が画面上の左
              let bit0 = (upper >>> (7 - x)) &&& 1uy
              let bit1 = (lower >>> (7 - x)) &&& 1uy
              (bit1 <<< 1) ||| bit0
            let rgb =
              nesPalette[int palette[int value]]
            let px = tileX * 8 + x
            let py = tileY * 8 + y
            if px >= int viewPort.x1
              && px < int viewPort.x2
              && py >= int viewPort.y1
              && py < int viewPort.y2 then
              setPixel (uint (shiftX + px)) (uint(shiftY + py)) rgb fr'
            else fr'
          ) fr
        ) frameAcc
      ) frame

  let drawSpriteScanline (ppu: PpuState) y (frame: Frame) : Frame =

    let oamIndices =
      [0 .. ppu.oam.Length - 4]
      |> List.filter (fun i -> i % 4 = 0) // スプライトごとに4バイト

    oamIndices
    |> List.fold (fun frameAcc i ->
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

      // スキャンラインに関係するかチェック（スプライト高さは8固定）
      if y < tileY || y >= tileY + 8 then
        frameAcc
      else
        let localY = 
          if flipVerical then 7 - (y - tileY)
          else y - tileY

        let upper = tile[localY]
        let lower = tile[localY + 8]

        [0 .. 7]
        |> List.fold (fun fr' x ->
          let bit0 = (upper >>> (7 - x)) &&& 1uy
          let bit1 = (lower >>> (7 - x)) &&& 1uy
          let value = (bit1 <<< 1) ||| bit0

          match value with
          | 0uy -> fr'
          | _ ->
            let color = nesPalette[int sprPal[int value]]
            let px =
              if flipHorizontal then tileX + 7 - x else tileX + x
            setPixel (uint px) (uint y) color fr'
        ) frameAcc
    ) frame

  let render (ppu: PpuState) (frame : Frame) =

    let scrlX = fst ppu.scrlReg.xy
    let scrlY = snd ppu.scrlReg.xy

    let mainNameTable, sndNameTable = Ppu.getVisibleNameTables ppu (Ppu.getNameTableAddress ppu.ctrl)

    let rect1 = initialRect (uint scrlX) (uint scrlY) screenW screenH
    let rect2 = initialRect 0u 0u (uint scrlX) screenH
    let rect3 = initialRect (uint scrlX) 0u screenW (uint scrlY)
    let rect4 = initialRect 0u (uint scrlY) (uint scrlX) screenH


    frame
    |> renderNameTable ppu mainNameTable rect1 (- int scrlX) (- int scrlY)
    |> renderNameTable ppu sndNameTable rect2 (int screenW - int scrlX) (- int scrlY)
    |> renderNameTable ppu mainNameTable rect3 (- int scrlX) (int screenH - int scrlY)
    |> renderNameTable ppu sndNameTable rect4 (int screenW - int scrlX) (- int scrlY)
    |> drawSprites ppu

  let renderScanlineBased (ppu: PpuState) (frame: Frame) : Frame =
    let mainNameTable, subNameTable =
      Ppu.getVisibleNameTables ppu (Ppu.getNameTableAddress ppu.ctrl)

    [0 .. 239]
    |> List.fold (fun frameAcc y ->
      let scrollX, scrollY = ppu.scrollPerScanline[y].xy
      let scrlX, scrlY = int scrollX, int scrollY

      // スキャンラインyに対応するviewPort（1ライン分のみ）
      let rect1 = initialRect (uint scrlX) (uint y + uint scrlY) screenW (uint y + 1u)

      // 背景描画（メインネームテーブル）
      let frame1 = renderNameTableScanline ppu mainNameTable rect1 (-scrlX) (-scrlY) y frameAcc

      // 背景描画（補完としてサブネームテーブルを描く必要があれば）
      let needSub =
        scrlX <> 0 || scrlY <> 0

      let frame2 =
        if needSub then
          let rect2 = initialRect 0u 0u (uint scrlX) (uint y + 1u)
          renderNameTableScanline ppu subNameTable rect2 (int screenW - int scrlX) (- int scrlY) y frame1
        else
          frame1

      // スプライト描画（スキャンラインごと）
      drawSpriteScanline ppu y frame2
    ) frame


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