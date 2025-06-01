module Render

open Screen
open Palette
open Ppu

let backgroundPalette ppu tileCol tileRow =
  let attrTableIdx = tileRow / 4 * 8 + tileCol / 4
  let attrByte = ppu.vram[0x3C0 + attrTableIdx] // 一時的に指定

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
    ppu.pal[start + 2]
    ppu.pal[start + 2]
  |]

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

let drawBackground (ppu: NesPpu) (frame: Frame) : Frame =
  let bank = backgroundPatternAddr ppu.ctrl

  [0 .. 0x03BF] // 今は一時的に指定
  |> List.fold ( fun frameAcc i ->
    let tile = ppu.vram[i] |> uint16
    let tileX = i % 32
    let tileY = i / 32
    let tile' = ppu.chr[int (bank + tile * 16us)..int (bank + tile * 16us + 15us)]
    let palette = backgroundPalette ppu tileX tileY

    [0 .. 7]
    |> List.fold (fun fr y ->
      let upper = tile'[y]
      let lower = tile'[y + 8]
      [0 .. 7]
      |> List.fold (fun fr' x ->
        let value = // CHR データの最後尾が画面上の左
          let bit0 = (upper >>> (7 - x)) &&& 1uy
          let bit1 = (lower >>> (7 - x)) &&& 1uy
          (bit1 <<< 1) ||| bit0
        let rgb =
          match value with
          | 0uy -> nesPalette[int ppu.pal[0]]
          | _ -> nesPalette[int palette[int value]]
        let px = tileX * 8 + x
        let py = tileY * 8 + y
        setPixel px py rgb fr'
      ) fr
    ) frameAcc
  ) frame

let drawSprites (ppu: NesPpu) (frame: Frame) : Frame =
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

    let bank = spritePatternAddr ppu.ctrl |> int
    let tileStart = bank + (int tileIdx * 16)
    let tile = ppu.chr[tileStart .. tileStart + 15]

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
          setPixel px py color fr'
      ) fr
    ) frameAcc
  ) frame

let render (ppu: NesPpu) frame =
  frame |> drawBackground ppu |> drawSprites ppu
