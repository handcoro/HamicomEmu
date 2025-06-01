module Render

open Screen
open Palette
open Ppu

let dumpNameTable (ppu: NesPpu) (ntIndex: int) =
  let baseAddr =
    match ntIndex with
    | 0 -> 0x2000us
    | 1 -> 0x2400us
    | 2 -> 0x2800us
    | 3 -> 0x2C00us
    | _ -> failwith "Invalid name table index (must be 0-3)"

  [0 .. 29] |> List.map (fun row ->
    [0 .. 31] |> List.map (fun col ->
      let addr = baseAddr + uint16 (row * 32 + col)
      let mirrored = Ppu.mirrorVramAddr ppu.mirror addr
      let index = int mirrored
      sprintf "%02X" ppu.vram.[index]
    )
    |> String.concat " "
  )
  |> String.concat "\n"

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
          let x', y' =
            match flipHorizontal, flipVerical with
            | false, false -> tileX + x,     tileY + y
            | true,  false -> tileX + 7 - x, tileY + y
            | false, true  -> tileX + x,     tileY + 7 - y
            | true,  true  -> tileX + 7 - x, tileY + 7 - y
          setPixel x' y' color fr'
      ) fr
    ) frameAcc
  ) frame

let render (ppu: NesPpu) frame =
  // printfn "ppu vram: %A" ppu.vram
  let bank = backgroundPatternAddr ppu.ctrl
  let mutable fr = frame
  for i in 0 .. 0x03BF do // 今は一時的に指定
    let tile = ppu.vram[i] |> uint16
    let tileX = i % 32
    let tileY = i / 32
    let tile' = ppu.chr[int (bank + tile * 16us)..int (bank + tile * 16us + 15us)]
    let palette = backgroundPalette ppu tileX tileY

    for y in 0 .. 7 do
      let mutable upper = tile'[y]
      let mutable lower = tile'[y + 8]
      for x in 0 .. 7 do
        let value = (1uy &&& lower <<< 1) ||| (1uy &&& upper)
        upper <- upper >>> 1
        lower <- lower >>> 1
        let rgb =
          match value with
          | 0uy -> nesPalette[int ppu.pal[0]]
          | 1uy -> nesPalette[int palette[1]]
          | 2uy -> nesPalette[int palette[2]]
          | 3uy -> nesPalette[int palette[3]]
          | _ -> failwith "can't be"
        let px = tileX * 8 + (7 - x)
        let py = tileY * 8 + y
        fr <- setPixel px py rgb fr
  drawSprites ppu fr
