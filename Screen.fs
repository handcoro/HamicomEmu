module Screen

open System
open Palette

let rnd = Random()

type Frame = {
  data: (byte * byte * byte) array
}

module Frame =
  let Width = 256
  let Height = 240

let initialFrame = {
  data = Array.create (Frame.Width * Frame.Height) (0uy, 0uy, 0uy)
}

let setPixel x y rgb fr =
  let pos = y * Frame.Width + x
  if pos < fr.data.Length then
    fr.data[pos] <- rgb
  fr

let showTiles (chr : byte[]) bank (tiles: int list) =
  let tileSize = 8
  let tilesPerRow = Frame.Width / tileSize
  let rows = Frame.Height / tileSize
  let mutable frame = initialFrame
  
  let r0 = rnd.Next(0, 60)
  let r1 = rnd.Next(0, 60)
  let r2 = rnd.Next(0, 60)
  let r3 = rnd.Next(0, 60)

  if bank <= 1 then
    for idx in 0 .. min (tiles.Length - 1) (tilesPerRow * rows - 1) do
      let tileN = tiles[idx]
      let bank' = bank * 0x1000
      let tile = chr[(bank' + tileN * 16)..(bank' + tileN * 16 + 15)]

      let tileX = idx % tilesPerRow
      let tileY = idx / tilesPerRow

      for y in 0 .. 7 do
        let mutable upper = tile[y]
        let mutable lower = tile[y + 8]
        for x in [0 .. 7] do
          let value = (1uy &&& upper) <<< 1 ||| (1uy &&& lower)
          upper <- upper >>> 1
          lower <- lower >>> 1
          let rgb =
            match value with
            | 0uy -> nesPalette[r0]
            | 1uy -> nesPalette[r1]
            | 2uy -> nesPalette[r2]
            | 3uy -> nesPalette[r3]
            | _ -> failwith "can't be"
          let px = tileX * tileSize + (7 - x)
          let py = tileY * tileSize + y
          frame <- setPixel px py rgb frame
  frame