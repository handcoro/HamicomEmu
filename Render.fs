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

let render (ppu: NesPpu) frame =
  // printfn "ppu vram: %A" ppu.vram
  let bank = backgroundPatternAddr ppu.ctrl
  let mutable fr = frame
  for i in 0 .. 0x03BF do // 今は一時的に指定
    let tile = ppu.vram[i] |> uint16
    let tileX = i % 32
    let tileY = i / 32
    let tile' = ppu.chr[int (bank + tile * 16us)..int (bank + tile * 16us + 15us)]

    for y in 0 .. 7 do
      let mutable upper = tile'[y]
      let mutable lower = tile'[y + 8]
      for x in [0 .. 7] do
        let value = (1uy &&& upper) <<< 1 ||| (1uy &&& lower)
        upper <- upper >>> 1
        lower <- lower >>> 1
        let rgb =
          match value with
          | 0uy -> nesPalette[0x01]
          | 1uy -> nesPalette[0x23]
          | 2uy -> nesPalette[0x27]
          | 3uy -> nesPalette[0x30]
          | _ -> failwith "can't be"
        let px = tileX * 8 + (7 - x)
        let py = tileY * 8 + y
        fr <- setPixel px py rgb fr
  fr