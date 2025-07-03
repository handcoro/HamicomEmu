namespace HamicomEmu.Ppu

module Screen =

  type Frame = {
    data: (byte * byte * byte) array
    bgPaletteIdx: byte array
  }

  module Frame =
    let width = 256
    let height = 240

  let initialFrame = {
    data = Array.create (Frame.width * Frame.height) (0uy, 0uy, 0uy)
    bgPaletteIdx = Array.create (Frame.width * Frame.height) 0uy
  }

  let isValidPixel x y =
    x < uint Frame.width && y < uint Frame.height

  let setBackgroundPixel x y rgb paletteIdx fr =
    if isValidPixel x y then
      let pos = y * uint Frame.width + x
      fr.data[int pos] <- rgb
      fr.bgPaletteIdx[int pos] <- paletteIdx
    fr

  let setSpritePixel x y rgb fr =
    if isValidPixel x y then
      let pos = y * uint Frame.width + x
      fr.data[int pos] <- rgb
    fr