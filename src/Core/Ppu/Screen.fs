namespace HamicomEmu.Ppu

module Screen =

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
    let pos = y * uint Frame.Width + x
    if pos < uint fr.data.Length then
      fr.data[int pos] <- rgb
    fr
