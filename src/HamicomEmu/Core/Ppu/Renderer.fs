namespace HamicomEmu.Ppu

module Renderer =

    open HamicomEmu.Ppu.Palette
    open HamicomEmu.Ppu.Types

    let renderFrame ppu =
        ppu.frameBuffer |> Array.map (fun idx -> nesPalette[int idx])
