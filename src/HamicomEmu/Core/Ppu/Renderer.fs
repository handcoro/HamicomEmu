namespace HamicomEmu.Ppu

module Renderer =

    open HamicomEmu.Ppu.Palette
    open HamicomEmu.Ppu.Types

    /// パレットインデックス→RGB変換（バッファ再利用版）
    /// 事前に確保した出力バッファを再利用してGC圧力を軽減
    let renderFrameInto (ppu: PpuState) (output: (byte * byte * byte) array) =
        for i = 0 to ppu.frameBuffer.Length - 1 do
            output[i] <- nesPalette[int ppu.frameBuffer[i]]
        output

    /// パレットインデックス→RGB変換（最適化版）
    /// Array.init で直接生成し、Array.map の間接参照オーバーヘッドを回避
    let renderFrame ppu =
        let output = Array.zeroCreate ppu.frameBuffer.Length
        renderFrameInto ppu output
