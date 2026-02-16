namespace HamicomEmu.Ppu

module Common =

    open HamicomEmu.Ppu.Screen
    open HamicomEmu.Mapper
    open HamicomEmu.Mapper.Common

    let mirrorPaletteIndex index =
        match index with
        | 0x10
        | 0x14
        | 0x18
        | 0x1C -> index - 0x10
        | _ -> index

    // Horizontal:
    //   [ A ] [ a ]
    //   [ B ] [ b ]
    // Vertical:
    //   [ A ] [ B ]
    //   [ a ] [ b ]

    let mirrorVramAddr mirror mapper addr =
        let mirroredVram = addr &&& 0b10_1111_1111_1111 // 0x3000 - 0x3EFF を 0x2000 - 0x2EFF にミラーリング
        let vramIndex = mirroredVram - 0x2000 // VRAM ベクター
        let nameTable = vramIndex / 0x400

        // VRC1 などのマッパーミラーリングも取得
        let mirror = Mapper.getMirroring mirror mapper

        match mirror, nameTable with
        | Vertical, 2
        | Vertical, 3 -> vramIndex - 0x800 // a b -> A B
        | Horizontal, 2 -> vramIndex - 0x400 // B -> B
        | Horizontal, 1 -> vramIndex - 0x400 // a -> A
        | Horizontal, 3 -> vramIndex - 0x800 // b -> B
        | _ -> vramIndex // それ以外はそのまま

    let inline idx x y = y * width + x

    let inline inLeftmostRange x = x >= 0 && x <= 7

    let inline mirrorTransparentColorIndex i =
        if i % 4 = 0 then i &&& 0x10 else i

    /// ビット反転ユーティリティ
    let inline reverseBits b =
        let mutable x = 0uy
        for i in 0 .. 7 do
            if (b >>> i) &&& 1uy <> 0uy then
                x <- x ||| (1uy <<< (7 - i))
        x
