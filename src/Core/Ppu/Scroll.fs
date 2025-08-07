namespace HamicomEmu.Ppu

module Scroll =

    
    let coarseXMask      = 0b000_00_00000_11111us
    let coarseYMask      = 0b000_00_11111_00000us
    let horizontalNTMask = 0b000_01_00000_00000us
    let verticalNTMask   = 0b000_10_00000_00000us
    let fineYMask        = 0b111_00_00000_00000us

    /// coarse X のインクリメント（水平スクロール）
    let incrementCoarseX v =
        if (v &&& coarseXMask) = 31us then
            // 水平ネームテーブル切り替え
            (v &&& ~~~coarseXMask) ^^^ horizontalNTMask
        else
            v + 1us

    /// Y方向の進行（fineY や coarseY を含む垂直スクロール）
    let incrementY v =
        let fineY = v &&& fineYMask >>> 12
        if fineY < 7us then
            v + 0b001_00_00000_00000us // fine Y + 1
        else
            let v = v &&& ~~~fineYMask // fineY = 0
            let coarseY = v &&& coarseYMask >>> 5
            if coarseY = 29us then
                // 垂直ネームテーブル切り替え
                (v &&& ~~~coarseYMask) ^^^ verticalNTMask
            elif coarseY = 31us then
                // ネームテーブルはトグルしない
                v &&& ~~~coarseYMask
            else
                // coarse Y + 1
                v + 0b1_00000us

    /// Coarse X、Coarse Y、ネームテーブルをもとにタイル単位のアドレス取得
    let inline getTileIndexAddress v =
        0x2000us ||| (v &&& 0xFFFus)

    /// NN 1111 YYY XXX
    /// || |||| ||| +++-- high 3 bits of coarse X (x/4)
    /// || |||| +++------ high 3 bits of coarse Y (y/4)
    /// || ++++---------- attribute offset (960 bytes)
    /// ++--------------- nametable select
    let inline getAttributeAddress v =
        0x23C0us ||| (v &&& 0x0C00us) ||| ((v >>> 4) &&& 0x38us) ||| ((v >>> 2) &&& 0x07us)
        // ntAddr ||| 0x23C0us ||| coarseYHi ||| coarseXHi
