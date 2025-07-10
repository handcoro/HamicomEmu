namespace HamicomEmu.Mapper

module Mapper =

  open HamicomEmu.Mapper.Types
  open HamicomEmu.Cartridge

  let readNrom addr cart = // PRG ROM の読み込み
    let addr' = addr - 0x8000us // 0x8000 - 0xFFFF の範囲を 0x0000 - 0x7FFF に変換
    let addr2 = if cart.prgRom.Length = 0x4000 && addr' >= 0x4000us then addr' % 0x4000us else addr' // 16KB ROM の場合はミラーリング
    cart.prgRom[int addr2]

  let readUxrom addr cart state =
    let prg = cart.prgRom
    let bankSize = 16 * 1024 // 16 KB
    let totalBanks = prg.Length / bankSize
    if addr >= 0x8000us && addr < 0xC000us then
      // 可変バンク
      let bank = int state.bankSelect % totalBanks
      let offset = bank * bankSize + int (addr - 0x8000us)
      prg[offset]
    elif addr >= 0xC000us then
      // 固定バンク（最後尾）
      let bank = totalBanks - 1
      let offset = bank * bankSize + int (addr - 0xC000us)
      prg[offset]
    else
      // PRG-ROM の外参照
      0uy

  let cpuRead addr cart =
    match cart.mapper with
    | NROM _ ->
      readNrom addr cart
    | UxROM state ->
      readUxrom addr cart state

  let cpuWrite addr value cart =
    match cart.mapper with
    | UxROM _ when addr >= 0x8000us ->
        // bank 選択
        let newState = { bankSelect = value &&& 0x0Fuy }
        UxROM newState, ()
    | _ ->
      printfn "Attempt to write to Cartridge Rom space. addr: %04X" addr
      cart.mapper, ()

  let ppuRead addr cart =
    let addr = int addr
    match cart.mapper with
    | NROM _ | _ when cart.chrRom <> [||] ->
      cart.chrRom[addr]
    | NROM _ | _ ->
      cart.chrRam[addr]

  let ppuReadRange startAddr endAddr cart =
    match cart.mapper with
    | NROM _ | _ when cart.chrRom <> [||] ->
      cart.chrRom[startAddr .. endAddr]
    | NROM _ | _ ->
      cart.chrRam[startAddr .. endAddr]
    // | Others ->
    //   Array.init (endAddr - startAddr + 1) (fun i -> ppuRead (startAddr + i) cart)

  let ppuWrite addr value cart =
    let addr = int addr
    match cart.mapper with
    | NROM _ | _ when cart.chrRom <> [||] ->
        cart
    | NROM _ | _ ->
        cart.chrRam[int addr] <- value
        cart
