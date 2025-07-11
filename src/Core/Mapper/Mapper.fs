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
    | UxROM state ->
      readUxrom addr cart state
    | NROM _ | _ ->
      readNrom addr cart

  let cpuWrite addr value cart =
    match cart.mapper with
    | UxROM _ when addr >= 0x8000us ->
      // bank 選択
      let newState = { bankSelect = value &&& 0x0Fuy }
      UxROM newState, ()
    | J87 _ when addr >= 0x6000us && addr <= 0x7FFFus ->
      let newState = { bankSelect = value &&& 0b11uy } // 0-1 bits 使用
      // printfn "[MAPPER CPU WRITE] Bank Select: %A" newState.bankSelect
      J87 newState, ()
    | _ ->
      printfn "Attempt to write to Cartridge Rom space. addr: %04X" addr
      cart.mapper, ()

  let ppuRead addr cart =
    let addr = int addr
    match cart.mapper with
    | J87 state ->
      let chr = cart.chrRom
      let hi = state.bankSelect &&& 0b01uy
      let lo = state.bankSelect &&& 0b10uy
      let bitsCorrected = ((hi <<< 1) ||| (lo >>> 1)) |> int
      let bankSize = 8 * 1024
      let totalBanks = chr.Length / bankSize
      let bank = bitsCorrected % totalBanks
      let offset = bank * bankSize
      chr[addr + offset]

    | NROM _ | _ when cart.chrRom <> [||] ->
      cart.chrRom[addr]
    | NROM _ | _ ->
      cart.chrRam[addr]

  let ppuReadRange startAddr endAddr cart =
    match cart.mapper with
    | J87 _ ->
      Array.init (endAddr - startAddr + 1) (fun i -> ppuRead (startAddr + i) cart)
    | NROM _ | _ when cart.chrRom <> [||] ->
      cart.chrRom[startAddr .. endAddr]
    | NROM _ | _ ->
      cart.chrRam[startAddr .. endAddr]

  let ppuWrite addr value cart =
    let addr = int addr
    match cart.mapper with
    | NROM _ | _ when cart.chrRom <> [||] ->
        cart
    | NROM _ | _ ->
        cart.chrRam[int addr] <- value
        cart
