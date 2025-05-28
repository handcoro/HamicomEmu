module Bus

open Cartridge
open Ppu

module Ram =
  let Begin = 0x0000us
  let MirrorsEnd = 0x1FFFus

module PpuRegisters =
  let Begin = 0x2000us
  let MirrorsEnd = 0x3FFFus

module ApuRegisters =
  let Begin = 0x4000us
  let MirrorsEnd = 0x4017us

module PrgRom =
  let Begin = 0x8000us
  let End = 0xFFFFus

type Bus = {
  CpuVram: byte array // 0x0000 - 0x1FFF
  Rom: Rom
  Ppu: NesPpu
  Cycles: uint
}

let initialBus rom = {
  CpuVram = Array.create 0x2000 0uy
  Rom = rom
  Ppu = initialPpu rom
  Cycles = 0u
}

let readPrgRom bus addr = // PRG ROM の読み込み
  let addr' = addr - 0x8000us // 0x8000 - 0xFFFF の範囲を 0x0000 - 0x7FFF に変換
  let addr2 = if bus.Rom.PrgRom.Length = 0x4000 && addr' >= 0x4000us then addr' % 0x4000us else addr' // 16KB ROM の場合はミラーリング
  bus.Rom.PrgRom[int addr2]

let inline inRange startAddr endAddr addr =
  addr >= startAddr && addr <= endAddr
let rec memRead addr bus = 
  match addr with
  | addr when addr |> inRange Ram.Begin Ram.MirrorsEnd ->
    let mirrorDownAddr = addr &&& 0b0000_0111_1111_1111us
    bus.CpuVram[int mirrorDownAddr], bus

  | addr when addr |> inRange ApuRegisters.Begin ApuRegisters.MirrorsEnd ->
    failwithf "APU is not implemented yet. addr: %04X" addr

  | 0x2000us | 0x2001us | 0x2003us | 0x2005us | 0x2006us | 0x4014us ->
    failwithf "Attempt to read from write-only PPU address: %04X" addr
  // TODO:
  // | 0x2002us -> Status
  // | 0x2004us -> OAM data

  | 0x2007us ->
    let data, ppu = readFromDataRegister bus.Ppu
    data, { bus with Ppu = ppu }

  | addr when addr |> inRange 0x2008us PpuRegisters.MirrorsEnd ->
    let mirrorDownAddr = addr &&& 0b0010_0000_0000_0111us
    memRead mirrorDownAddr bus

  | addr when addr |> inRange PrgRom.Begin PrgRom.End ->
    readPrgRom bus addr, bus

  | _ -> failwithf "Invalid Memory access at: %04X" addr

let rec memWrite addr value bus =
  match addr with
  | addr when addr |> inRange Ram.Begin Ram.MirrorsEnd ->
    let mirrorDownAddr = addr &&& 0b0000_0111_1111_1111us
    bus.CpuVram[int mirrorDownAddr] <- value
    bus

  | addr when addr |> inRange ApuRegisters.Begin ApuRegisters.MirrorsEnd ->
    failwithf "APU is not implemented yet. addr: %04X\n" addr

  | 0x2000us ->
    let ppu = writeToControlRegister value bus.Ppu
    { bus with Ppu = ppu }

  // TODO:
  // | 0x2001us -> // Mask
  // | 0x2003us -> // OAM Address
  // | 0x2004us -> // OAM Data
  // | 0x2005us -> // Scroll
  // | 0x4014us -> // OAM DMA

  | 0x2006us ->
    let ppu = writeToAddressRegister value bus.Ppu
    { bus with Ppu = ppu }

  | 0x2007us ->
    let ppu = writeToDataRegister value bus.Ppu
    { bus with Ppu = ppu }

  | addr when addr |> inRange 0x2008us PpuRegisters.MirrorsEnd ->
    let mirrorDownAddr = addr &&& 0b0010_0000_0000_0111us
    bus |> memWrite mirrorDownAddr value

  | addr when addr |> inRange PrgRom.Begin PrgRom.End -> // PRG ROM は書き込み禁止
    failwithf "Attempt to write to Cartridge Rom space. addr: %04X\n" addr

  | _ -> failwithf "Invalid Memory write-access at: %04X" addr

let memRead16 pos bus =
  let lo, bus1 = memRead pos bus
  let hi, bus2 = memRead (pos + 1us) bus1
  (uint16 hi <<< 8) ||| uint16 lo, bus2

let memRead16ZeroPage (pos: byte) bus = // ゼロページの 16 ビットデータ読み込み（リトルエンディアンをデコード）
  let loPos = pos |> uint16
  let hiPos = pos + 1uy |> uint16
  let lo, bus1 = memRead loPos bus
  let hi, bus2 = memRead hiPos bus1
  (uint16 hi <<< 8) ||| uint16 lo, bus2

let memRead16Wrap pos bus = // 16 ビットデータ読み込み（リトルエンディアンをデコード、ページ境界バグ対応）
  let lo, bus1 = memRead pos bus
  let hiPos = if pos &&& 0x00FFus = 0x00FFus then pos &&& 0xFF00us else pos + 1us
  let hi, bus2 = memRead hiPos bus1
  (uint16 hi <<< 8) ||| uint16 lo, bus2

let memWrite16 addr pos bus = // 16ビットデータ書き込み（リトルエンディアン化）
  let hi = pos >>> 8 |> byte
  let lo = pos &&& 0xFFus |> byte
  bus |> memWrite addr lo |> memWrite (addr + 1us) hi

// let pollNmiStatus bus =
  // TODO: NMI 状態の取得

let tick cycles bus =
  let cyc = bus.Cycles + uint cycles
  let ppu = ppuTick (cycles * 3u) bus.Ppu
  { bus with Cycles = cyc; Ppu = ppu }