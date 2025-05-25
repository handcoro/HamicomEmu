module Bus

open Cartridge

module Ram =
  let Begin = 0x0000us
  let MirrorsEnd = 0x1FFFus

module PpuRegisters =
  let Begin = 0x2000us
  let MirrorsEnd = 0x3FFFus

module PrgRom =
  let Begin = 0x8000us
  let End = 0xFFFFus

type Bus = {
  CpuVram: byte array // 0x0000 - 0x1FFF
  Rom: Rom
}

let initialBus rom = {
  CpuVram = Array.create 0x2000 0uy
  Rom = rom
}

let readPrgRom bus addr = // PRG ROM の読み込み
  let addr' = addr - 0x8000us // 0x8000 - 0xFFFF の範囲を 0x0000 - 0x7FFF に変換
  let addr2 = if bus.Rom.PrgRom.Length = 0x4000 && addr' >= 0x4000us then addr' % 0x4000us else addr' // 16KB ROM の場合はミラーリング
  bus.Rom.PrgRom[int addr2]

let inline inRange startAddr endAddr addr =
  addr >= startAddr && addr <= endAddr
let memRead bus addr = 
  match addr with
  | addr when addr |> inRange Ram.Begin Ram.MirrorsEnd ->
    let mirrorDownAddr = addr &&& 0b0000_0111_1111_1111us
    bus.CpuVram.[int mirrorDownAddr]
  | addr when addr |> inRange PpuRegisters.Begin PpuRegisters.MirrorsEnd ->
    let mirrorDownAddr = addr &&& 0b0010_0000_0000_0111us
    failwithf "PPU is not implemented yet. addr: %04X\n" addr
  | addr when addr |> inRange PrgRom.Begin PrgRom.End ->
    addr |> readPrgRom bus
  | _ -> failwithf "Invalid Memory access at: %04X" addr

let memWrite addr value bus =
  match addr with
  | addr when addr |> inRange Ram.Begin Ram.MirrorsEnd ->
    let mirrorDownAddr = addr &&& 0b0000_0111_1111_1111us
    bus.CpuVram.[int mirrorDownAddr] <- value
    bus
  | addr when addr |> inRange PpuRegisters.Begin PpuRegisters.MirrorsEnd ->
    let mirrorDownAddr = addr &&& 0b0010_0000_0000_0111us
    failwithf "PPU is not implemented yet. addr: %04X\n" addr
  | addr when addr |> inRange PrgRom.Begin PrgRom.End -> // PRG ROM は書き込み禁止
    failwithf "Attempt to write to Cartridge Rom space. addr: %04X\n" addr
  | _ -> failwithf "Invalid Memory write-access at: %04X" addr

let memRead16 bus pos = // 16ビットデータ読み込み（リトルエンディアンをデコード）
  let read = memRead bus
  let lo = read  pos        |> uint16
  let hi = read (pos + 1us) |> uint16
  (hi <<< 8) ||| lo

let memWrite16 addr pos bus = // 16ビットデータ書き込み（リトルエンディアン化）
  let hi = pos >>> 8 |> byte
  let lo = pos &&& 0xFFus |> byte
  bus |> memWrite addr lo |> memWrite (addr + 1us) hi