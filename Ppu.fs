module Ppu

open Cartridge

   // 7  bit  0
   // ---- ----
   // VPHB SINN
   // |||| ||||
   // |||| ||++- Base nametable address
   // |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
   // |||| |+--- VRAM address increment per CPU read/write of PPUDATA
   // |||| |     (0: add 1, going across; 1: add 32, going down)
   // |||| +---- Sprite pattern table address for 8x8 sprites
   // ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
   // |||+------ Background pattern table address (0: $0000; 1: $1000)
   // ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
   // |+-------- PPU master/slave select
   // |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
   // +--------- Generate an NMI at the start of the
   //            vertical blanking interval (0: off; 1: on)
module ControlFlags =
  let Nametable1               = 0b0000_0001uy
  let Nametable2               = 0b0000_0010uy
  let VramAddIncrement         = 0b0000_0100uy
  let SpritePatternAddress     = 0b0000_1000uy
  let BackgroundPatternAddress = 0b0001_0000uy
  let SpriteSize               = 0b0010_0000uy
  let MasterSlaveSelect        = 0b0100_0000uy
  let GenerateNmi              = 0b1000_0000uy

module StatusFlags =
  let PpuOpenBus     = 0b0001_1111uy
  let SpriteOverflow = 0b0010_0000uy
  let SpriteZeroHit  = 0b0100_0000uy
  let Vblank         = 0b1000_0000uy

module MaskFlags =
  let Grayscale = 0b0000_0001uy
  let ShowBackgroundLeftMost = 0b0000_0010uy
  let ShowSpritesLeftMost = 0b0000_0100uy
  let BackgroundRendering = 0b0000_1000uy
  let SpriteRendering = 0b0001_0000uy
  let EmphasizeRed = 0b0010_0000uy
  let EmphasizeGreen = 0b0100_0000uy
  let EmphasizeBlue = 0b1000_0000uy

type AddressRegister = {
  value: byte * byte
  hiPtr: bool
}

let initialAddressRegister = {
  value = (0uy, 0uy)
  hiPtr = true
}

type NesPpu = {
  chr: byte array
  pal: byte array
  vram: byte array
  oam: byte array
  oamAddr: byte
  mirror: Mirroring
  addrReg: AddressRegister
  ctrl: byte
  mask: byte
  status: byte
  buffer: byte
  scanline: uint16
  cycles: uint
  nmiInterrupt: option<byte>
}

let initialPpu (rom: Rom) = {
  chr = rom.chrRom
  pal = Array.create 32 0uy // パレットテーブルは32バイト
  vram = Array.create 0x2000 0uy // PPU VRAM は8KB
  oam = Array.create 256 0uy // OAM データは256バイト
  oamAddr = 0uy
  mirror = rom.screenMirroring
  addrReg = initialAddressRegister
  ctrl = 0uy // 初期状態では制御レジスタは0
  mask = 0uy
  status = 0uy
  buffer = 0uy
  scanline = 0us
  cycles = 0u
  nmiInterrupt = None
}

let hasFlag flag r = r &&& flag <> 0uy
let setFlag flag r = r ||| flag
let clearFlag flag r = r &&& (~~~flag)
let updateFlag flag condition p =
  if condition then setFlag flag p else clearFlag flag p

let private setAddrRegValue (data: uint16) =
  (byte (data >>> 8), byte data)

let getAddrRegValue ar =
  ar.value |> fun (hi, lo) -> (uint16 hi <<< 8 ||| uint16 lo)

let private ppuAdressMask = 0x3FFFus

let private updateAddressRegister (data: byte) ar =
  // hiPtr を目印に 2 回に分けて書き込む
  let ar' = if ar.hiPtr then
              { ar with value = (data, snd ar.value) }
            else
              { ar with value = (fst ar.value, data) }

  let v = getAddrRegValue ar'
  let vt' = if v > ppuAdressMask then
              setAddrRegValue (v &&& 0b11_1111_1111_1111us)
            else
              ar'.value

  let toggledHp = not ar.hiPtr
  { ar' with value = vt'; hiPtr = toggledHp }

let private incrementAddressRegister inc ar =
  let lo = snd ar.value
  let hi = fst ar.value

  let lo' = lo + inc
  let hi' = hi + if lo' < lo then 1uy else 0uy // 桁上り
  let ar' = { ar with value = (hi', lo') }
  let v = getAddrRegValue ar'
  let vt = if v > ppuAdressMask then setAddrRegValue (v &&& 0b11_1111_1111_1111us) else ar'.value
  { ar' with value = vt }

let private resetLatchAddressRegister ar = { ar with hiPtr = true }

let readFromOamData ppu = ppu.oam[int ppu.oamAddr]

let writeToAddressRegister value ppu =
  let ar = ppu.addrReg |> updateAddressRegister value
  { ppu with addrReg = ar }

/// -- ここらへんは Control 関係でまとめる？
let private VramAddressIncrement cr =
  if hasFlag ControlFlags.VramAddIncrement cr then 32uy else 1uy

let backgroundPatternAddr ctrl =
  if hasFlag ControlFlags.BackgroundPatternAddress ctrl then 0x1000us else 0x0000us

let updateControl data ppu = { ppu with ctrl = data}
let writeToControlRegister value ppu =
  let beforeNmiStatus = hasFlag ControlFlags.GenerateNmi ppu.ctrl
  let ppu' = updateControl value ppu
  let afterNmi = hasFlag ControlFlags.GenerateNmi ppu'.ctrl
  if not beforeNmiStatus && afterNmi && hasFlag StatusFlags.Vblank ppu'.status then
    { ppu' with nmiInterrupt = Some 1uy }
  else
    ppu'

let incrementVramAddress ppu =
  let inc = VramAddressIncrement ppu.ctrl
  { ppu with addrReg = ppu.addrReg |> incrementAddressRegister inc }
/// --

// Horizontal:
//   [ A ] [ a ]
//   [ B ] [ b ]

// Vertical:
//   [ A ] [ B ]
//   [ a ] [ b ]
let mirrorVramAddr mirror addr =
  let mirroredVram = addr &&& 0b10_1111_1111_1111us // 0x3000 - 0x3EFF を 0x2000 - 0x2EFF にミラーリング
  let vramIndex = mirroredVram - 0x2000us // VRAM ベクター
  let nameTable = vramIndex / 0x400us // ネームテーブルのインデックス（0, 1, 2, 3）
  match mirror, nameTable with
  | Vertical, 2us | Vertical, 3us -> vramIndex - 0x800us // a b -> A B
  | Horizontal, 2us -> vramIndex - 0x400us // B -> B
  | Horizontal, 1us -> vramIndex - 0x400us // a -> A
  | Horizontal, 3us -> vramIndex - 0x800us // b -> B
  | _ -> vramIndex // それ以外はそのまま

let readFromDataRegister ppu =
  let addr = getAddrRegValue ppu.addrReg
  // アドレスをインクリメント
  let ppu' = incrementVramAddress ppu

  match addr with
  | addr when addr <= 0x1FFFus ->
    let result = ppu'.buffer
    result, { ppu' with buffer = ppu'.chr[int addr] }

  | addr when addr <= 0x2FFFus ->
    let result = ppu'.buffer
    result, { ppu' with buffer = ppu'.vram[addr |> mirrorVramAddr ppu'.mirror |> int] }

  | addr when addr <= 0x3EFFus -> failwithf "Address space 0x3000 - 0x3EFF is not expected, addr: %04X" addr
  | addr when addr <= 0x3FFFus -> // TODO: パレットのミラーリング処理
    ppu'.pal[int (addr - 0x3F00us)], ppu'
  | _ -> failwithf "Invalid PPU address: %04X" addr

let writeToDataRegister value ppu =
  let addr = getAddrRegValue ppu.addrReg
  let ppu' = incrementVramAddress ppu

  match addr with
  | addr when addr <= 0x1FFFus ->
    printfn "Attempt to Write to Chr Rom Space: %04X" addr
    ppu'

  | addr when addr <= 0x3EFFus ->
    ppu'.vram[addr |> mirrorVramAddr ppu'.mirror |> int] <- value
    ppu'

  // | addr when addr <= 0x3EFFus -> failwithf "Address space 0x3000 - 0x3EFF is not expected, addr: %04X" addr
  | addr when addr <= 0x3FFFus -> // TODO: パレットのミラーリング処理
    ppu'.pal[int (addr - 0x3F00us)] <- value
    ppu'
  | _ -> failwithf "Invalid PPU address: %04X" addr

let resetVblankStatus status = clearFlag StatusFlags.Vblank status

let readFromStatusRegister status =
  let st = resetVblankStatus status
  status, st

let ppuTick cycles ppu =
  let cyc = ppu.cycles + uint cycles

  // OAM アドレスの処理を後で入れる
  // 340 は画面のライン 1 本分
  if cyc < 341u then
    { ppu with cycles = cyc }
  else
    let cyc' = cyc - 341u
    let nextScanline = ppu.scanline + 1us

    match nextScanline with
    | 241us ->
        // VBlank 開始
        let nmi = hasFlag ControlFlags.GenerateNmi ppu.ctrl
        let ctrl' = ppu.ctrl |> updateFlag ControlFlags.GenerateNmi true
        if nmi then
          let st = ppu.status |> updateFlag StatusFlags.Vblank true
          { ppu with scanline = nextScanline; cycles = cyc'; status = st; ctrl = ctrl'; nmiInterrupt = Some 1uy }
        else
          { ppu with scanline = nextScanline; cycles = cyc'; ctrl = ctrl' }

    | s when s >= 262us ->
        // フレーム終了（VBlank 終了）
        let st = resetVblankStatus ppu.status
        { ppu with scanline = 0us; status = st; cycles = cyc'; nmiInterrupt = None }

    | _ ->
        // 通常のスキャンライン進行
        { ppu with scanline = nextScanline; cycles = cyc' }