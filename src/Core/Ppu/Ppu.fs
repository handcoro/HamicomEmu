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
  let Grayscale                = 0b0000_0001uy
  let ShowBackgroundInLeftmost = 0b0000_0010uy
  let ShowSpritesInLeftmost    = 0b0000_0100uy
  let BackgroundRendering      = 0b0000_1000uy
  let SpriteRendering          = 0b0001_0000uy
  let EmphasizeRed             = 0b0010_0000uy
  let EmphasizeGreen           = 0b0100_0000uy
  let EmphasizeBlue            = 0b1000_0000uy

type AddressRegister = {
  value: byte * byte
  hiPtr: bool
}

let initialAddressRegister = {
  value = (0uy, 0uy)
  hiPtr = true
}

type ScrollRegister = {
  xy: byte * byte
  latchX: bool
}

let initialScrollRegister = {
  xy = (0uy, 0uy)
  latchX = true
}

type NesPpu = {
  chr: byte array
  pal: byte array
  vram: byte array
  oam: byte array
  oamAddr: byte
  mirror: Mirroring
  addrReg: AddressRegister
  scrlReg: ScrollRegister
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
  scrlReg = initialScrollRegister
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
let updateFlag flag condition b =
  if condition then setFlag flag b else clearFlag flag b

let private resetLatchScrollRegister sr = { sr with latchX = true }

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

let writeToAddressRegister value ppu =
  let ar = ppu.addrReg |> updateAddressRegister value
  { ppu with addrReg = ar }

/// -- ここらへんは Control 関係でまとめる？
let private VramAddressIncrement cr =
  if hasFlag ControlFlags.VramAddIncrement cr then 32uy else 1uy

let getNameTableAddress ctrl =
  match ctrl &&& (ControlFlags.Nametable1 ||| ControlFlags.Nametable2) with
  | 0uy -> 0x2000us
  | 1uy -> 0x2400us
  | 2uy -> 0x2800us
  | 3uy -> 0x2C00us
  | _ -> failwith "can't be"

let backgroundPatternAddr ctrl =
  if hasFlag ControlFlags.BackgroundPatternAddress ctrl then 0x1000us else 0x0000us

/// 多分スプライトサイズの扱いがまだ不十分
let spritePatternAddr ctrl = 
  if not (hasFlag ControlFlags.SpriteSize ctrl) && hasFlag ControlFlags.SpritePatternAddress ctrl then
    0x1000us
  else
    0x0000us

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

/// baseAddr は $2000, $2400, $2800, $2C00 のいずれか
let getVisibleNameTables ppu baseAddr =
  let baseIndex =
    match baseAddr &&& 0x0FFFus with
    | n when n < 0x400us -> 0us
    | n when n < 0x800us -> 1us
    | n when n < 0xC00us -> 2us
    | _ -> 3us

  // ミラーリング結果に基づいて VRAM のスライスを取得
  let getTable i =
    let addr = 0x2000us + (i * 0x400us)
    let index = mirrorVramAddr ppu.mirror addr |> int
    ppu.vram[index .. index + 0x3FF]

  let main = baseIndex |> getTable
  let snd = (baseIndex + 1us) % 4us |> getTable
  main, snd

let mirrorPaletteAddr addr =
  let index = addr &&& 0x1Fus
  match index with
    | 0x10us | 0x14us | 0x18us | 0x1Cus -> index - 0x10us
    | _ -> index

let readFromDataRegister ppu =
  let addr = getAddrRegValue ppu.addrReg
  // アドレスをインクリメント
  let ppu' = incrementVramAddress ppu

  match addr with
  | addr when addr <= 0x1FFFus ->
    let result = ppu'.buffer
    result, { ppu' with buffer = ppu'.chr[int addr] }

  | addr when addr <= 0x3EFFus ->
    let result = ppu'.buffer
    result, { ppu' with buffer = ppu'.vram[addr |> mirrorVramAddr ppu'.mirror |> int] }

  | addr when addr <= 0x3FFFus -> // TODO: バッファを挟むかどうするか
    ppu'.pal[addr |> mirrorPaletteAddr |> int], ppu'
  | _ -> failwithf "Invalid PPU address: %04X" addr

let writeToDataRegister value ppu =
  let addr = getAddrRegValue ppu.addrReg
  let ppu' = incrementVramAddress ppu

  match addr with
  | addr when addr <= 0x1FFFus ->
    // printfn "Attempt to Write to Chr Rom Space: %04X" addr
    ppu'

  | addr when addr <= 0x3EFFus ->
    ppu'.vram[addr |> mirrorVramAddr ppu'.mirror |> int] <- value
    ppu'

  | addr when addr <= 0x3FFFus ->
    ppu'.pal[addr |> mirrorPaletteAddr |> int] <- value
    ppu'
  | _ -> failwithf "Invalid PPU address: %04X" addr

let resetVblankStatus status = clearFlag StatusFlags.Vblank status

let readFromStatusRegister ppu =
  let sr = resetLatchScrollRegister ppu.scrlReg
  let afterSt = resetVblankStatus ppu.status
  {ppu with scrlReg = sr }, { ppu with scrlReg = sr; status = afterSt }

let writeToMaskRegister value ppu =
  { ppu with mask = value }

let writeToOamAddress value ppu =
  {ppu with oamAddr = value}

let readFromOamData ppu =
  ppu.oam[int ppu.oamAddr]

let writeToOamData value ppu =
  ppu.oam[int ppu.oamAddr] <- value
  let nextAddr = ppu.oamAddr + 1uy // 書き込み後インクリメント
  { ppu with oamAddr = nextAddr }

let writeToOamDma values ppu =
  { ppu with oam = values }

let isSpriteZeroHit cycles ppu = // 0 番スプライトにスキャンラインが引っかかったか判定
  let y = ppu.oam[0] |> uint
  let x = ppu.oam[3] |> uint
  (y = uint ppu.scanline) && (x <= cycles) && (hasFlag MaskFlags.SpriteRendering ppu.mask)

let private updateScrollRegister (data: byte) sr =
  // hiPtr を目印に 2 回に分けて書き込む
  let sr' = if sr.latchX then
              { sr with xy = (data, snd sr.xy) }
            else
              { sr with xy = (fst sr.xy, data) }

  let toggled = not sr.latchX
  { sr' with latchX = toggled }

let writeToScrollRegister value ppu =
  let sr = ppu.scrlReg |> updateScrollRegister value
  { ppu with scrlReg = sr }

let ppuTick cycles ppu =
  let cyc = ppu.cycles + uint cycles

  // 340 は画面のライン 1 本分
  let oamAddr = if cyc <= 257u && cyc >= 320u then 0uy else ppu.oamAddr // OAM アドレスを 0 にする処理がこれで合ってるのかよくわからない
  if cyc < 341u then
    { ppu with oamAddr = oamAddr; cycles = cyc}
  else
    let st = ppu.status |> updateFlag StatusFlags.SpriteZeroHit (isSpriteZeroHit cyc ppu)
    let cyc' = cyc - 341u
    let nextScanline = ppu.scanline + 1us

    match nextScanline with
    | 241us ->
        // VBlank 開始
        let st'= st |> setFlag StatusFlags.Vblank |> clearFlag StatusFlags.SpriteZeroHit
        let nmi = hasFlag ControlFlags.GenerateNmi ppu.ctrl
        let ctrl' = ppu.ctrl |> setFlag ControlFlags.GenerateNmi
        let ppu' = { ppu with scanline = nextScanline; oamAddr = oamAddr; cycles = cyc'; status = st'; ctrl = ctrl' }
        if nmi then
          { ppu' with nmiInterrupt = Some 1uy }
        else
          ppu'

    | s when s >= 262us ->
        // フレーム終了（VBlank 終了）
        let st' = st |> resetVblankStatus |> clearFlag StatusFlags.SpriteZeroHit
        { ppu with scanline = 0us; status = st'; oamAddr = oamAddr; cycles = cyc'; nmiInterrupt = None }

    | _ ->
        // 通常のスキャンライン進行
        { ppu with scanline = nextScanline; oamAddr = oamAddr; cycles = cyc' }