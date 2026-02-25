module BenchmarkHelpers

open HamicomEmu.Cartridge
open HamicomEmu.Mapper.Common
open HamicomEmu.Mapper
open HamicomEmu.Ppu.Types
open HamicomEmu.Apu.Types

// モジュールエイリアス（init関数の曖昧さ回避）
module Ppu = HamicomEmu.Ppu.Ppu
module Apu = HamicomEmu.Apu.Apu

/// CHR ROM page size (8KB)
let private chrRomPageSize = 8 * 1024

/// PRG ROM を 32KB に正規化
let private normalizePrgRom (prg: byte array) =
    match prg.Length with
    | len when len < 0x8000 ->
        let padded = Array.create 0x8000 0uy
        Array.blit prg 0 padded 0 len
        padded
    | len when len = 0x8000 -> prg
    | len when len > 0x8000 -> prg[0..0x7FFF]
    | _ -> failwith "Unexpected PRG ROM size"

/// テスト用カートリッジを作成
let createTestCartridge program =
    let prgRom = normalizePrgRom program
    let chrRom = Array.create chrRomPageSize 2uy

    {
        prgRom = prgRom
        chrRom = chrRom
        chrRam = [||]
        mapper = Factory.create 3 prgRom chrRom None
        screenMirroring = Vertical
    }

/// NOP (0xEA) で埋められた PRG ROM を作成
let createNopFilledRom () =
    Array.create 0x8000 0xEAuy

/// LDA #$42, NOP, NOP, ... のパターンで PRG ROM を作成
let createSimpleInstructionRom () =
    let program = Array.create 0x8000 0xEAuy // NOP で埋める
    program[0] <- 0xA9uy // LDA #$42
    program[1] <- 0x42uy
    program[2] <- 0xEAuy // NOP
    program[3] <- 0xEAuy // NOP
    program[4] <- 0x4Cuy // JMP $8000 (ループ)
    program[5] <- 0x00uy
    program[6] <- 0x80uy
    program

/// 各種アドレッシングモードをテストする命令列を作成
let createAddressingModeTestRom () =
    let program = Array.create 0x8000 0xEAuy
    let mutable offset = 0
    
    // Immediate
    program[offset] <- 0xA9uy; program[offset + 1] <- 0x42uy; offset <- offset + 2
    // Zero Page
    program[offset] <- 0xA5uy; program[offset + 1] <- 0x10uy; offset <- offset + 2
    // Zero Page,X
    program[offset] <- 0xB5uy; program[offset + 1] <- 0x10uy; offset <- offset + 2
    // Absolute
    program[offset] <- 0xADuy; program[offset + 1] <- 0x00uy; program[offset + 2] <- 0x20uy; offset <- offset + 3
    // Absolute,X
    program[offset] <- 0xBDuy; program[offset + 1] <- 0x00uy; program[offset + 2] <- 0x20uy; offset <- offset + 3
    // Absolute,Y
    program[offset] <- 0xB9uy; program[offset + 1] <- 0x00uy; program[offset + 2] <- 0x20uy; offset <- offset + 3
    // Indirect,X
    program[offset] <- 0xA1uy; program[offset + 1] <- 0x10uy; offset <- offset + 2
    // Indirect,Y
    program[offset] <- 0xB1uy; program[offset + 1] <- 0x10uy; offset <- offset + 2
    
    // JMP でループ
    program[offset] <- 0x4Cuy
    program[offset + 1] <- 0x00uy
    program[offset + 2] <- 0x80uy
    
    program

// ==========================================
// PPU ヘルパー関数
// ==========================================

/// 指定数のスプライトを持つPPU状態を作成
let createPpuWithSprites (spriteCount: int) (cart: Cartridge) =
    let ppu = Ppu.init cart
    // OAMに指定数のスプライトを配置
    for i in 0 .. (min spriteCount 64) - 1 do
        ppu.oam[i * 4] <- byte (i * 16) // Y座標
        ppu.oam[i * 4 + 1] <- byte i     // タイルインデックス
        ppu.oam[i * 4 + 2] <- 0uy        // 属性
        ppu.oam[i * 4 + 3] <- byte (i * 8) // X座標
    ppu

/// 特定のスキャンライン/サイクルでPPU状態を作成
let createPpuAtScanline (scanline: uint16) (cycle: uint) (cart: Cartridge) =
    let ppu = Ppu.init cart
    ppu.scanline <- scanline
    ppu.cycle <- cycle
    ppu

// ==========================================
// APU ヘルパー関数
// ==========================================

/// 各チャンネルのON/OFF指定でAPU状態を作成
let createApuWithChannels (pulse1: bool, pulse2: bool, triangle: bool, noise: bool, dmc: bool) =
    let apu = Apu.init
    let status = 
        (if pulse1 then 0b00001uy else 0uy) |||
        (if pulse2 then 0b00010uy else 0uy) |||
        (if triangle then 0b00100uy else 0uy) |||
        (if noise then 0b01000uy else 0uy) |||
        (if dmc then 0b10000uy else 0uy)
    Apu.writeToStatus status apu  // 戻り値を返す

/// 測定可能な周波数が設定されたAPU状態を作成
let createApuWithFrequencies () =
    let mutable apu = Apu.init
    // 全チャンネル有効化
    apu <- Apu.writeToStatus 0b0001_1111uy apu
    
    // Pulse1 設定（440Hz A音）
    apu <- Apu.write 0x4000us 0b0011_1111uy apu // Volume 15, constant volume
    apu <- Apu.write 0x4002us 0xFEuy apu         // Timer low
    apu <- Apu.write 0x4003us 0x00uy apu         // Timer high
    
    // Pulse2 設定（880Hz A音1オクターブ上）
    apu <- Apu.write 0x4004us 0b0011_1111uy apu // Volume 15
    apu <- Apu.write 0x4006us 0x7Fuy apu         // Timer low
    apu <- Apu.write 0x4007us 0x00uy apu         // Timer high
    
    // Triangle 設定（220Hz A音1オクターブ下）
    apu <- Apu.write 0x4008us 0x81uy apu         // Linear counter
    apu <- Apu.write 0x400Aus 0xFCuy apu         // Timer low
    apu <- Apu.write 0x400Bus 0x01uy apu         // Timer high
    
    // Noise 設定（短周期）
    apu <- Apu.write 0x400Cus 0b0011_1111uy apu // Volume 15
    apu <- Apu.write 0x400Eus 0x00uy apu         // Period 0
    
    apu