module Tests

open System.IO

open Expecto
open HamicomEmu.Cpu.Cpu
open HamicomEmu.Bus.Bus
open HamicomEmu.Ppu.Ppu
open HamicomEmu.Cartridge
open HamicomEmu.Trace

// let runWith (program: byte[]) (setup: CpuState -> CpuState) : CpuState =
//   initialCpu
//   |> load program
//   |> reset
//   |> setup
//   |> run

let makeTraceCallback (log: ResizeArray<string>) =
  fun cpu bus ->
  let line = trace cpu bus
  if line <> "" then log.Add(line)

let tests =
  testList "Trace Tests" [
    // test "Format Trace" {
    //   let bus = initialBus (testRom [||])
    //             |> memWrite 100us 0xA2uy
    //             |> memWrite 101us 0x01uy
    //             |> memWrite 102us 0xcauy
    //             |> memWrite 103us 0x88uy
    //             |> memWrite 104us 0x00uy
      
    //   let cpu = initialCpu
    //   let cpu' = { cpu with A = 1uy; X = 2uy; Y = 3uy; PC = 0x64us }
    //   let log = ResizeArray<string>()
    //   let callback = makeTraceCallback log
    //   let _, _ = (cpu', bus) ||> runWithCallback callback

    //   let expected = [
    //     "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD"
    //     "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD"
    //     "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD"
    //   ]
    //   Expect.sequenceEqual (Seq.toList log) expected "Trace output should match expected"
    // }
    // test "Format Memory Access" {
    //   let bus = initialBus (testRom [||])
    //             // ORA ($33), Y
    //             |> memWrite 100us 0x11uy
    //             |> memWrite 101us 0x33uy
    //             // data
    //             |> memWrite 0x33us 00uy
    //             |> memWrite 0x34us 04uy
    //             // target call
    //             |> memWrite 0x400us 0xAAuy
    //   let cpu = initialCpu
    //   let cpu' = { cpu with Y = 0uy; PC = 0x64us }
    //   let log = ResizeArray<string>()
    //   let callback = makeTraceCallback log
    //   let _, _ = (cpu', bus) ||> runWithCallback callback

    //   let expected = [
    //     "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD"
    //   ]
    //   Expect.sequenceEqual (Seq.toList log) expected "Trace output should match expected"
    // }
    // test "Run nestest.nes and Trace" {
    //   let writer = System.IO.File.CreateText("mynes.log")
    //   let callback = fun cpu bus ->
    //       writer.WriteLine(trace cpu bus)
    //       writer.Flush()
    //   let raw = File.ReadAllBytes "roms/nestest.nes"
    //   let parsed = parseRom raw
    //   match parsed with
    //   | Ok rom ->
    //       let bus = initialBus rom
    //       let cpu = initialCpu |> fun c -> { c with PC = 0xC000us }
    //       (cpu, bus) ||> runWithCallback callback |> ignore
    //   | Error e -> failwith $"Failed to parse ROM: {e}"
    //   Expect.isOk parsed "nestest.nes should parse successfully"
    //   writer.Close()
    // }
    test "Run Alter Ego and Trace" {
      let writer = System.IO.File.CreateText("alter_ego.log")
      let callback = fun cpu bus ->
          writer.WriteLine(trace cpu bus)
          writer.Flush()
      let raw = File.ReadAllBytes "roms/Alter_Ego.nes"
      let parsed = parseRom raw
      match parsed with
      | Ok rom ->
          let bus = initialBus rom
          let cpu = initialCpu
          (cpu, bus) ||> reset ||> runWithCallback callback |> ignore
      | Error e -> failwith $"Failed to parse ROM: {e}"
      Expect.isOk parsed "Rom file should parse successfully"
      writer.Close()
    }

    test "PPU Simple Test" {
      let ppuAddr = 0x2000us
      let value = 0x77uy

      let bus0 = initialBus (testRom [||])
      let mutable ppu = bus0.ppu

      // コントロールレジスタ設定
      // printfn $"before: {ppu}"
      ppu <- writeToControlRegister 0x00uy ppu
      // アドレス設定
      ppu <- writeToAddressRegister 0x20uy ppu
      ppu <- writeToAddressRegister 0x00uy ppu
      // printfn $"1: {ppu}"

      // $2007 に書き込み
      ppu <- writeToDataRegister value ppu
      // printfn $"2: {ppu}"

      // 再びアドレス設定（VRAM 読みは1バイトずれている可能性あり）
      ppu <- writeToAddressRegister 0x20uy ppu
      ppu <- writeToAddressRegister 0x00uy ppu
      // printfn $"3: {ppu}"

      // 読み捨て
      let _, ppu = readFromDataRegister ppu
      // 実データ読み
      let actual, _ = readFromDataRegister ppu

      Expect.equal actual value "Should correctly read back the value written to PPU memory"
    }

  ]
  (* 色々変えすぎて今はテストが通りません
  testList "Instruction Tests" [

    test "reset" {
      let program = [| 0x00uy |]
      let cpu = initialCpu
      let result =
        cpu |> load program
            |> reset

      Expect.equal result.PC 0x8000us "Program counter should be 0x8000"
    }

    test "LDA immidiate load data" {
      let program = [| 0xA9uy; 0x05uy; 0x00uy |] // LDA 05 BRK
      let cpu = initialCpu
      let result =
        cpu |> load program
            |> reset
            |> run

      Expect.equal result.A 0x05uy "Accumulator should be 5"
      Expect.isFalse (result.P &&& Flags.Z <> 0b00uy) "Zero flag should be false"
      Expect.isFalse (result.P &&& Flags.N <> 0uy) "Negative flag shoud be false"
    }

    test "LDA zero flag" {
      let program = [| 0xA9uy; 0x00uy; 0x00uy |] // LDA 00 BRK
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu with A = 0xFFuy }
      let result = cpu |> run

      Expect.equal result.A 0x00uy "A should be 0"
      Expect.isTrue (result.P &&& Flags.Z <> 0uy) "Z flag should be true"
      Expect.isFalse (result.P &&& Flags.N <> 0uy) "N flag should be false"
    }

    test "LDA negative flag" {
      let program = [| 0xA9uy; 0x80uy; 0x00uy |] // LDA 80 BRK
      let cpu = initialCpu
      let result =
        cpu |> load program
            |> reset
            |> run

      Expect.equal result.A 0x80uy "Accumlator A should be 0x80"
      Expect.isFalse (result.P &&& Flags.Z <> 0b00uy) "Zero flag should be false"
      Expect.isTrue (result.P &&& Flags.N <> 0uy) "Negative flag shoud be true"
    }

    test "TAX move A to X" {
      let program = [| 0xAAuy; 0x00uy |] // LDA 80 BRK
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu with A = byte 10 }
      let result = cpu |> run

      Expect.equal result.X (byte 10) "Accumulator X should be 0x80"
    }

    test "Text 5 ops working together" {
      let program = [| 0xA9uy; 0xC0uy; 0xAAuy; 0xE8uy; 0x00uy |]
      let cpu = initialCpu
      let result =
        cpu |> load program
            |> reset
            |> run

      Expect.equal result.X 0xC1uy "X shoud be 0XC1"
    }

    test "INX overflow" {
      let program = [| 0xE8uy; 0xE8uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu with X = 0xFFuy }
      let result = cpu |> run

      Expect.equal result.X 0x01uy "X should be 0x01"
    }

    test "LDA from memory" {
      let program = [| 0xA5uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
            |> memWrite 0x10us 0x55uy
      let result = cpu |> run

      Expect.equal result.A 0x55uy "A should be 0x01"
    }

    test "LDA from memory zero page X" {
      let program = [| 0xB5uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
            |> memWrite 0x11us 0x56uy
      let cpu = { cpu  with X = 0x01uy }
      let result = cpu |> run

      Expect.equal result.A 0x56uy "A should be 0x01"
    }

    test "LDA from memory absolute" {
      let program = [| 0xADuy; 0x10uy; 0xAAuy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
            |> memWrite 0xAA10us 0x57uy
      let result = cpu |> run

      Expect.equal result.A 0x57uy "A should be 0x01"
    }

    test "LDA from memory absolute X" {
      let program = [| 0xBDuy; 0x10uy; 0xAAuy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
            |> memWrite 0xAA15us 0x58uy
      let cpu = { cpu  with X = 0x05uy }
      let result = cpu |> run

      Expect.equal result.A 0x58uy "A should be 0x01"
    }

    test "LDA from memory absolute Y" {
      let program = [| 0xB9uy; 0x10uy; 0xAAuy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
            |> memWrite 0xAA18us 0x59uy
      let cpu = { cpu  with Y = 0x08uy }
      let result = cpu |> run

      Expect.equal result.A 0x59uy "A should be 0x01"
    }

    test "LDA from memory indirect X" {
      let program = [| 0xA1uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
            |> memWrite 0x18us 0x03uy
            |> memWrite 0x19us 0xFFuy
            |> memWrite 0xFF03us 0x5Auy
      let cpu = { cpu  with X = 0x08uy }
      let result = cpu |> run

      Expect.equal result.A 0x5Auy "A should be 0x5A"
    }

    test "LDA from memory indirect Y" {
      let program = [| 0xB1uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
            |> memWrite 0x10us 0x04uy
            |> memWrite 0x11us 0xFFuy
            |> memWrite 0xFF14us 0x5Buy
      let cpu = { cpu  with Y = 0x10uy }
      let result = cpu |> run

      Expect.equal result.A 0x5Buy "X should be 0x01"
    }

    test "STA to memory zero page X" {
      let program = [| 0x85uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0xBAuy }
      let result = cpu |> run

      Expect.equal (0x10us |> memRead result) 0xBAuy "X should be 0x01"
    }

    test "ADC no carry" {
      let program = [| 0x69uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0x0Auy }
      let result = cpu |> run

      Expect.equal result.A 0x1Auy "A should be 0x1A"
      Expect.equal (result.P &&& Flags.C) 0uy "Carry bit should be 0"
    }

    test "ADC has carry" {
      let program = [| 0x69uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0x0Auy; P = 0x01uy }
      let result = cpu |> run

      Expect.equal result.A 0x1Buy "A should be 0xCB"
      Expect.equal (result.P &&& Flags.C) 0uy "Carry bit should be 0"
    }

    test "ADC occur carry" {
      let program = [| 0x69uy; 0x02uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0xFFuy }
      let result = cpu |> run

      Expect.equal result.A 0x01uy "A should be 0x01"
      Expect.equal (result.P &&& Flags.C) 1uy "Carry bit should be 1"
    }

    test "ADC occur carry and zero" {
      let program = [| 0x69uy; 0x01uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0xFFuy }
      let result = cpu |> run

      Expect.equal result.A 0x00uy "A should be 0x00"
      Expect.equal (result.P &&& Flags.C > 0uy) true "Carry flag should be true"
      Expect.equal (result.P &&& Flags.Z > 0uy) true "Zero flag should be true"
    }

    test "ADC occur overflow plus" {
      let program = [| 0x69uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0x7Fuy }
      let result = cpu |> run

      Expect.equal result.A 0x8Fuy "A should be 0x8F"
      Expect.equal (result.P &&& Flags.V > 0uy) true "Overflow flag should be true"
      Expect.equal (result.P &&& Flags.N > 0uy) true "Negative flag should be true"
    }

    test "ADC occur overflow plus with carry" {
      let program = [| 0x69uy; 0x10uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0x6Fuy; P = 0x01uy }
      let result = cpu |> run

      Expect.equal result.A 0x80uy "A should be 0x80"
      Expect.equal (result.P &&& Flags.C > 0uy) false "Carry flag should be false"
      Expect.equal (result.P &&& Flags.V > 0uy) true "Overflow flag should be true"
      Expect.equal (result.P &&& Flags.N > 0uy) true "Negative flag should be true"
    }

    test "ADC occur overflow minus" {
      let program = [| 0x69uy; 0x81uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0x81uy }
      let result = cpu |> run

      Expect.equal result.A 0x02uy "A should be 0x80"
      Expect.equal (result.P &&& Flags.C > 0uy) true "Carry flag should be true"
      Expect.equal (result.P &&& Flags.V > 0uy) true "Overflow flag should be true"
    }

    test "ADC overflow minul with carry" {
      let program = [| 0x69uy; 0x80uy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0x80uy; P = 0x01uy }
      let result = cpu |> run

      Expect.equal result.A 0x01uy "A should be 0x01"
      Expect.equal (result.P &&& Flags.C > 0uy) true "Carry flag should be true"
      Expect.equal (result.P &&& Flags.V > 0uy) true "Overflow flag should be true"
    }

    test "ADC no overflow" {
      let program = [| 0x69uy; 0x7Fuy; 0x00uy |]
      let cpu = initialCpu
      let cpu =
        cpu |> load program
            |> reset
      let cpu = { cpu  with A = 0x83uy }
      let result = cpu |> run

      Expect.equal (result.P &&& Flags.C > 0uy) true "Carry flag should be true"
      Expect.equal (result.P &&& Flags.V > 0uy) false "Overflow flag should be false"
    }

    test "SBC immediate subtract data" {
      let program = [| 0xA9uy; 0x10uy;  // LDA #$10
              0xE9uy; 0x05uy;  // SBC #$05
              0x00uy |]        // BRK

      let cpu = runWith program (fun c -> { c with P = setFlag Flags.C c.P }) // キャリーフラグをセット（デフォルトで必要）

      Expect.equal cpu.A 0x0Buy "Accumulator should be 0x0B (16 - 5)"
      Expect.isFalse (hasFlag Flags.Z cpu.P) "Zero flag should be false"
      Expect.isFalse (hasFlag Flags.N cpu.P) "Negative flag should be false"
    }

    test "SBC with borrow (Carry clear)" {
      let program = [| 0xE9uy; 0x01uy; 0x00uy |]
      let setup cpu = { cpu with A = 0x03uy; P = 0uy } // Carry=0 → 実質 -2
      let result = runWith program setup

      Expect.equal result.A 0x01uy "A should be 0x01 (3 - 1 - 1)"
    }

    test "SBC result zero" {
      let program = [| 0xE9uy; 0x01uy; 0x00uy |]
      let setup cpu = { cpu with A = 0x02uy; P = 0uy } // 2 - 1 - 1 = 0
      let result = runWith program setup

      Expect.equal result.A 0x00uy "Result should be zero"
      Expect.isTrue (result.P &&& Flags.Z <> 0uy) "Zero flag should be set"
    }

    test "SBC negative result" {
      let program = [| 0xE9uy; 0x03uy; 0x00uy |]
      let setup cpu = { cpu with A = 0x01uy; P = Flags.C } // 1 - 3 = -2
      let result = runWith program setup

      Expect.equal result.A 0xFEuy "A should wrap around to 0xFE"
      Expect.isTrue (result.P &&& Flags.N <> 0uy) "Negative flag should be set"
    }

    test "SBC signed overflow positive to negative" {
      let program = [| 0xE9uy; 0x7Fuy; 0x00uy |] // SBC #$7F
      let setup cpu = { cpu with A = 0x80uy; P = Flags.C } // 128 - 127 = 1, 符号変化あり
      let result = runWith program setup

      Expect.equal result.A 0x01uy "A should be 1"
      Expect.isTrue (result.P &&& Flags.V <> 0uy) "Overflow flag should be set"
    }

    test "SBC signed overflow negative to positive" {
      let program = [| 0xE9uy; 0x80uy; 0x00uy |] // SBC #$80
      let setup cpu = { cpu with A = 0x7Fuy; P = Flags.C } // 127 - 128 = -1 (→ 0xFF)
      let result = runWith program setup

      Expect.equal result.A 0xFFuy "A should be 0xFF (i.e., -1)"
      Expect.isTrue (result.P &&& Flags.V <> 0uy) "Overflow flag should be set"
      Expect.isTrue (result.P &&& Flags.N <> 0uy) "Negative flag should be set"
    }

    test "SBC no borrow, carry remains set" {
      let program = [| 0xE9uy; 0x01uy; 0x00uy |] // SBC #$01
      let setup cpu = { cpu with A = 0x03uy; P = Flags.C } // 3 - 1 = 2
      let result = runWith program setup

      Expect.equal result.A 0x02uy "A should be 2"
      Expect.isTrue (result.P &&& Flags.C <> 0uy) "Carry should remain set (no borrow)"
      Expect.isFalse (result.P &&& Flags.Z <> 0uy) "Zero should not be set"
    }

    test "SBC borrow occurs, carry cleared" {
      let program = [| 0xE9uy; 0x04uy; 0x00uy |] // SBC #$04
      let setup cpu = { cpu with A = 0x03uy; P = Flags.C } // 3 - 4 = -1 → 255, borrow
      let result = runWith program setup

      Expect.equal result.A 0xFFuy "A should be 0xFF (-1)"
      Expect.isFalse (result.P &&& Flags.C <> 0uy) "Carry should be cleared (borrow occurred)"
      Expect.isTrue (result.P &&& Flags.N <> 0uy) "Negative flag should be set"
    }

    test "SBC with carry clear subtracts extra one" {
      let program = [| 0xE9uy; 0x01uy; 0x00uy |] // SBC #$01
      let setup cpu = { cpu with A = 0x03uy; P = 0uy } // Carry clear → 3 - 1 - 1 = 1
      let result = runWith program setup

      Expect.equal result.A 0x01uy "A should be 1 due to extra borrow"
      Expect.isTrue (result.P &&& Flags.C <> 0uy) "Carry should be set (no final borrow)"
    }

    test "SBC result zero with carry set" {
      let program = [| 0xE9uy; 0x01uy; 0x00uy |] // SBC #$01
      let setup cpu = { cpu with A = 0x02uy; P = 0uy } // Carry clear → 2 - 1 - 1 = 0
      let result = runWith program setup

      Expect.equal result.A 0x00uy "A should be 0"
      Expect.isTrue (result.P &&& Flags.Z <> 0uy) "Zero flag should be set"
      Expect.isTrue (result.P &&& Flags.C <> 0uy) "Carry should be set (no final borrow)"
    }

    test "AND immediate" {
      let program = [| 0x29uy; 0x03uy;  // AND
                      0x00uy |]        // BRK

      let cpu = runWith program (fun c -> { c with A = 0x01uy })

      Expect.equal cpu.A 0x01uy "Accumulator should be 0x01"
    }

    test "EOR immediate" {
      let program = [| 0x49uy; 0x06uy;  // AND
                      0x00uy |]        // BRK

      let cpu = runWith program (fun c -> { c with A = 0x04uy })

      Expect.equal cpu.A 0x02uy "Accumulator should be 0x01"
    }

    test "ORA immediate" {
      let program = [| 0x09uy; 0b1010uy;  // AND
                      0x00uy |]        // BRK

      let cpu = runWith program (fun c -> { c with A = 0b0110uy })

      Expect.equal cpu.A 0x0Euy "Accumulator should be 0x0E"
    }

    test "ASL zero page" {
      let program = [| 0x06uy; 0x03uy; // ASL
                      0x00uy |]        // BRK

      let cpu = runWith program (fun c -> c |> memWrite 0x03us 0x0Auy)

      Expect.equal (memRead cpu 0x03us) 0x14uy "Accumulator should be 0x14"
    }

    test "ASL accumlator" {
      let program = [| 0x0Auy; // ASL
                      0x00uy |]        // BRK

      let cpu = runWith program (fun c -> { c with A = 0b0110uy })

      Expect.equal cpu.A 0b1100uy "Accumulator should be 0b1100"
    }

    test "LSR zero page" {
      let program = [| 0x46uy; 0x03uy; // LSR $03
                      0x00uy |]        // BRK

      let cpu = runWith program (fun c -> c |> memWrite 0x03us 0b1000_0010uy)

      Expect.equal (memRead cpu 0x03us) 0b0100_0001uy "Memory at $03 should be 0b0100_0001"
      Expect.equal (cpu.P &&& Flags.C) 0uy "Carry should be cleared"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.equal (cpu.P &&& Flags.N) 0uy "Negative flag should not be set"
    }

    test "LSR accumulator" {
      let program = [| 0x4Auy; // LSR A
                      0x00uy |] // BRK

      let cpu = runWith program (fun c -> { c with A = 0b0000_0011uy })

      Expect.equal cpu.A 0b0000_0001uy "Accumulator should be 0b0000_0001"
      Expect.equal (cpu.P &&& Flags.C) Flags.C "Carry should be set (bit 0 was 1)"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.equal (cpu.P &&& Flags.N) 0uy "Negative flag should not be set"
    }

    test "ASL accumulator sets carry when MSB is 1" {
      let program = [| 0x0Auy; // ASL A
                      0x00uy |] // BRK

      let cpu = runWith program (fun c -> { c with A = 0b1000_0000uy })

      Expect.equal cpu.A 0b0000_0000uy "Accumulator should be 0 after shift"
      Expect.equal (cpu.P &&& Flags.C) Flags.C "Carry should be set"
      Expect.equal (cpu.P &&& Flags.Z) Flags.Z "Zero flag should be set"
      Expect.equal (cpu.P &&& Flags.N) 0uy "Negative flag should not be set"
    }

    test "LSR accumulator clears carry when LSB is 0" {
      let program = [| 0x4Auy; // LSR A
                      0x00uy |] // BRK

      let cpu = runWith program (fun c -> { c with A = 0b0000_0010uy })

      Expect.equal cpu.A 0b0000_0001uy "Accumulator should be 0b0000_0001"
      Expect.equal (cpu.P &&& Flags.C) 0uy "Carry should be cleared"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.equal (cpu.P &&& Flags.N) 0uy "Negative flag should not be set"
    }

    test "ROL accumulator with no carry" {
      let program = [| 0x2Auy; // LSR A
                      0x00uy |] // BRK

      let cpu = runWith program (fun c -> { c with A = 0b0100_0011uy })

      Expect.equal cpu.A 0b1000_0110uy "Accumulator should be 0b1000_0110"
      Expect.equal (cpu.P &&& Flags.C) 0uy "Carry should be cleared"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "ROL accumulator with carry" {
      let program = [| 0x2Auy; // LSR A
                      0x00uy |] // BRK

      let cpu = runWith program (fun c -> { c with A = 0b0100_0011uy;
                                                   P = Flags.C })
      Expect.equal cpu.A 0b1000_0111uy "Accumulator should be 0b1000_0111"
      Expect.equal (cpu.P &&& Flags.C) 0uy "Carry should be cleared"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.isTrue (hasFlag cpu.P Flags.N) "Negative flag should be set"
    }
    test "ROL accumulator occur carry" {
      let program = [| 0x2Auy; // LSR A
                      0x00uy |] // BRK

      let cpu = runWith program (fun c -> { c with A = 0b1100_0011uy })
      Expect.equal cpu.A 0b1000_0110uy "Accumulator should be 0b1000_0110"
      Expect.isTrue (hasFlag cpu.P Flags.C) "Carry should be set"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.isTrue (hasFlag cpu.P Flags.N) "Negative flag should be set"
    }
    test "ROL zeropage occur carry" {
      let program = [| 0x26uy; 0x04uy; // ROL $04
                       0x00uy |] // BRK

      let cpu = runWith program (fun c -> c |> memWrite 0x04us 0b1000_0010uy )
      Expect.equal (memRead cpu 0x04us) 0b0000_0100uy "Memory at $04 should be 0b0000_0100"
      Expect.isTrue (hasFlag cpu.P Flags.C) "Carry should be set"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.isFalse (hasFlag Flags.N cpu.P) "Negative flag should not be set"
    }
    test "ROL accumulator zero flag" {
      let program = [| 0x2Auy; // LSR A
                      0x00uy |] // BRK

      let cpu = runWith program (fun c -> { c with A = 0uy })

      Expect.equal cpu.A 0uy "Accumulator should be 0"
      Expect.equal (cpu.P &&& Flags.C) 0uy "Carry should be cleared"
      Expect.isTrue (hasFlag cpu.P Flags.Z) "Zero flag should be set"
      Expect.isFalse (hasFlag cpu.P Flags.N) "Negative flag should not be set"
    }
    test "ROR accumulator with carry" {
      let program = [| 0x6Auy; // ROR
                      0x00uy |] // BRK

      let cpu = runWith program (fun c -> { c with A = 0b0100_0010uy;
                                                   P = Flags.C })
      Expect.equal cpu.A 0b1010_0001uy "Accumulator should be 0b1010_0001"
      Expect.equal (cpu.P &&& Flags.C) 0uy "Carry should be cleared"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "ROR zeropage occur carry" {
      let program = [| 0x66uy; 0x1Auy; // ROR $1A
                       0x00uy |] // BRK

      let cpu = runWith program (fun c -> c |> memWrite 0x1Aus 0b1000_0011uy )
      Expect.equal (memRead cpu 0x1Aus) 0b0100_0001uy "Memory at $1A should be 0b0100_0001"
      Expect.isTrue (hasFlag Flags.C cpu.P) "Carry should be set"
      Expect.equal (cpu.P &&& Flags.Z) 0uy "Zero flag should not be set"
      Expect.isFalse (hasFlag Flags.N cpu.P) "Negative flag should not be set"
    }
    test "BCC with no carry" {
      let program = [| 0x90uy; 0x02uy; // BCC +2
                       0x00uy; 0x00uy; // BRK BRK
                       0xE8uy; 0x00uy |] // INX BRK
      let cpu = runWith program (fun c -> { c with P = 0uy })
      Expect.equal cpu.X 0x01uy "X should be 1"
      Expect.equal cpu.PC 0x8006us "Program counter should be 0x8002"
    }
    test "BCC with carry" {
      let program = [| 0x90uy; 0x02uy; // BCC +2
                       0x00uy; 0x00uy; // BRK BRK
                       0xE8uy; 0x00uy |] // INX BRK
      let cpu = runWith program (fun c -> { c with P = setFlag Flags.C c.P })
      Expect.equal cpu.X 0x00uy "X should be 0"
      Expect.isTrue (hasFlag Flags.C cpu.P) "Carry flag should be set"
      Expect.equal cpu.PC 0x8003us "Program counter should be 0x8002"
    }
    test "BCS with no carry" {
      let program = [| 0xB0uy; 0x02uy; // BCC +2
                       0x00uy; 0x00uy; // BRK BRK
                       0xE8uy; 0x00uy |] // INX BRK
      let cpu = runWith program (fun c -> { c with P = 0uy })
      Expect.equal cpu.X 0x00uy "X should be 0"
      Expect.equal cpu.PC 0x8003us "Program counter should be 0x8002"
    }
    test "BCS with carry" {
      let program = [| 0xB0uy; 0x02uy; // BCC +2
                       0x00uy; 0x00uy; // BRK BRK
                       0xE8uy; 0x00uy |] // INX BRK
      let cpu = runWith program (fun c -> { c with P = setFlag Flags.C c.P })
      Expect.equal cpu.X 0x01uy "X should be 1"
      Expect.isTrue (hasFlag Flags.C cpu.P) "Carry flag should be set"
      Expect.equal cpu.PC 0x8006us "Program counter should be 0x8002"
    }
    test "BCC negative offset" {
      let program = [| 0x90uy; 0xFCuy; // BCC -4
                       0x00uy; // BRK BRK
                        |]
      let cpu = runWith program (fun c -> 
                                    c |> memWrite 0x7FFEus 0xE8uy // INX
                                      |> memWrite 0x7FFFus 0x00uy
                                      |> fun c -> { c with P = 0uy }
                                      )
      Expect.equal cpu.X 0x01uy "X should be 1"
      Expect.equal cpu.PC 0x8000us "Program counter should be 0x8001"
    }
    test "BEQ with zero flag" {
      let program = [| 0xF0uy; 0x02uy; // BCC +2
                       0x00uy; 0x00uy; // BRK BRK
                       0xE8uy; 0x00uy |] // INX BRK
      let cpu = runWith program (fun c -> { c with P = setFlag Flags.Z c.P })
      Expect.equal cpu.X 0x01uy "X should be 1"
      Expect.isFalse (hasFlag Flags.Z cpu.P) "Zero flag should be cleared"
      Expect.equal cpu.PC 0x8006us "Program counter should be 0x8006"
    }
    test "BNE with zero flag" {
      let program = [| 0xD0uy; 0x02uy; // BCC +2
                       0x00uy; 0x00uy; // BRK BRK
                       0xE8uy; 0x00uy |] // INX BRK
      let cpu = runWith program (fun c -> { c with P = setFlag Flags.Z c.P })
      Expect.equal cpu.X 0x00uy "X should be 0"
      Expect.isTrue (hasFlag Flags.Z cpu.P) "Zero flag should be set"
      Expect.equal cpu.PC 0x8003us "Program counter should be 0x8003"
    }
    test "BIT absolute all zero" {
      let program = [| 0x2Cuy; 0x00uy;   // BIT #$0000
                       0x00uy;        |] // BRK
      let cpu = runWith program (fun c -> { c with A = 0uy })
      Expect.isTrue (hasFlag Flags.Z cpu.P) "Zero flag should be set"
      Expect.isFalse (hasFlag Flags.V cpu.P) "Overflow flag should be cleared"
      Expect.isFalse (hasFlag Flags.N cpu.P) "Negative flag should be cleared"
    }
    test "BIT absolute occur VN" {
      let program = [| 0x2Cuy; 0x00uy;   // BIT #$0000
                       0x00uy         |] // BRK
      let cpu = runWith program (fun c -> c |> memWrite 0x0000us 0xFFuy
                                            |> fun c -> { c with A = 0xFFuy })
      Expect.isFalse (hasFlag Flags.Z cpu.P) "Zero flag should be cleared"
      Expect.isTrue (hasFlag Flags.V cpu.P) "Overflow flag should be set"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "BIT zeropage all zero" {
      let program = [| 0x2Cuy; 0x02uy; 0x00uy;   // BIT #$0003
                       0x00uy         |]         // BRK
      let cpu = runWith program (fun c -> { c with A = 0uy })
      Expect.equal cpu.X 0x00uy "X should be 0"
      Expect.isTrue (hasFlag Flags.Z cpu.P) "Zero flag should be set"
      Expect.isFalse (hasFlag Flags.V cpu.P) "Overflow flag should be cleared"
      Expect.isFalse (hasFlag Flags.N cpu.P) "Negative flag should be cleared"
    }
    test "CLC" {
      let program = [| 0x18uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with P = Flags.C ||| Flags.N })
      Expect.isFalse (hasFlag Flags.C cpu.P) "Carry flag shoud be cleared"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "SEC" {
      let program = [| 0x38uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with P = Flags.N })
      Expect.isTrue (hasFlag Flags.C cpu.P) "Carry flag shoud be set"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "CLD" {
      let program = [| 0xD8uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with P = Flags.D ||| Flags.N })
      Expect.isFalse (hasFlag Flags.D cpu.P) "Decimal Mode flag shoud be cleared"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "SED" {
      let program = [| 0xF8uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with P = Flags.N })
      Expect.isTrue (hasFlag Flags.D cpu.P) "Decimal Mode flag shoud be set"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "CLI" {
      let program = [| 0x58uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with P = Flags.I ||| Flags.N })
      Expect.isFalse (hasFlag Flags.I cpu.P) "Interrupt Disable flag shoud be cleared"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "SEI" {
      let program = [| 0x78uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with P = Flags.N })
      Expect.isTrue (hasFlag Flags.I cpu.P) "Interrupt Disable flag shoud be set"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "CLV" {
      let program = [| 0xB8uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with P = Flags.V ||| Flags.N })
      Expect.isFalse (hasFlag Flags.V cpu.P) "Overflow flag shoud be cleared"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "CMP" {
      let program = [| 0xC9uy; 0x01uy |]
      let cpu = runWith program (fun c -> { c with A = 0x02uy })
      Expect.isTrue (hasFlag Flags.C cpu.P) "Carry flag shoud be set"
    }
    test "CMP Equal" {
      let program = [| 0xC9uy; 0x02uy |]
      let cpu = runWith program (fun c -> { c with A = 0x02uy })
      Expect.isTrue (hasFlag Flags.C cpu.P) "Carry flag shoud be set"
      Expect.isTrue (hasFlag Flags.Z cpu.P) "Zero flag shoud be set"
    }
    test "CMP Negative" {
      let program = [| 0xC9uy; 0x03uy |]
      let cpu = runWith program (fun c -> { c with A = 0x02uy })
      Expect.isFalse (hasFlag Flags.C cpu.P) "Carry flag shoud be cleared"
      Expect.isFalse (hasFlag Flags.Z cpu.P) "Zero flag shoud be cleared"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag shoud be set"
    }
    test "CPX" {
      let program = [| 0xE0uy; 0x01uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with X = 0x02uy })
      Expect.isTrue (hasFlag Flags.C cpu.P) "Carry flag shoud be set"
    }
    test "CPY" {
      let program = [| 0xC0uy; 0x01uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with Y = 0x02uy })
      Expect.isTrue (hasFlag Flags.C cpu.P) "Carry flag shoud be set"
    }
    test "DEC zeropage" {
      let program = [| 0xC6uy; 0x01uy; 0x00uy |]
      let cpu = runWith program (fun c -> c |> memWrite 0x0001us 0x05uy)
      Expect.equal (0x0001us |> memRead cpu) 0x04uy "Adress 0x0001 value should be 0x04"
    }
    test "DEC Negative" {
      let program = [| 0xC6uy; 0x01uy; 0x00uy |]
      let cpu = runWith program (fun c -> c |> memWrite 0x0001us 0x00uy)
      Expect.equal (0x0001us |> memRead cpu) 0xFFuy "Adress 0x0001 value should be 0xFF"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag shoud be set"
    }
    test "DEX" {
      let program = [| 0xCAuy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with X = 0x05uy })
      Expect.equal cpu.X 0x04uy "Register X should be 0x04"
    }
    test "DEX Negative" {
      let program = [| 0xCAuy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with X = 0x00uy })
      Expect.equal cpu.X 0xFFuy "Register X value should be 0xFF"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag shoud be set"
    }
    test "DEY" {
      let program = [| 0x88uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with Y = 0x05uy })
      Expect.equal cpu.Y 0x04uy "Register Y should be 0x04"
    }
    test "DEY Negative" {
      let program = [| 0x88uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with Y = 0x00uy })
      Expect.equal cpu.Y 0xFFuy "Register Y value should be 0xFF"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag shoud be set"
    }
    test "INC zeropage" {
      let program = [| 0xE6uy; 0x01uy; 0x00uy |]
      let cpu = runWith program (fun c -> c |> memWrite 0x0001us 0x05uy)
      Expect.equal (0x0001us |> memRead cpu) 0x06uy "Adress 0x0001 value should be 0x06"
    }
    test "INC Zero flag" {
      let program = [| 0xE6uy; 0x01uy; 0x00uy |]
      let cpu = runWith program (fun c -> c |> memWrite 0x0001us 0xFFuy)
      Expect.equal (0x0001us |> memRead cpu) 0x00uy "Adress 0x0001 value should be 0x00"
      Expect.isTrue (hasFlag Flags.Z cpu.P) "Zero flag shoud be set"
    }
    test "INY" {
      let program = [| 0xC8uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with Y = 0x05uy })
      Expect.equal cpu.Y 0x06uy "Register Y should be 0x06"
    }
    test "INY Zero" {
      let program = [| 0xC8uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with Y = 0xFFuy })
      Expect.equal cpu.Y 0x00uy "Register Y value should be 0x00"
      Expect.isTrue (hasFlag Flags.Z cpu.P) "Zero flag shoud be set"
    }
    test "JMP Absolute" {
      let program = [| 0x4Cuy; 0x50uy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> c
                                          |> memWrite 0x1050us 0xC8uy
                                          |> memWrite 0x1051us 0x00uy)
      Expect.equal cpu.Y 0x01uy "Register Y value should be 0x01"
      Expect.equal cpu.PC 0x1052us "Program counter should be 0x1052"
    }
    test "JMP Indirect" {
      let program = [| 0x6Cuy; 0x50uy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> c
                                          |> memWrite 0x1050us 0x12uy
                                          |> memWrite 0x1051us 0x34uy
                                          |> memWrite 0x3412us 0xC8uy
                                          |> memWrite 0x3413us 0x00uy)
      Expect.equal cpu.Y 0x01uy "Register Y value should be 0x01"
      Expect.equal cpu.PC 0x3414us "Program counter should be 0x3414"
    }
    test "JSR" {
      let program = [| 0x20uy; 0x31uy; 0x40uy; 0x00uy |]
      let cpu = runWith program (fun c -> c
                                          |> memWrite 0x4031us 0xE8uy
                                          |> memWrite 0x4032us 0x00uy)
      Expect.equal cpu.X 0x01uy "Register X value should be 0x01"
      Expect.equal cpu.SP 0xFDuy "Stack Pointer should be 0xFD"
      Expect.equal (0x01FEus |> memRead16 cpu) 0x8002us "Value at Memory Address 0x01F should be 0x8002"
    }
    test "RTS" {
      let program = [| 0x60uy; 0x00uy |]
      let cpu = runWith program (fun c -> c
                                          |> memWrite 0x01FEus 0x12uy
                                          |> memWrite 0x01FFus 0x05uy
                                          |> memWrite 0x0513us 0xE8uy
                                          |> memWrite 0x0514us 0x00uy
                                          |> fun c -> { c with SP = 0xFDuy })
      Expect.equal cpu.X 0x01uy "Register X value should be 0x01"
      Expect.equal cpu.SP 0xFFuy "Stack Pointer should be 0xFF"
      Expect.equal cpu.PC 0x0515us "Program counter should be 0x0515us"
    }
    test "JSR and RTS" {
      let program = [| 0x20uy; 0x39uy; 0x20uy; 0x00uy |]
      let cpu = runWith program (fun c -> c
                                          |> memWrite 0x2039us 0xE8uy
                                          |> memWrite 0x203Aus 0x60uy
                                          |> memWrite 0x203Bus 0x00uy)
      Expect.equal cpu.X 0x01uy "Register X value should be 0x01"
      Expect.equal cpu.SP 0xFFuy "Stack Pointer should be 0xFF"
      Expect.equal (0x01FEus |> memRead16 cpu) 0x8002us "Value at Memory Address 0x01FE should be 0x8002"
    }
    test "LDX immidiate load data" {
      let program = [| 0xA2uy; 0x07uy; 0x00uy |] // LDX
      let cpu = runWith program (fun c -> c)
      Expect.equal cpu.X 0x07uy "Accumulator should be 7"
      Expect.isFalse (hasFlag Flags.Z cpu.P) "Zero flag should be false"
      Expect.isFalse (hasFlag Flags.N cpu.P) "Negative flag shoud be false"
    }
    test "LDY immidiate load data" {
      let program = [| 0xA0uy; 0x25uy; 0x00uy |] // LDY
      let cpu = runWith program (fun c -> c)
      Expect.equal cpu.Y 0x25uy "Accumulator should be 0x25"
      Expect.isFalse (hasFlag Flags.Z cpu.P) "Zero flag should be false"
      Expect.isFalse (hasFlag Flags.N cpu.P) "Negative flag shoud be false"
    }
    test "NOP" {
      let program = [| 0xEAuy; 0x00uy |]
      let cpu = runWith program (fun c -> c)
      Expect.equal cpu.PC 0x8002us "Program counter should be 0x8002"
    }
    test "PHA" {
      let program = [| 0x48uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with A = 0x12uy })
      Expect.equal cpu.A 0x12uy "Accumulator should be 0x12"
      Expect.equal cpu.SP 0xFEuy "Stack Pointer should be 0xFE"
      Expect.equal (0x01FFus |> memRead cpu) 0x12uy "Value at Memory Address 0x01FF should be 0x12"
    }
    test "PLA" {
      let program = [| 0x68uy; 0x00uy |]
      let cpu = runWith program (fun c -> c |> memWrite 0x01FFus 0x66uy
                                            |> fun c -> { c with SP = 0xFEuy })
      Expect.equal cpu.A 0x66uy "Accumulator should be 0x66"
      Expect.equal cpu.SP 0xFFuy "Stack Pointer should be 0xFF"
      Expect.isFalse (cpu.P |> hasFlag Flags.Z) "Zero flag should be cleared"
      Expect.isFalse (cpu.P |> hasFlag Flags.N) "Negative flag should be cleared"
    }
    test "PLA zero" {
      let program = [| 0x68uy; 0x00uy |]
      let cpu = runWith program (fun c -> c |> memWrite 0x01FFus 0x00uy
                                            |> fun c -> { c with SP = 0xFEuy })
      Expect.equal cpu.A 0x00uy "Accumulator should be 0x00"
      Expect.equal cpu.SP 0xFFuy "Stack Pointer should be 0xFF"
      Expect.isTrue (cpu.P |> hasFlag Flags.Z) "Zero flag should be set"
      Expect.isFalse (cpu.P |> hasFlag Flags.N) "Negative flag should be cleared"
    }
    test "PHA and PLA" {
      let program = [| 0x48uy; // PHA
                       0xA9uy; 0x11uy; // LDA
                       0x68uy; 0x00uy |] //PLA
      let cpu = runWith program (fun c -> { c with A = 0x98uy })
      Expect.equal cpu.A 0x98uy "Accumulator should be 0x98"
      Expect.equal cpu.SP 0xFFuy "Stack Pointer should be 0xFF"
      Expect.isFalse (cpu.P |> hasFlag Flags.Z) "Zero flag should be cleared"
      Expect.isTrue (cpu.P |> hasFlag Flags.N) "Negative flag should be set"
      Expect.equal cpu.PC 0x8005us "Program counter should be 0x8005"
    }
    test "PHP" {
      let program = [| 0x08uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with P = Flags.C ||| Flags.V })
      Expect.equal cpu.SP 0xFEuy "Stack Pointer should be 0xFE"
      Expect.isTrue (0x01FFus |> memRead cpu |> hasFlag Flags.C) "Carry flag should be set"
      Expect.isTrue (0x01FFus |> memRead cpu |> hasFlag Flags.V) "Overflow flag should be set"
    }
    test "PLP" {
      let program = [| 0x28uy; 0x00uy |]
      let cpu = runWith program (fun c -> c |> memWrite 0x01FFus (Flags.N ||| Flags.Z)
                                            |> fun c -> { c with SP = 0xFEuy })
      Expect.equal cpu.SP 0xFFuy "Stack Pointer should be 0xFF"
      Expect.isTrue (cpu.P |> hasFlag Flags.Z) "Zero flag should be set"
      Expect.isTrue (cpu.P |> hasFlag Flags.N) "Negative flag should be cleared"
    }
    test "PHP and PLP" {
      let program = [| 0x08uy; // PHA
                       0xA9uy; 0x21uy; // LDA
                       0x28uy; 0x00uy |] //PLA
      let cpu = runWith program (fun c -> { c with P = Flags.V ||| Flags.Z })
      Expect.equal cpu.A 0x21uy "Accumulator should be 0x21"
      Expect.equal cpu.SP 0xFFuy "Stack Pointer should be 0xFF"
      Expect.isTrue (cpu.P |> hasFlag Flags.Z) "Zero flag should be set"
      Expect.isTrue (cpu.P |> hasFlag Flags.V) "Overflow flag should be set"
      Expect.equal cpu.PC 0x8005us "Program counter should be 0x8005"
    }
    test "STX to memory zero page" {
      let program = [| 0x86uy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with X = 0xBBuy })

      Expect.equal (0x10us |> memRead cpu) 0xBBuy "Memory Adress 0x0010 value should be 0xBB"
    }
    test "STY to memory zero page" {
      let program = [| 0x84uy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with Y = 0xBCuy })

      Expect.equal (0x10us |> memRead cpu) 0xBCuy "Memory Adress 0x0010 value should be 0xBC"
    }
    test "TAY" {
      let program = [| 0xA8uy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with A = 0xBDuy })

      Expect.equal cpu.Y 0xBDuy "Y Register should be 0xBD"
    }
    test "TXA" {
      let program = [| 0x8Auy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with X = 0xBFuy })

      Expect.equal cpu.A 0xBFuy "A Register should be 0xBF"
    }
    test "TYA" {
      let program = [| 0x98uy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with Y = 0xC1uy })

      Expect.equal cpu.A 0xC1uy "A Register should be 0xC1"
    }
    test "TSX" {
      let program = [| 0xBAuy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with SP = 0xE1uy })

      Expect.equal cpu.X 0xE1uy "X Register should be 0xE1"
      Expect.isTrue (hasFlag Flags.N cpu.P) "Negative flag should be set"
    }
    test "TXS" {
      let program = [| 0x9Auy; 0x10uy; 0x00uy |]
      let cpu = runWith program (fun c -> { c with X = 0xE3uy })

      Expect.equal cpu.SP 0xE3uy "Stack Pointer should be 0xE3"
    }
  ]
  *)
