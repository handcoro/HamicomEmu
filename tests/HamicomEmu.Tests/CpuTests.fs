module CpuTests

open Expecto
open HamicomEmu.Bus
open HamicomEmu.Cartridge
open HamicomEmu.Cpu
open HamicomEmu.Common.BitUtils

module Flags = Cpu.Flags

type CpuTestState = { cpu: Cpu.CpuState; bus: Bus.BusState }

let withCpu f state = { state with cpu = f state.cpu }

let private programWithResetVector (program: byte array) =
    let rom = Array.zeroCreate 0x8000
    Array.blit program 0 rom 0 program.Length
    rom[0x7FFC] <- 0x00uy
    rom[0x7FFD] <- 0x80uy
    rom

let initState program =
    let cart = testCartridge (programWithResetVector program)
    let cpu, bus = Cpu.powerOn cart
    let cpu, bus = Cpu.reset cpu bus
    { cpu = cpu; bus = bus }

let memWrite addr value state = {
    state with
        bus = Bus.memWrite addr value state.bus
}

let memRead state addr = Bus.memRead addr state.bus |> fst

let memRead16 state addr = Bus.memRead16 addr state.bus |> fst

let private stepOnce state =
    let cpu, bus, _ = Cpu.step state.cpu state.bus
    { state with cpu = cpu; bus = bus }

let run state =
    let rec loop guard state =
        if guard = 0 then
            state
        else
            let opcode, _ = Bus.memRead state.cpu.pc state.bus

            if opcode = 0x00uy then
                // Treat BRK as a test terminator without invoking the interrupt handler.
                {
                    state with
                        cpu = {
                            state.cpu with
                                pc = state.cpu.pc + 1us
                        }
                }
            else
                loop (guard - 1) (stepOnce state)

    loop 10000 state

let runWith program setup = initState program |> setup |> run

let cpuTests =
    testList "CPU Tests" [
        test "Reset" {
            let program = [| 0x00uy |]
            let result = initState program

            Expect.equal result.cpu.pc 0x8000us "Program counter should be 0x8000"
        }

        test "LDA immediate load data" {
            let program = [| 0xA9uy; 0x05uy; 0x00uy |] // LDA 05 BRK
            let result = initState program |> run

            Expect.equal result.cpu.a 0x05uy "Accumulator should be 5"
            Expect.isFalse (result.cpu.p &&& Flags.Z <> 0b00uy) "Zero flag should be false"
            Expect.isFalse (result.cpu.p &&& Flags.N <> 0uy) "Negative flag should be false"
        }

        test "LDA zero flag" {
            let program = [| 0xA9uy; 0x00uy; 0x00uy |] // LDA 00 BRK
            let result = runWith program <| withCpu (fun c -> { c with a = 0xFFuy })

            Expect.equal result.cpu.a 0x00uy "A should be 0"
            Expect.isTrue (result.cpu.p &&& Flags.Z <> 0uy) "Z flag should be true"
            Expect.isFalse (result.cpu.p &&& Flags.N <> 0uy) "N flag should be false"
        }

        test "LDA negative flag" {
            let program = [| 0xA9uy; 0x80uy; 0x00uy |] // LDA 80 BRK
            let result = initState program |> run

            Expect.equal result.cpu.a 0x80uy "Accumulator A should be 0x80"
            Expect.isFalse (result.cpu.p &&& Flags.Z <> 0b00uy) "Zero flag should be false"
            Expect.isTrue (result.cpu.p &&& Flags.N <> 0uy) "Negative flag should be true"
        }

        test "TAX move A to X" {
            let program = [| 0xAAuy; 0x00uy |] // TAX BRK
            let result = runWith program <| withCpu (fun c -> { c with a = byte 10 })

            Expect.equal result.cpu.x (byte 10) "Register X should be 10"
        }

        test "Text 5 ops working together" {
            let program = [| 0xA9uy; 0xC0uy; 0xAAuy; 0xE8uy; 0x00uy |]
            let result = initState program |> run

            Expect.equal result.cpu.x 0xC1uy "X should be 0XC1"
        }

        test "INX overflow" {
            let program = [| 0xE8uy; 0xE8uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with x = 0xFFuy })

            Expect.equal result.cpu.x 0x01uy "X should be 0x01"
        }

        test "LDA from memory" {
            let program = [| 0xA5uy; 0x10uy; 0x00uy |]
            let result = runWith program <| memWrite 0x10us 0x55uy

            Expect.equal result.cpu.a 0x55uy "A should be 0x55"
        }

        test "LDA from memory zero page X" {
            let program = [| 0xB5uy; 0x10uy; 0x00uy |]

            let result =
                runWith program
                <| (memWrite 0x11us 0x56uy >> withCpu (fun c -> { c with x = 0x01uy }))

            Expect.equal result.cpu.a 0x56uy "A should be 0x56"
        }

        test "LDA from memory absolute" {
            let program = [| 0xADuy; 0x10uy; 0x04uy; 0x00uy |]
            let result = runWith program <| memWrite 0x0410us 0x57uy

            Expect.equal result.cpu.a 0x57uy "A should be 0x57"
        }

        test "LDA from memory absolute X" {
            let program = [| 0xBDuy; 0x10uy; 0x04uy; 0x00uy |]

            let result =
                runWith program
                <| (memWrite 0x0415us 0x58uy >> withCpu (fun c -> { c with x = 0x05uy }))

            Expect.equal result.cpu.a 0x58uy "A should be 0x58"
        }

        test "LDA from memory absolute Y" {
            let program = [| 0xB9uy; 0x10uy; 0x04uy; 0x00uy |]

            let result =
                runWith program
                <| (memWrite 0x0418us 0x59uy >> withCpu (fun c -> { c with y = 0x08uy }))

            Expect.equal result.cpu.a 0x59uy "A should be 0x59"
        }

        test "LDA from memory indirect X" {
            let program = [| 0xA1uy; 0x10uy; 0x00uy |]

            let result =
                runWith program
                <| (memWrite 0x18us 0x03uy
                    >> memWrite 0x19us 0x05uy
                    >> memWrite 0x0503us 0x5Auy
                    >> withCpu (fun c -> { c with x = 0x08uy }))

            Expect.equal result.cpu.a 0x5Auy "A should be 0x5A"
        }

        test "LDA from memory indirect Y" {
            let program = [| 0xB1uy; 0x10uy; 0x00uy |]

            let result =
                runWith program
                <| (memWrite 0x10us 0x04uy
                    >> memWrite 0x11us 0x05uy
                    >> memWrite 0x0514us 0x5Buy
                    >> withCpu (fun c -> { c with y = 0x10uy }))

            Expect.equal result.cpu.a 0x5Buy "A should be 0x5B"
        }

        test "STA to memory zero page X" {
            let program = [| 0x85uy; 0x10uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0xBAuy })

            Expect.equal (0x10us |> memRead result) 0xBAuy "Memory at 0x10 should be 0xBA"
        }

        test "ADC no carry" {
            let program = [| 0x69uy; 0x10uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0x0Auy })

            Expect.equal result.cpu.a 0x1Auy "A should be 0x1A"
            Expect.equal (result.cpu.p &&& Flags.C) 0uy "Carry bit should be 0"
        }

        test "ADC has carry" {
            let program = [| 0x69uy; 0x10uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0x0Auy; p = 0x01uy })

            Expect.equal result.cpu.a 0x1Buy "A should be 0x1B"
            Expect.equal (result.cpu.p &&& Flags.C) 0uy "Carry bit should be 0"
        }

        test "ADC occur carry" {
            let program = [| 0x69uy; 0x02uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0xFFuy })

            Expect.equal result.cpu.a 0x01uy "A should be 0x01"
            Expect.equal (result.cpu.p &&& Flags.C) 1uy "Carry bit should be 1"
        }

        test "ADC occur carry and zero" {
            let program = [| 0x69uy; 0x01uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0xFFuy })

            Expect.equal result.cpu.a 0x00uy "A should be 0x00"
            Expect.equal (result.cpu.p &&& Flags.C > 0uy) true "Carry flag should be true"
            Expect.equal (result.cpu.p &&& Flags.Z > 0uy) true "Zero flag should be true"
        }

        test "ADC occur overflow plus" {
            let program = [| 0x69uy; 0x10uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0x7Fuy })

            Expect.equal result.cpu.a 0x8Fuy "A should be 0x8F"
            Expect.equal (result.cpu.p &&& Flags.V > 0uy) true "Overflow flag should be true"
            Expect.equal (result.cpu.p &&& Flags.N > 0uy) true "Negative flag should be true"
        }

        test "ADC occur overflow plus with carry" {
            let program = [| 0x69uy; 0x10uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0x6Fuy; p = 0x01uy })

            Expect.equal result.cpu.a 0x80uy "A should be 0x80"
            Expect.equal (result.cpu.p &&& Flags.C > 0uy) false "Carry flag should be false"
            Expect.equal (result.cpu.p &&& Flags.V > 0uy) true "Overflow flag should be true"
            Expect.equal (result.cpu.p &&& Flags.N > 0uy) true "Negative flag should be true"
        }

        test "ADC occur overflow minus" {
            let program = [| 0x69uy; 0x81uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0x81uy })

            Expect.equal result.cpu.a 0x02uy "A should be 0x02"
            Expect.equal (result.cpu.p &&& Flags.C > 0uy) true "Carry flag should be true"
            Expect.equal (result.cpu.p &&& Flags.V > 0uy) true "Overflow flag should be true"
        }

        test "ADC overflow minul with carry" {
            let program = [| 0x69uy; 0x80uy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0x80uy; p = 0x01uy })

            Expect.equal result.cpu.a 0x01uy "A should be 0x01"
            Expect.equal (result.cpu.p &&& Flags.C > 0uy) true "Carry flag should be true"
            Expect.equal (result.cpu.p &&& Flags.V > 0uy) true "Overflow flag should be true"
        }

        test "ADC no overflow" {
            let program = [| 0x69uy; 0x7Fuy; 0x00uy |]
            let result = runWith program <| withCpu (fun c -> { c with a = 0x83uy })

            Expect.equal (result.cpu.p &&& Flags.C > 0uy) true "Carry flag should be true"
            Expect.equal (result.cpu.p &&& Flags.V > 0uy) false "Overflow flag should be false"
        }

        test "SBC immediate subtract data" {
            let program = [|
                0xA9uy
                0x10uy // LDA #$10
                0xE9uy
                0x05uy // SBC #$05
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.C c.p }) // キャリーフラグをセット（デフォルトで必要）

            Expect.equal cpu.cpu.a 0x0Buy "Accumulator should be 0x0B (16 - 5)"
            Expect.isFalse (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be false"
            Expect.isFalse (hasFlag Flags.N cpu.cpu.p) "Negative flag should be false"
        }

        test "SBC with borrow (Carry clear)" {
            let program = [| 0xE9uy; 0x01uy; 0x00uy |]

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x03uy; p = 0uy }) // Carry=0 → 実質 -2

            let result = runWith program setup

            Expect.equal result.cpu.a 0x01uy "A should be 0x01 (3 - 1 - 1)"
        }

        test "SBC result zero" {
            let program = [| 0xE9uy; 0x01uy; 0x00uy |]

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x02uy; p = 0uy }) // 2 - 1 - 1 = 0

            let result = runWith program setup

            Expect.equal result.cpu.a 0x00uy "Result should be zero"
            Expect.isTrue (result.cpu.p &&& Flags.Z <> 0uy) "Zero flag should be set"
        }

        test "SBC negative result" {
            let program = [| 0xE9uy; 0x03uy; 0x00uy |]

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x01uy; p = Flags.C }) // 1 - 3 = -2

            let result = runWith program setup

            Expect.equal result.cpu.a 0xFEuy "A should wrap around to 0xFE"
            Expect.isTrue (result.cpu.p &&& Flags.N <> 0uy) "Negative flag should be set"
        }

        test "SBC signed overflow positive to negative" {
            let program = [| 0xE9uy; 0x7Fuy; 0x00uy |] // SBC #$7F

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x80uy; p = Flags.C }) // 128 - 127 = 1, 符号変化あり

            let result = runWith program setup

            Expect.equal result.cpu.a 0x01uy "A should be 1"
            Expect.isTrue (result.cpu.p &&& Flags.V <> 0uy) "Overflow flag should be set"
        }

        test "SBC signed overflow negative to positive" {
            let program = [| 0xE9uy; 0x80uy; 0x00uy |] // SBC #$80

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x7Fuy; p = Flags.C }) // 127 - 128 = -1 (→ 0xFF)

            let result = runWith program setup

            Expect.equal result.cpu.a 0xFFuy "A should be 0xFF (i.e., -1)"
            Expect.isTrue (result.cpu.p &&& Flags.V <> 0uy) "Overflow flag should be set"
            Expect.isTrue (result.cpu.p &&& Flags.N <> 0uy) "Negative flag should be set"
        }

        test "SBC no borrow, carry remains set" {
            let program = [| 0xE9uy; 0x01uy; 0x00uy |] // SBC #$01

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x03uy; p = Flags.C }) // 3 - 1 = 2

            let result = runWith program setup

            Expect.equal result.cpu.a 0x02uy "A should be 2"
            Expect.isTrue (result.cpu.p &&& Flags.C <> 0uy) "Carry should remain set (no borrow)"
            Expect.isFalse (result.cpu.p &&& Flags.Z <> 0uy) "Zero should not be set"
        }

        test "SBC borrow occurs, carry cleared" {
            let program = [| 0xE9uy; 0x04uy; 0x00uy |] // SBC #$04

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x03uy; p = Flags.C }) // 3 - 4 = -1 → 255, borrow

            let result = runWith program setup

            Expect.equal result.cpu.a 0xFFuy "A should be 0xFF (-1)"
            Expect.isFalse (result.cpu.p &&& Flags.C <> 0uy) "Carry should be cleared (borrow occurred)"
            Expect.isTrue (result.cpu.p &&& Flags.N <> 0uy) "Negative flag should be set"
        }

        test "SBC with carry clear subtracts extra one" {
            let program = [| 0xE9uy; 0x01uy; 0x00uy |] // SBC #$01

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x03uy; p = 0uy }) // Carry clear → 3 - 1 - 1 = 1

            let result = runWith program setup

            Expect.equal result.cpu.a 0x01uy "A should be 1 due to extra borrow"
            Expect.isTrue (result.cpu.p &&& Flags.C <> 0uy) "Carry should be set (no final borrow)"
        }

        test "SBC result zero with carry set" {
            let program = [| 0xE9uy; 0x01uy; 0x00uy |] // SBC #$01

            let setup state =
                state |> withCpu (fun c -> { c with a = 0x02uy; p = 0uy }) // Carry clear → 2 - 1 - 1 = 0

            let result = runWith program setup

            Expect.equal result.cpu.a 0x00uy "A should be 0"
            Expect.isTrue (result.cpu.p &&& Flags.Z <> 0uy) "Zero flag should be set"
            Expect.isTrue (result.cpu.p &&& Flags.C <> 0uy) "Carry should be set (no final borrow)"
        }

        test "AND immediate" {
            let program = [|
                0x29uy
                0x03uy // AND
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x01uy })

            Expect.equal cpu.cpu.a 0x01uy "Accumulator should be 0x01"
        }

        test "EOR immediate" {
            let program = [|
                0x49uy
                0x06uy // EOR
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x04uy })

            Expect.equal cpu.cpu.a 0x02uy "Accumulator should be 0x02"
        }

        test "ORA immediate" {
            let program = [|
                0x09uy
                0b1010uy // ORA
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0b0110uy })

            Expect.equal cpu.cpu.a 0x0Euy "Accumulator should be 0x0E"
        }

        test "ASL zero page" {
            let program = [|
                0x06uy
                0x03uy // ASL $03
                0x00uy
            |] // BRK

            let cpu = runWith program <| memWrite 0x03us 0x0Auy

            Expect.equal (memRead cpu 0x03us) 0x14uy "Memory at $03 should be 0x14"
        }

        test "ASL accumulator" {
            let program = [|
                0x0Auy // ASL
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0b0110uy })

            Expect.equal cpu.cpu.a 0b1100uy "Accumulator should be 0b1100"
        }

        test "LSR zero page" {
            let program = [|
                0x46uy
                0x03uy // LSR $03
                0x00uy
            |] // BRK

            let cpu = runWith program <| memWrite 0x03us 0b1000_0010uy

            Expect.equal (memRead cpu 0x03us) 0b0100_0001uy "Memory at $03 should be 0b0100_0001"
            Expect.equal (cpu.cpu.p &&& Flags.C) 0uy "Carry should be cleared"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.equal (cpu.cpu.p &&& Flags.N) 0uy "Negative flag should not be set"
        }

        test "LSR accumulator" {
            let program = [|
                0x4Auy // LSR A
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0b0000_0011uy })

            Expect.equal cpu.cpu.a 0b0000_0001uy "Accumulator should be 0b0000_0001"
            Expect.equal (cpu.cpu.p &&& Flags.C) Flags.C "Carry should be set (bit 0 was 1)"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.equal (cpu.cpu.p &&& Flags.N) 0uy "Negative flag should not be set"
        }

        test "ASL accumulator sets carry when MSB is 1" {
            let program = [|
                0x0Auy // ASL A
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0b1000_0000uy })

            Expect.equal cpu.cpu.a 0b0000_0000uy "Accumulator should be 0 after shift"
            Expect.equal (cpu.cpu.p &&& Flags.C) Flags.C "Carry should be set"
            Expect.equal (cpu.cpu.p &&& Flags.Z) Flags.Z "Zero flag should be set"
            Expect.equal (cpu.cpu.p &&& Flags.N) 0uy "Negative flag should not be set"
        }

        test "LSR accumulator clears carry when LSB is 0" {
            let program = [|
                0x4Auy // LSR A
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0b0000_0010uy })

            Expect.equal cpu.cpu.a 0b0000_0001uy "Accumulator should be 0b0000_0001"
            Expect.equal (cpu.cpu.p &&& Flags.C) 0uy "Carry should be cleared"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.equal (cpu.cpu.p &&& Flags.N) 0uy "Negative flag should not be set"
        }

        test "ROL accumulator with no carry" {
            let program = [|
                0x2Auy // ROL A
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0b0100_0011uy })

            Expect.equal cpu.cpu.a 0b1000_0110uy "Accumulator should be 0b1000_0110"
            Expect.equal (cpu.cpu.p &&& Flags.C) 0uy "Carry should be cleared"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "ROL accumulator with carry" {
            let program = [|
                0x2Auy // ROL A
                0x00uy
            |] // BRK

            let cpu =
                runWith program
                <| withCpu (fun c -> {
                    c with
                        a = 0b0100_0011uy
                        p = Flags.C
                })

            Expect.equal cpu.cpu.a 0b1000_0111uy "Accumulator should be 0b1000_0111"
            Expect.equal (cpu.cpu.p &&& Flags.C) 0uy "Carry should be cleared"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.isTrue (hasFlag cpu.cpu.p Flags.N) "Negative flag should be set"
        }
        test "ROL accumulator occur carry" {
            let program = [|
                0x2Auy // ROL A
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0b1100_0011uy })
            Expect.equal cpu.cpu.a 0b1000_0110uy "Accumulator should be 0b1000_0110"
            Expect.isTrue (hasFlag cpu.cpu.p Flags.C) "Carry should be set"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.isTrue (hasFlag cpu.cpu.p Flags.N) "Negative flag should be set"
        }
        test "ROL zeropage occur carry" {
            let program = [|
                0x26uy
                0x04uy // ROL $04
                0x00uy
            |] // BRK

            let cpu = runWith program <| memWrite 0x04us 0b1000_0010uy
            Expect.equal (memRead cpu 0x04us) 0b0000_0100uy "Memory at $04 should be 0b0000_0100"
            Expect.isTrue (hasFlag cpu.cpu.p Flags.C) "Carry should be set"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.isFalse (hasFlag Flags.N cpu.cpu.p) "Negative flag should not be set"
        }
        test "ROL accumulator zero flag" {
            let program = [|
                0x2Auy // ROL A
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0uy })

            Expect.equal cpu.cpu.a 0uy "Accumulator should be 0"
            Expect.equal (cpu.cpu.p &&& Flags.C) 0uy "Carry should be cleared"
            Expect.isTrue (hasFlag cpu.cpu.p Flags.Z) "Zero flag should be set"
            Expect.isFalse (hasFlag cpu.cpu.p Flags.N) "Negative flag should not be set"
        }
        test "ROR accumulator with carry" {
            let program = [|
                0x6Auy // ROR
                0x00uy
            |] // BRK

            let cpu =
                runWith program
                <| withCpu (fun c -> {
                    c with
                        a = 0b0100_0010uy
                        p = Flags.C
                })

            Expect.equal cpu.cpu.a 0b1010_0001uy "Accumulator should be 0b1010_0001"
            Expect.equal (cpu.cpu.p &&& Flags.C) 0uy "Carry should be cleared"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "ROR zeropage occur carry" {
            let program = [|
                0x66uy
                0x1Auy // ROR $1A
                0x00uy
            |] // BRK

            let cpu = runWith program <| memWrite 0x1Aus 0b1000_0011uy
            Expect.equal (memRead cpu 0x1Aus) 0b0100_0001uy "Memory at $1A should be 0b0100_0001"
            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry should be set"
            Expect.equal (cpu.cpu.p &&& Flags.Z) 0uy "Zero flag should not be set"
            Expect.isFalse (hasFlag Flags.N cpu.cpu.p) "Negative flag should not be set"
        }
        test "BCC with no carry" {
            let program = [|
                0x90uy
                0x02uy // BCC +2
                0x00uy
                0x00uy // BRK
                0xE8uy
                0x00uy
            |] // INX BRK

            let cpu = runWith program <| withCpu (fun c -> { c with p = 0uy })
            Expect.equal cpu.cpu.x 0x01uy "X should be 1"
            Expect.equal cpu.cpu.pc 0x8006us "Program counter should be 0x8006"
        }
        test "BCC with carry" {
            let program = [|
                0x90uy
                0x02uy // BCC +2
                0x00uy
                0x00uy // BRK
                0xE8uy
                0x00uy
            |] // INX BRK

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.C c.p })
            Expect.equal cpu.cpu.x 0x00uy "X should be 0"
            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
            Expect.equal cpu.cpu.pc 0x8003us "Program counter should be 0x8003"
        }
        test "BCS with no carry" {
            let program = [|
                0xB0uy
                0x02uy // BCS +2
                0x00uy
                0x00uy // BRK
                0xE8uy
                0x00uy
            |] // INX BRK

            let cpu = runWith program <| withCpu (fun c -> { c with p = 0uy })
            Expect.equal cpu.cpu.x 0x00uy "X should be 0"
            Expect.equal cpu.cpu.pc 0x8003us "Program counter should be 0x8003"
        }
        test "BCS with carry" {
            let program = [|
                0xB0uy
                0x02uy // BCS +2
                0x00uy
                0x00uy // BRK
                0xE8uy
                0x00uy
            |] // INX BRK

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.C c.p })
            Expect.equal cpu.cpu.x 0x01uy "X should be 1"
            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
            Expect.equal cpu.cpu.pc 0x8006us "Program counter should be 0x8006"
        }
        test "BCC negative offset" {
            let program = [|
                0x18uy // CLC (Clear Carry)
                0xE8uy // INX (at 0x8001)
                0xE0uy // CPX
                0x02uy // #2
                0x90uy // BCC
                0xFBuy // -5 (jumps back to 0x8001)
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with x = 0uy })

            Expect.equal cpu.cpu.x 0x02uy "X should be 2 (incremented twice due to loop)"
            Expect.equal cpu.cpu.pc 0x8007us "Program counter should be 0x8007"
        }
        test "BEQ with zero flag" {
            let program = [|
                0xF0uy
                0x02uy // BEQ +2
                0x00uy
                0x00uy // BRK
                0xE8uy
                0x00uy
            |] // INX BRK

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.Z c.p })
            Expect.equal cpu.cpu.x 0x01uy "X should be 1"
            Expect.isFalse (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be cleared"
            Expect.equal cpu.cpu.pc 0x8006us "Program counter should be 0x8006"
        }
        test "BNE with zero flag" {
            let program = [|
                0xD0uy
                0x02uy // BNE +2
                0x00uy
                0x00uy // BRK
                0xE8uy
                0x00uy
            |] // INX BRK

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.Z c.p })
            Expect.equal cpu.cpu.x 0x00uy "X should be 0"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
            Expect.equal cpu.cpu.pc 0x8003us "Program counter should be 0x8003"
        }
        test "BIT absolute all zero" {
            let program = [|
                0x2Cuy
                0x00uy // BIT $0000
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0uy })
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
            Expect.isFalse (hasFlag Flags.V cpu.cpu.p) "Overflow flag should be cleared"
            Expect.isFalse (hasFlag Flags.N cpu.cpu.p) "Negative flag should be cleared"
        }
        test "BIT absolute occur VN" {
            let program = [|
                0x2Cuy
                0x00uy // BIT $0000
                0x00uy
            |] // BRK

            let cpu =
                runWith program
                <| (memWrite 0x0000us 0xFFuy >> withCpu (fun c -> { c with a = 0xFFuy }))

            Expect.isFalse (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be cleared"
            Expect.isTrue (hasFlag Flags.V cpu.cpu.p) "Overflow flag should be set"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "BIT zeropage all zero" {
            let program = [|
                0x24uy
                0x02uy // BIT $02
                0x00uy
            |] // BRK

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0uy })
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
            Expect.isFalse (hasFlag Flags.V cpu.cpu.p) "Overflow flag should be cleared"
            Expect.isFalse (hasFlag Flags.N cpu.cpu.p) "Negative flag should be cleared"
        }
        test "CLC" {
            let program = [| 0x18uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.C ||| Flags.N })
            Expect.isFalse (hasFlag Flags.C cpu.cpu.p) "Carry flag should be cleared"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "SEC" {
            let program = [| 0x38uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.N })
            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "CLD" {
            let program = [| 0xD8uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.D ||| Flags.N })
            Expect.isFalse (hasFlag Flags.D cpu.cpu.p) "Decimal Mode flag should be cleared"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "SED" {
            let program = [| 0xF8uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.N })
            Expect.isTrue (hasFlag Flags.D cpu.cpu.p) "Decimal Mode flag should be set"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "CLI" {
            let program = [| 0x58uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.I ||| Flags.N })
            Expect.isFalse (hasFlag Flags.I cpu.cpu.p) "Interrupt Disable flag should be cleared"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "SEI" {
            let program = [| 0x78uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.N })
            Expect.isTrue (hasFlag Flags.I cpu.cpu.p) "Interrupt Disable flag should be set"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "CLV" {
            let program = [| 0xB8uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.V ||| Flags.N })
            Expect.isFalse (hasFlag Flags.V cpu.cpu.p) "Overflow flag should be cleared"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "CMP" {
            let program = [| 0xC9uy; 0x01uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x02uy })
            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
        }
        test "CMP Equal" {
            let program = [| 0xC9uy; 0x02uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x02uy })
            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }
        test "CMP Negative" {
            let program = [| 0xC9uy; 0x03uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x02uy })
            Expect.isFalse (hasFlag Flags.C cpu.cpu.p) "Carry flag should be cleared"
            Expect.isFalse (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be cleared"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "CPX" {
            let program = [| 0xE0uy; 0x01uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0x02uy })
            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
        }
        test "CPY" {
            let program = [| 0xC0uy; 0x01uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0x02uy })
            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
        }
        test "DEC zeropage" {
            let program = [| 0xC6uy; 0x01uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x0001us 0x05uy
            Expect.equal (0x0001us |> memRead cpu) 0x04uy "Address 0x0001 value should be 0x04"
        }
        test "DEC Negative" {
            let program = [| 0xC6uy; 0x01uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x0001us 0x00uy
            Expect.equal (0x0001us |> memRead cpu) 0xFFuy "Address 0x0001 value should be 0xFF"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "DEX" {
            let program = [| 0xCAuy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0x05uy })
            Expect.equal cpu.cpu.x 0x04uy "Register X should be 0x04"
        }
        test "DEX Negative" {
            let program = [| 0xCAuy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0x00uy })
            Expect.equal cpu.cpu.x 0xFFuy "Register X value should be 0xFF"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "DEY" {
            let program = [| 0x88uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0x05uy })
            Expect.equal cpu.cpu.y 0x04uy "Register Y should be 0x04"
        }
        test "DEY Negative" {
            let program = [| 0x88uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0x00uy })
            Expect.equal cpu.cpu.y 0xFFuy "Register Y value should be 0xFF"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "INC zeropage" {
            let program = [| 0xE6uy; 0x01uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x0001us 0x05uy
            Expect.equal (0x0001us |> memRead cpu) 0x06uy "Address 0x0001 value should be 0x06"
        }
        test "INC Zero flag" {
            let program = [| 0xE6uy; 0x01uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x0001us 0xFFuy
            Expect.equal (0x0001us |> memRead cpu) 0x00uy "Address 0x0001 value should be 0x00"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }
        test "INY" {
            let program = [| 0xC8uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0x05uy })
            Expect.equal cpu.cpu.y 0x06uy "Register Y should be 0x06"
        }
        test "INY Zero" {
            let program = [| 0xC8uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0xFFuy })
            Expect.equal cpu.cpu.y 0x00uy "Register Y value should be 0x00"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }
        test "JMP Absolute" {
            let program = [| 0x4Cuy; 0x50uy; 0x10uy; 0x00uy |]

            let cpu = runWith program <| (memWrite 0x1050us 0xC8uy >> memWrite 0x1051us 0x00uy)

            Expect.equal cpu.cpu.y 0x01uy "Register Y value should be 0x01"
            Expect.equal cpu.cpu.pc 0x1052us "Program counter should be 0x1052"
        }
        test "JMP Indirect" {
            let program = [| 0x6Cuy; 0x50uy; 0x02uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x0250us 0x12uy
                    >> memWrite 0x0251us 0x03uy
                    >> memWrite 0x0312us 0xC8uy
                    >> memWrite 0x0313us 0x00uy)

            Expect.equal cpu.cpu.y 0x01uy "Register Y value should be 0x01"
            Expect.equal cpu.cpu.pc 0x0314us "Program counter should be 0x0314"
        }
        test "JSR" {
            let program = [| 0x20uy; 0x04uy; 0x80uy; 0x00uy; 0xE8uy; 0x00uy |]

            let cpu = runWith program <| id

            Expect.equal cpu.cpu.x 0x01uy "Register X value should be 0x01"
            Expect.equal cpu.cpu.sp 0xF8uy "Stack Pointer should be 0xF8"
            Expect.equal (0x01F9us |> memRead16 cpu) 0x8002us "Value at Memory Address 0x01F9 should be 0x8002"
        }
        test "RTS" {
            let program =
                Array.concat [
                    [| 0x60uy; 0x00uy |] // RTS at 0x8000
                    Array.zeroCreate<byte> 0x11 // Padding to 0x8013
                    [| 0xE8uy; 0x00uy |] // At 0x8013: INX, BRK
                ]

            let cpu =
                runWith program
                <| (memWrite 0x01FEus 0x12uy
                    >> memWrite 0x01FFus 0x80uy
                    >> withCpu (fun c -> { c with sp = 0xFDuy }))

            Expect.equal cpu.cpu.x 0x01uy "Register X value should be 0x01"
            Expect.equal cpu.cpu.sp 0xFFuy "Stack Pointer should be 0xFF"
            Expect.equal cpu.cpu.pc 0x8015us "Program counter should be 0x8015"
        }
        test "JSR and RTS" {
            let program = [| 0x20uy; 0x04uy; 0x80uy; 0x00uy; 0xE8uy; 0x60uy; 0x00uy |]

            let cpu = runWith program <| id

            Expect.equal cpu.cpu.x 0x01uy "Register X value should be 0x01"
            Expect.equal cpu.cpu.sp 0xFAuy "Stack Pointer should be 0xFA"
            Expect.equal (0x01F9us |> memRead16 cpu) 0x8002us "Value at Memory Address 0x01F9 should be 0x8002"
        }
        test "LDX immediate load data" {
            let program = [| 0xA2uy; 0x07uy; 0x00uy |] // LDX
            let cpu = runWith program id
            Expect.equal cpu.cpu.x 0x07uy "X register should be 7"
            Expect.isFalse (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be false"
            Expect.isFalse (hasFlag Flags.N cpu.cpu.p) "Negative flag should be false"
        }
        test "LDY immediate load data" {
            let program = [| 0xA0uy; 0x25uy; 0x00uy |] // LDY
            let cpu = runWith program id
            Expect.equal cpu.cpu.y 0x25uy "Y register should be 0x25"
            Expect.isFalse (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be false"
            Expect.isFalse (hasFlag Flags.N cpu.cpu.p) "Negative flag should be false"
        }
        test "NOP" {
            let program = [| 0xEAuy; 0x00uy |]
            let cpu = runWith program id
            Expect.equal cpu.cpu.pc 0x8002us "Program counter should be 0x8002"
        }
        test "PHA" {
            let program = [| 0x48uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x12uy })
            Expect.equal cpu.cpu.a 0x12uy "Accumulator should be 0x12"
            Expect.equal cpu.cpu.sp 0xF9uy "Stack Pointer should be 0xF9"
            Expect.equal (0x01FAus |> memRead cpu) 0x12uy "Value at Memory Address 0x01FA should be 0x12"
        }
        test "PLA" {
            let program = [| 0x68uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x01FFus 0x66uy >> withCpu (fun c -> { c with sp = 0xFEuy }))

            Expect.equal cpu.cpu.a 0x66uy "Accumulator should be 0x66"
            Expect.equal cpu.cpu.sp 0xFFuy "Stack Pointer should be 0xFF"
            Expect.isFalse (cpu.cpu.p |> hasFlag Flags.Z) "Zero flag should be cleared"
            Expect.isFalse (cpu.cpu.p |> hasFlag Flags.N) "Negative flag should be cleared"
        }
        test "PLA zero" {
            let program = [| 0x68uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x01FFus 0x00uy >> withCpu (fun c -> { c with sp = 0xFEuy }))

            Expect.equal cpu.cpu.a 0x00uy "Accumulator should be 0x00"
            Expect.equal cpu.cpu.sp 0xFFuy "Stack Pointer should be 0xFF"
            Expect.isTrue (cpu.cpu.p |> hasFlag Flags.Z) "Zero flag should be set"
            Expect.isFalse (cpu.cpu.p |> hasFlag Flags.N) "Negative flag should be cleared"
        }
        test "PHA and PLA" {
            let program = [|
                0x48uy // PHA
                0xA9uy
                0x11uy // LDA #$11
                0x68uy // PLA
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x98uy })
            Expect.equal cpu.cpu.a 0x98uy "Accumulator should be 0x98"
            Expect.equal cpu.cpu.sp 0xFAuy "Stack Pointer should be 0xFA"
            Expect.isFalse (cpu.cpu.p |> hasFlag Flags.Z) "Zero flag should be cleared"
            Expect.isTrue (cpu.cpu.p |> hasFlag Flags.N) "Negative flag should be set"
            Expect.equal cpu.cpu.pc 0x8005us "Program counter should be 0x8005"
        }
        test "PHP" {
            let program = [| 0x08uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.C ||| Flags.V })
            Expect.equal cpu.cpu.sp 0xF9uy "Stack Pointer should be 0xF9"
            Expect.isTrue (0x01FAus |> memRead cpu |> hasFlag Flags.C) "Carry flag should be set"
            Expect.isTrue (0x01FAus |> memRead cpu |> hasFlag Flags.V) "Overflow flag should be set"
        }
        test "PLP" {
            let program = [| 0x28uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x01FFus (Flags.N ||| Flags.Z)
                    >> withCpu (fun c -> { c with sp = 0xFEuy }))

            Expect.equal cpu.cpu.sp 0xFFuy "Stack Pointer should be 0xFF"
            Expect.isTrue (cpu.cpu.p |> hasFlag Flags.Z) "Zero flag should be set"
            Expect.isTrue (cpu.cpu.p |> hasFlag Flags.N) "Negative flag should be set"
        }
        test "PHP and PLP" {
            let program = [|
                0x08uy // PHP
                0xA9uy
                0x21uy // LDA #$21
                0x28uy // PLP
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = Flags.V ||| Flags.Z })
            Expect.equal cpu.cpu.a 0x21uy "Accumulator should be 0x21"
            Expect.equal cpu.cpu.sp 0xFAuy "Stack Pointer should be 0xFA"
            Expect.isTrue (cpu.cpu.p |> hasFlag Flags.Z) "Zero flag should be set"
            Expect.isTrue (cpu.cpu.p |> hasFlag Flags.V) "Overflow flag should be set"
            Expect.equal cpu.cpu.pc 0x8005us "Program counter should be 0x8005"
        }
        test "STX to memory zero page" {
            let program = [| 0x86uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0xBBuy })

            Expect.equal (0x10us |> memRead cpu) 0xBBuy "Memory Address 0x0010 value should be 0xBB"
        }
        test "STY to memory zero page" {
            let program = [| 0x84uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0xBCuy })

            Expect.equal (0x10us |> memRead cpu) 0xBCuy "Memory Address 0x0010 value should be 0xBC"
        }
        test "TAY" {
            let program = [| 0xA8uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0xBDuy })

            Expect.equal cpu.cpu.y 0xBDuy "Y Register should be 0xBD"
        }
        test "TXA" {
            let program = [| 0x8Auy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0xBFuy })

            Expect.equal cpu.cpu.a 0xBFuy "A Register should be 0xBF"
        }
        test "TYA" {
            let program = [| 0x98uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0xC1uy })

            Expect.equal cpu.cpu.a 0xC1uy "A Register should be 0xC1"
        }
        test "TSX" {
            let program = [| 0xBAuy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with sp = 0xE1uy })

            Expect.equal cpu.cpu.x 0xE1uy "X Register should be 0xE1"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }
        test "TXS" {
            let program = [| 0x9Auy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0xE3uy })

            Expect.equal cpu.cpu.sp 0xE3uy "Stack Pointer should be 0xE3"
        }

        // ===== 分岐命令の追加テスト =====
        test "BMI with negative flag set" {
            let program = [|
                0x30uy
                0x02uy // BMI +2
                0x00uy // BRK
                0x00uy // BRK
                0xE8uy // INX
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.N c.p })
            Expect.equal cpu.cpu.x 0x01uy "X should be 1 (branch taken)"
            Expect.equal cpu.cpu.pc 0x8006us "Program counter should be 0x8006"
        }

        test "BMI with negative flag clear" {
            let program = [|
                0x30uy
                0x02uy // BMI +2
                0x00uy // BRK
                0x00uy // BRK
                0xE8uy // INX
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = 0uy })
            Expect.equal cpu.cpu.x 0x00uy "X should be 0 (branch not taken)"
            Expect.equal cpu.cpu.pc 0x8003us "Program counter should be 0x8003"
        }

        test "BPL with negative flag clear" {
            let program = [|
                0x10uy
                0x02uy // BPL +2
                0x00uy // BRK
                0x00uy // BRK
                0xE8uy // INX
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = 0uy })
            Expect.equal cpu.cpu.x 0x01uy "X should be 1 (branch taken)"
            Expect.equal cpu.cpu.pc 0x8006us "Program counter should be 0x8006"
        }

        test "BPL with negative flag set" {
            let program = [|
                0x10uy
                0x02uy // BPL +2
                0x00uy // BRK
                0x00uy // BRK
                0xE8uy // INX
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.N c.p })
            Expect.equal cpu.cpu.x 0x00uy "X should be 0 (branch not taken)"
            Expect.equal cpu.cpu.pc 0x8003us "Program counter should be 0x8003"
        }

        test "BVC with overflow flag clear" {
            let program = [|
                0x50uy
                0x02uy // BVC +2
                0x00uy // BRK
                0x00uy // BRK
                0xE8uy // INX
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = 0uy })
            Expect.equal cpu.cpu.x 0x01uy "X should be 1 (branch taken)"
            Expect.equal cpu.cpu.pc 0x8006us "Program counter should be 0x8006"
        }

        test "BVC with overflow flag set" {
            let program = [|
                0x50uy
                0x02uy // BVC +2
                0x00uy // BRK
                0x00uy // BRK
                0xE8uy // INX
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.V c.p })
            Expect.equal cpu.cpu.x 0x00uy "X should be 0 (branch not taken)"
            Expect.equal cpu.cpu.pc 0x8003us "Program counter should be 0x8003"
        }

        test "BVS with overflow flag set" {
            let program = [|
                0x70uy
                0x02uy // BVS +2
                0x00uy // BRK
                0x00uy // BRK
                0xE8uy // INX
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = setFlag Flags.V c.p })
            Expect.equal cpu.cpu.x 0x01uy "X should be 1 (branch taken)"
            Expect.equal cpu.cpu.pc 0x8006us "Program counter should be 0x8006"
        }

        test "BVS with overflow flag clear" {
            let program = [|
                0x70uy
                0x02uy // BVS +2
                0x00uy // BRK
                0x00uy // BRK
                0xE8uy // INX
                0x00uy // BRK
            |]

            let cpu = runWith program <| withCpu (fun c -> { c with p = 0uy })
            Expect.equal cpu.cpu.x 0x00uy "X should be 0 (branch not taken)"
            Expect.equal cpu.cpu.pc 0x8003us "Program counter should be 0x8003"
        }

        // ===== 転送命令のフラグテスト =====
        test "TAX sets zero flag" {
            let program = [| 0xAAuy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x00uy })

            Expect.equal cpu.cpu.x 0x00uy "X should be 0"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
            Expect.isFalse (hasFlag Flags.N cpu.cpu.p) "Negative flag should be clear"
        }

        test "TAX sets negative flag" {
            let program = [| 0xAAuy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x80uy })

            Expect.equal cpu.cpu.x 0x80uy "X should be 0x80"
            Expect.isFalse (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be clear"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }

        test "TAY sets zero flag" {
            let program = [| 0xA8uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x00uy })

            Expect.equal cpu.cpu.y 0x00uy "Y should be 0"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }

        test "TAY sets negative flag" {
            let program = [| 0xA8uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0xFFuy })

            Expect.equal cpu.cpu.y 0xFFuy "Y should be 0xFF"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }

        test "TXA sets zero flag" {
            let program = [| 0x8Auy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0x00uy })

            Expect.equal cpu.cpu.a 0x00uy "A should be 0"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }

        test "TXA sets negative flag" {
            let program = [| 0x8Auy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0x90uy })

            Expect.equal cpu.cpu.a 0x90uy "A should be 0x90"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }

        test "TYA sets zero flag" {
            let program = [| 0x98uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0x00uy })

            Expect.equal cpu.cpu.a 0x00uy "A should be 0"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }

        test "TYA sets negative flag" {
            let program = [| 0x98uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0xA5uy })

            Expect.equal cpu.cpu.a 0xA5uy "A should be 0xA5"
            Expect.isTrue (hasFlag Flags.N cpu.cpu.p) "Negative flag should be set"
        }

        // ===== LDX/LDY のアドレッシングモード追加 =====
        test "LDX zero page" {
            let program = [| 0xA6uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x10us 0x42uy

            Expect.equal cpu.cpu.x 0x42uy "X should be 0x42"
        }

        test "LDX zero page Y" {
            let program = [| 0xB6uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x15us 0x43uy >> withCpu (fun c -> { c with y = 0x05uy }))

            Expect.equal cpu.cpu.x 0x43uy "X should be 0x43"
        }

        test "LDX absolute" {
            let program = [| 0xAEuy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x1234us 0x44uy

            Expect.equal cpu.cpu.x 0x44uy "X should be 0x44"
        }

        test "LDX absolute Y" {
            let program = [| 0xBEuy; 0x34uy; 0x12uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x1239us 0x45uy >> withCpu (fun c -> { c with y = 0x05uy }))

            Expect.equal cpu.cpu.x 0x45uy "X should be 0x45"
        }

        test "LDY zero page" {
            let program = [| 0xA4uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x10us 0x46uy

            Expect.equal cpu.cpu.y 0x46uy "Y should be 0x46"
        }

        test "LDY zero page X" {
            let program = [| 0xB4uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x15us 0x47uy >> withCpu (fun c -> { c with x = 0x05uy }))

            Expect.equal cpu.cpu.y 0x47uy "Y should be 0x47"
        }

        test "LDY absolute" {
            let program = [| 0xACuy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x1234us 0x48uy

            Expect.equal cpu.cpu.y 0x48uy "Y should be 0x48"
        }

        test "LDY absolute X" {
            let program = [| 0xBCuy; 0x34uy; 0x12uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x1239us 0x49uy >> withCpu (fun c -> { c with x = 0x05uy }))

            Expect.equal cpu.cpu.y 0x49uy "Y should be 0x49"
        }

        // ===== STX/STY のアドレッシングモード追加 =====
        test "STX absolute" {
            let program = [| 0x8Euy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0x4Auy })

            Expect.equal (memRead cpu 0x1234us) 0x4Auy "Memory at 0x1234 should be 0x4A"
        }

        test "STX zero page Y" {
            let program = [| 0x96uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0x4Buy; y = 0x05uy })

            Expect.equal (memRead cpu 0x15us) 0x4Buy "Memory at 0x15 should be 0x4B"
        }

        test "STY absolute" {
            let program = [| 0x8Cuy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0x4Cuy })

            Expect.equal (memRead cpu 0x1234us) 0x4Cuy "Memory at 0x1234 should be 0x4C"
        }

        test "STY zero page X" {
            let program = [| 0x94uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with y = 0x4Duy; x = 0x05uy })

            Expect.equal (memRead cpu 0x15us) 0x4Duy "Memory at 0x15 should be 0x4D"
        }

        // ===== STA のアドレッシングモード追加 =====
        test "STA zero page" {
            let program = [| 0x85uy; 0x10uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x4Euy })

            Expect.equal (memRead cpu 0x10us) 0x4Euy "Memory at 0x10 should be 0x4E"
        }

        test "STA absolute" {
            let program = [| 0x8Duy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x4Fuy })

            Expect.equal (memRead cpu 0x1234us) 0x4Fuy "Memory at 0x1234 should be 0x4F"
        }

        test "STA absolute X" {
            let program = [| 0x9Duy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x50uy; x = 0x05uy })

            Expect.equal (memRead cpu 0x1239us) 0x50uy "Memory at 0x1239 should be 0x50"
        }

        test "STA absolute Y" {
            let program = [| 0x99uy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with a = 0x51uy; y = 0x05uy })

            Expect.equal (memRead cpu 0x1239us) 0x51uy "Memory at 0x1239 should be 0x51"
        }

        test "STA indirect X" {
            let program = [| 0x81uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x18us 0x34uy
                    >> memWrite 0x19us 0x12uy
                    >> withCpu (fun c -> { c with a = 0x52uy; x = 0x08uy }))

            Expect.equal (memRead cpu 0x1234us) 0x52uy "Memory at 0x1234 should be 0x52"
        }

        test "STA indirect Y" {
            let program = [| 0x91uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0x34uy
                    >> memWrite 0x11us 0x12uy
                    >> withCpu (fun c -> { c with a = 0x53uy; y = 0x05uy }))

            Expect.equal (memRead cpu 0x1239us) 0x53uy "Memory at 0x1239 should be 0x53"
        }

        // ===== エッジケース：ゼロページラップアラウンド =====
        test "LDA zero page X wraps around" {
            let program = [| 0xB5uy; 0xFFuy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x04us 0x54uy >> withCpu (fun c -> { c with x = 0x05uy }))

            Expect.equal cpu.cpu.a 0x54uy "A should be 0x54 (wrapped to 0x04)"
        }

        test "STX zero page Y wraps around" {
            let program = [| 0x96uy; 0xFEuy; 0x00uy |]
            let cpu = runWith program <| withCpu (fun c -> { c with x = 0x55uy; y = 0x03uy })

            Expect.equal (memRead cpu 0x01us) 0x55uy "Memory at 0x01 should be 0x55 (wrapped)"
        }

        test "LDA indirect X wraps in zero page" {
            let program = [| 0xA1uy; 0xFEuy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x01us 0x34uy // Wrapped: 0xFE + 0x03 = 0x01
                    >> memWrite 0x02us 0x12uy
                    >> memWrite 0x1234us 0x56uy
                    >> withCpu (fun c -> { c with x = 0x03uy }))

            Expect.equal cpu.cpu.a 0x56uy "A should be 0x56"
        }

        // ===== エッジケース：JMP Indirect のページ境界バグ =====
        test "JMP Indirect page boundary bug" {
            let program = [| 0x6Cuy; 0xFFuy; 0x01uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x01FFus 0x34uy // Low byte
                    >> memWrite 0x0100us 0x12uy // High byte (should be 0x0200, but wraps)
                    >> memWrite 0x1234us 0xE8uy
                    >> memWrite 0x1235us 0x00uy)

            Expect.equal cpu.cpu.x 0x01uy "X should be 1 (jumped to wrong address)"
        }

        // ===== その他のアドレッシングモード =====
        test "CMP zero page" {
            let program = [| 0xC5uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0x01uy >> withCpu (fun c -> { c with a = 0x02uy }))

            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
        }

        test "CMP absolute" {
            let program = [| 0xCDuy; 0x34uy; 0x12uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x1234us 0x02uy >> withCpu (fun c -> { c with a = 0x02uy }))

            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }

        test "CPX zero page" {
            let program = [| 0xE4uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0x01uy >> withCpu (fun c -> { c with x = 0x02uy }))

            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
        }

        test "CPX absolute" {
            let program = [| 0xECuy; 0x34uy; 0x12uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x1234us 0x02uy >> withCpu (fun c -> { c with x = 0x02uy }))

            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }

        test "CPY zero page" {
            let program = [| 0xC4uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0x01uy >> withCpu (fun c -> { c with y = 0x02uy }))

            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
        }

        test "CPY absolute" {
            let program = [| 0xCCuy; 0x34uy; 0x12uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x1234us 0x02uy >> withCpu (fun c -> { c with y = 0x02uy }))

            Expect.isTrue (hasFlag Flags.C cpu.cpu.p) "Carry flag should be set"
            Expect.isTrue (hasFlag Flags.Z cpu.cpu.p) "Zero flag should be set"
        }

        test "INC absolute" {
            let program = [| 0xEEuy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x1234us 0x05uy

            Expect.equal (memRead cpu 0x1234us) 0x06uy "Memory at 0x1234 should be 0x06"
        }

        test "DEC absolute" {
            let program = [| 0xCEuy; 0x34uy; 0x12uy; 0x00uy |]
            let cpu = runWith program <| memWrite 0x1234us 0x05uy

            Expect.equal (memRead cpu 0x1234us) 0x04uy "Memory at 0x1234 should be 0x04"
        }

        test "AND zero page" {
            let program = [| 0x25uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0x0Fuy >> withCpu (fun c -> { c with a = 0xFFuy }))

            Expect.equal cpu.cpu.a 0x0Fuy "A should be 0x0F"
        }

        test "EOR zero page" {
            let program = [| 0x45uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0xFFuy >> withCpu (fun c -> { c with a = 0xAAuy }))

            Expect.equal cpu.cpu.a 0x55uy "A should be 0x55"
        }

        test "ORA zero page" {
            let program = [| 0x05uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0x0Fuy >> withCpu (fun c -> { c with a = 0xF0uy }))

            Expect.equal cpu.cpu.a 0xFFuy "A should be 0xFF"
        }

        test "ADC zero page" {
            let program = [| 0x65uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0x05uy >> withCpu (fun c -> { c with a = 0x0Auy }))

            Expect.equal cpu.cpu.a 0x0Fuy "A should be 0x0F"
        }

        test "SBC zero page" {
            let program = [| 0xE5uy; 0x10uy; 0x00uy |]

            let cpu =
                runWith program
                <| (memWrite 0x10us 0x05uy >> withCpu (fun c -> { c with a = 0x0Auy; p = Flags.C }))

            Expect.equal cpu.cpu.a 0x05uy "A should be 0x05"
        }
    ]
