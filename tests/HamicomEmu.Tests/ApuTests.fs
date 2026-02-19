module ApuTests

open Expecto
open HamicomEmu.Apu
open HamicomEmu.Apu.Types
open HamicomEmu.Common.BitUtils

let initialDmcState = {
    Dmc.init with
        currentAddress = 0xC000us
        buffer = Some 0b1111_0000uy
        outputLevel = 64uy
}

let apuTests =
    testList "APU Tests" [
        test "DMC tick consumes bit and updates outputLevel" {
            let state = {
                initialDmcState with
                    buffer = Some 0b1111_0000uy
                    outputLevel = 64uy
                    bitsRemaining = 0
                    timer = 0us
            }

            let state1, _, _ = Dmc.tick state
            Expect.equal state1.shiftRegister 0b0111_1000uy "Shifted 1 bit"
            Expect.equal state1.bitsRemaining 7 "bitCounter updated"
            Expect.equal state1.outputLevel 62uy "Output decreased (bit=0)"
        }

        test "DMC processes 8 bits of sampleBuffer and updates outputLevel" {
            let init = {
                initialDmcState with
                    buffer = Some 0b1111_0000uy
                    outputLevel = 64uy
                    bitsRemaining = 0
                    timer = 0us
            }

            // tick 1: 0 → -2 → 62
            let s1, _, _ = Dmc.tick init
            Expect.equal s1.outputLevel 62uy "Tick 1: output -2"
            Expect.equal s1.bitsRemaining 7 "Tick 1: bitsRemaining 7"
            Expect.equal s1.shiftRegister 0b0111_1000uy "Tick 1: shifted once"

            // tick 2: 0 → -2 → 60
            let s2, _, _ = Dmc.tick { s1 with timer = 0us }
            Expect.equal s2.outputLevel 60uy "Tick 2: output -2"
            Expect.equal s2.bitsRemaining 6 "Tick 2: bitsRemaining 6"
            Expect.equal s2.shiftRegister 0b0011_1100uy "Tick 2: shifted"

            // tick 3: 0 → -2 → 58
            let s3, _, _ = Dmc.tick { s2 with timer = 0us }
            Expect.equal s3.outputLevel 58uy "Tick 3: output -2"
            Expect.equal s3.bitsRemaining 5 "Tick 3: bitsRemaining 5"

            // tick 4: 0 → -2 → 56
            let s4, _, _ = Dmc.tick { s3 with timer = 0us }
            Expect.equal s4.outputLevel 56uy "Tick 4: output -2"
            Expect.equal s4.bitsRemaining 4 "Tick 4: bitsRemaining 4"

            // tick 5: 1 → +2 → 58
            let s5, _, _ = Dmc.tick { s4 with timer = 0us }
            Expect.equal s5.outputLevel 58uy "Tick 5: output +2"
            Expect.equal s5.bitsRemaining 3 "Tick 5: bitsRemaining 3"

            // tick 6: 1 → +2 → 60
            let s6, _, _ = Dmc.tick { s5 with timer = 0us }
            Expect.equal s6.outputLevel 60uy "Tick 6: output +2"

            // tick 7: 1 → +2 → 62
            let s7, _, _ = Dmc.tick { s6 with timer = 0us }
            Expect.equal s7.outputLevel 62uy "Tick 7: output +2"

            // tick 8: 1 → +2 → 64
            let s8, _, _ = Dmc.tick { s7 with timer = 0us }
            Expect.equal s8.outputLevel 64uy "Tick 8: output +2"

            // tick 9: buffer=None かつ bitsRemaining=0 → silence=true になるはず
            let s9, _, _ = Dmc.tick { s8 with timer = 0us }
            Expect.isTrue s9.isSilence "Tick 9: silence flag should be set"
            Expect.equal s9.outputLevel 64uy "Tick 9: output stays same"
        }

        test "startSample correctly sets currentAddress and bytesRemaining" {
            let dmc = {
                initialDmcState with
                    startAddress = 0x20uy  // = 0x20 * 64 = 0x800
                    sampleLength = 0x10uy  // = 0x10 * 16 + 1 = 257
            }

            let started = Dmc.startSample dmc

            let expectedAddr = 0xC000us + uint16 dmc.startAddress * 64us
            let expectedBytes = uint16 dmc.sampleLength * 16us + 1us

            Expect.equal started.currentAddress expectedAddr "currentAddress is correct"
            Expect.equal started.bytesRemaining expectedBytes "bytesRemaining is correct"
        }

        test "applySampleRead restarts sample when loop flag is set and bytesRemaining reaches 0" {
            let dmc = {
                initialDmcState with
                    startAddress = 0x21uy  // 0xC000 + 0x840 = 0xC840
                    sampleLength = 0x12uy  // (0x12 * 16) + 1 = 289
                    currentAddress = 0xFFFFus
                    bytesRemaining = 1us
                    isLoop = true
            }

            let dmc' = Dmc.applySampleRead 0x55uy dmc

            let expectedAddr = 0xC000us + (uint16 dmc.startAddress <<< 6)
            let expectedRemaining = (uint16 dmc.sampleLength <<< 4) + 1us

            Expect.equal dmc'.currentAddress expectedAddr "Loop currentAddress reset correctly"
            Expect.equal dmc'.bytesRemaining expectedRemaining "Loop bytesRemaining reset correctly"
            Expect.equal dmc'.buffer (Some 0x55uy) "Buffer updated correctly"
        }

        test "applySampleRead does not restart sample when loop flag is false and bytesRemaining reaches 0" {
            let dmc = {
                initialDmcState with
                    startAddress = 0x21uy
                    sampleLength = 0x12uy
                    currentAddress = 0x1234us
                    bytesRemaining = 1us
                    isLoop = false
            }

            let dmc' = Dmc.applySampleRead 0xAAuy dmc

            // currentAddress should just +1, not reset
            Expect.equal dmc'.currentAddress 0x1235us "currentAddress incremented"
            Expect.equal dmc'.bytesRemaining 0us "bytesRemaining decremented to 0"
            Expect.equal dmc'.buffer (Some 0xAAuy) "Buffer updated"
        }

        test "DMC requests IRQ when last sample byte is read and loop is disabled" {
            let dmc = {
                initialDmcState with
                    isLoop = false
                    irqEnabled = true
                    buffer = None
                    currentAddress = 0xC000us
                    bytesRemaining = 1us  // 最後の1バイトが残っている
            }

            // 最後のバイトを読み込む
            let dmc' = Dmc.applySampleRead 0xAAuy dmc

            Expect.isTrue dmc'.irqRequested "IRQ should be requested when last byte is read"
            Expect.equal dmc'.bytesRemaining 0us "bytesRemaining should be 0"
            Expect.equal dmc'.buffer (Some 0xAAuy) "Buffer should contain the read value"
        }

        test "DMC does not request IRQ when loop is enabled" {
            let dmc = {
                initialDmcState with
                    isLoop = true
                    irqEnabled = true
                    startAddress = 0x10uy
                    sampleLength = 0x01uy
                    buffer = None
                    currentAddress = 0xC000us
                    bytesRemaining = 1us
            }

            let dmc' = Dmc.applySampleRead 0xBBuy dmc

            Expect.isFalse dmc'.irqRequested "IRQ should not be requested when loop is enabled"
            Expect.isTrue (dmc'.bytesRemaining > 0us) "Sample should restart when looping"
        }

        test "DMC does not request IRQ when irqEnabled is false" {
            let dmc = {
                initialDmcState with
                    isLoop = false
                    irqEnabled = false
                    buffer = None
                    currentAddress = 0xC000us
                    bytesRemaining = 1us
            }

            let dmc' = Dmc.applySampleRead 0xCCuy dmc

            Expect.isFalse dmc'.irqRequested "IRQ should not be requested when irqEnabled is false"
        }

        test "applySampleRead clears irqRequested flag" {
            let dmc = {
                initialDmcState with
                    irqRequested = true
                    buffer = None
                    currentAddress = 0x8000us
                    bytesRemaining = 1us
            }

            let dmc' = Dmc.applySampleRead 0xAAuy dmc

            Expect.equal dmc'.irqRequested false "IRQ should be cleared when new sample is read"
        }

        test "APU $4015 read reflects all channel statuses and IRQ flags" {
            let apu = {
                Apu.init with
                    pulse1 = { Pulse.init One with lengthCounter = 5uy }
                    pulse2 = { Pulse.init Two with lengthCounter = 0uy }
                    triangle = { Triangle.init with lengthCounter = 2uy }
                    noise = { Noise.init with lengthCounter = 0uy }
                    dmc = { Dmc.init with bytesRemaining = 10us; irqRequested = true }
                    frameCounter.irqRequested = true
                    status =
                        StatusFlags.pulse1Enable |||
                        StatusFlags.pulse2Enable |||
                        StatusFlags.triangleEnable |||
                        StatusFlags.noiseEnable |||
                        StatusFlags.dmcEnable |||
                        StatusFlags.frameInterrupt
            }

            let result, apu' = Apu.read 0x4015us apu

            // bit 7 = DMC IRQ → 1
            Expect.isTrue (result &&& 0b1000_0000uy <> 0uy) "DMC IRQ should be set"

            // bit 6 = Frame IRQ → 1 (and should be cleared in apu')
            Expect.isTrue (result &&& 0b0100_0000uy <> 0uy) "Frame IRQ should be set"
            Expect.isFalse (hasFlag StatusFlags.frameInterrupt apu'.status) "Frame IRQ should be cleared after read"

            // bit 4 = DMC active (bytesRemaining > 0) → 1
            Expect.isTrue (result &&& 0b0001_0000uy <> 0uy) "DMC should be active"

            // bit 3 = Noise channel lengthCounter = 0 → 0
            Expect.isFalse (result &&& 0b0000_1000uy <> 0uy) "Noise should be inactive"

            // bit 2 = Triangle channel lengthCounter = 2 → 1
            Expect.isTrue (result &&& 0b0000_0100uy <> 0uy) "Triangle should be active"

            // bit 1 = Pulse2 channel lengthCounter = 0 → 0
            Expect.isFalse (result &&& 0b0000_0010uy <> 0uy) "Pulse2 should be inactive"

            // bit 0 = Pulse1 channel lengthCounter = 5 → 1
            Expect.isTrue (result &&& 0b0000_0001uy <> 0uy) "Pulse1 should be active"
        }


    ]