namespace HamicomEmu.Apu

module Constants =

    let frameStepCycles = 7457u // 1 step のサイクル数
    let sampleRate = 44100
    // -- BlipBuffer 用 --
    let bufferSize = 8192
    let numPhase = 128
    let kernelLength = 512

    let dutyTable = [|
        [| 0; 1; 0; 0; 0; 0; 0; 0 |] // 12.5%
        [| 0; 1; 1; 0; 0; 0; 0; 0 |] // 25%
        [| 0; 1; 1; 1; 1; 0; 0; 0 |] // 50%
        [| 1; 0; 0; 1; 1; 1; 1; 1 |] // 25% negated
    |]

    let triangleTable = [|
        15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0;
        0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15
    |]
    /// 除数インデックス
    let noisePeriods = [| 
        4;   8;  16;  32;  64;   96;  128;  160;
        202; 254; 380; 508; 762; 1016; 2034; 4068
    |]

    let dmcRateTable = [|
        428; 380; 340; 320; 286; 254; 226; 214;
        190; 160; 142; 128; 106;  84;  72;  54
    |]

    let lengthTable = [|
        10uy; 254uy; 20uy;  2uy; 40uy;  4uy; 80uy;  6uy;
        160uy; 8uy; 60uy; 10uy; 14uy; 12uy; 26uy; 14uy;
        12uy; 16uy; 24uy; 18uy; 48uy; 20uy; 96uy; 22uy;
        192uy; 24uy; 72uy; 26uy; 16uy; 28uy; 32uy; 30uy
    |]


module GeneralMasks =
    // 矩形波とノイズ共通
    let volumeMask   = 0b0000_1111uy
    let envelopeMask = 0b0000_1111uy
    let constantVolumeFlag    = 0b0001_0000uy
    let envelopeLoopFlag      = 0b0010_0000uy
    let lengthCounterHaltFlag = 0b0010_0000uy
    
    // 矩形波と三角波とノイズ共通
    let lengthCounterMask = 0b1111_1000uy
    let timerHiMask       = 0b0000_0111uy

module PulseBitMasks =
    // $4000, $4004
    let volumeMask            = 0b0000_1111uy
    let envelopeMask          = 0b0000_1111uy
    let constantVolumeFlag    = 0b0001_0000uy
    let envelopeLoopFlag      = 0b0010_0000uy
    let lengthCounterHaltFlag = 0b0010_0000uy
    let dutyCycleMask         = 0b1100_0000uy
    // $4001, $4005
    let sweepFlag       = 0b1000_0000uy
    let sweepPeriodMask = 0b0111_0000uy
    let sweepNegateFlag = 0b0000_1000uy
    let sweepShiftMask  = 0b0000_0111uy
    // $4003, $4007
    let timerHiMask       = 0b0000_0111uy
    let lengthCounterMask = 0b1111_1000uy

module TriangleBitMasks =
    // $4008
    let linearCounterMask     = 0b0111_1111uy
    let controlFlag           = 0b1000_0000uy
    let lengthCounterHaltFlag = 0b1000_0000uy
    // $400B
    let timerHiMask       = 0b0000_0111uy
    let lengthCounterMask = 0b1111_1000uy

module NoiseBitMasks =
    // $400C
    let volumeMask            = 0b0000_1111uy
    let envelopeMask          = 0b0000_1111uy
    let constantVolumeFlag    = 0b0001_0000uy
    let envelopeLoopFlag      = 0b0010_0000uy
    let lengthCounterHaltFlag = 0b0010_0000uy
    let modeFlag              = 0b1000_0000uy
    // $400E
    let periodMask        = 0b0000_1111uy
    let lengthCounterMask = 0b1111_1000uy

module DmcBitMasks =
    // $4010
    let irqEnabledFlag = 0b1000_0000uy
    let loopFlag       = 0b0100_0000uy
    let rateIndexMask  = 0b0000_1111uy
    // $4011
    let directLoadMask = 0b0111_1111uy

module StatusFlags =
    // $4015 write
    let dmcEnable      = 0b0001_0000uy
    let noiseEnable    = 0b0000_1000uy
    let triangleEnable = 0b0000_0100uy
    let pulse2Enable   = 0b0000_0010uy
    let pulse1Enable   = 0b0000_0001uy
    // $4015 read
    let dmcInterrupt                        = 0b1000_0000uy
    let frameInterrupt                      = 0b0100_0000uy
    let dmcActive                           = 0b0001_0000uy
    let noiseLengthCounterLargerThanZero    = 0b0000_1000uy
    let triangleLengthCounterLargerThanZero = 0b0000_0100uy
    let pulse2LengthCounterLargerThanZero   = 0b0000_0010uy
    let pulse1LengthCounterLargerThanZero   = 0b0000_0001uy

module FrameCounterFlags =
  // $4017
    let mode       = 0b1000_0000uy // 0 = 4-step, 1 = 5-step
    let irqInhibit = 0b0100_0000uy
