namespace HamicomEmu.Apu

module Types =

    type EnvelopeState = {
      mutable volume: byte
      mutable divider: byte
      mutable decay: byte
      mutable reload: bool
    }

    type SweepState = {
        enabled: bool
        negate: bool
        period: byte
        shift: byte
        mutable reload: bool
        mutable divider: byte
    }

    type PulseChannelNumber = One | Two

    type PulseState = {
        channel: PulseChannelNumber
        volume: byte
        duty: byte
        loopAndHalt: bool // エンベロープのループと長さカウンタ停止フラグ兼用
        isConstant: bool

        sweep: SweepState
        // timer は値が低いほど周波数が高くなる
        // TODO: 他チャンネルも含めて timer の仕様をよく調べる
        mutable timer: uint16
        mutable targetTimer: uint16

        mutable envelope: EnvelopeState

        // 長さカウンタ内部状態
        mutable lengthCounter: byte

        mutable dutyStep: int
    }

    type TriangleState = {
        linearCounterLoad: byte
        ctrlAndHalt: bool

        mutable timer: uint16
        timerReloadValue: uint16

        mutable linearCounter: byte
        mutable linearReloadFlag: bool

        mutable lengthCounter: byte

        mutable triangleStep: int
    }

    type NoiseState = {
        volume: byte
        loopAndHalt: bool
        isConstant: bool

        periodIndex: int
        isShortMode: bool

        mutable envelope: EnvelopeState

        mutable lengthCounter: byte
        // Linear feedback shift register
        mutable lfsr: uint16
        mutable timer: uint16
    }

    type DmcState = {
        irqEnabled: bool
        isLoop: bool
        rateIndex: byte

        mutable outputLevel: byte

        startAddress: byte
        sampleLength: byte

        currentAddress: uint16 // $C000 - $FFFF
        bytesRemaining: uint16
        mutable buffer: byte option

        mutable shiftRegister: byte
        mutable bitsRemaining: int

        mutable timer: uint16
        mutable isSilence: bool

        mutable irqRequested: bool

        mutable lastOutput: byte
    }

    type ApuReadRequest = DmcSampleRead of uint16

    type FrameCounterMode = FourStep | FiveStep

    type FrameCounter = {
        mode: FrameCounterMode
        irqInhibit: bool
        mutable irqRequested: bool
    }

    type FrameStep =
        | Step1
        | Step2
        | Step3
        | Step4
        | Step5

    type LowPassFilterState = { lastOutput: float32 }

    type ApuState = {
        mutable pulse1: PulseState
        mutable pulse2: PulseState
        mutable triangle: TriangleState
        mutable noise: NoiseState
        mutable dmc: DmcState
        mutable filterState: LowPassFilterState
        status: byte
        frameCounter: FrameCounter
        mutable cycle: uint
        mutable step: FrameStep
    }

    type DmcReadRequest = {
        addr: uint16
        onRead: byte -> DmcState
    }

    type TickResult = {
        apu: ApuState
        dmcRead: DmcReadRequest option
        stallCpuCycles: uint option
    }
