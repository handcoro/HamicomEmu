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

  type PulseChannelNumber =
  | One | Two

  type PulseState = {
    channel: PulseChannelNumber
    volume: byte
    duty: byte
    loopAndHalt: bool // エンベロープのループと長さカウンタ停止フラグ兼用
    isConstant: bool

    sweep: SweepState
    // timer は値が低いほど周波数が高くなる
    mutable timer: uint16

    mutable envelope: EnvelopeState

    // 長さカウンタ内部状態
    mutable lengthCounter: byte

    mutable phase: float
  }

  type TriangleState = {
    linearCounterLoad: byte
    ctrlAndHalt: bool

    timer: uint16

    mutable linearCounter: byte
    mutable linearReloadFlag: bool

    mutable lengthCounter: byte

    mutable phase: float
  }

  type NoiseState = {
    volume: byte
    loopAndHalt : bool
    isConstant : bool

    periodIndex: byte
    isShortMode: bool

    mutable envelope: EnvelopeState

    mutable lengthCounter: byte
    mutable shift: uint16
    mutable phase: float
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

    outputBuffer: ResizeArray<byte>
    mutable lastOutput : byte
  }

  type ApuReadRequest =
  | DmcSampleRead of uint16

  type FrameCounterMode = FourStep | FiveStep

  type FrameCounter = {
    mode: FrameCounterMode
    irqInhibit: bool
    irqRequested: bool
  }

  type FrameStep =
    | Step1
    | Step2
    | Step3
    | Step4
    | Step5

  type ApuState = {
    mutable pulse1: PulseState
    mutable pulse2: PulseState
    mutable triangle: TriangleState
    mutable noise: NoiseState
    mutable dmc: DmcState
    status: byte
    frameCounter: FrameCounter
    mutable cycle: uint
    mutable step: FrameStep
    mutable irq: bool
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
