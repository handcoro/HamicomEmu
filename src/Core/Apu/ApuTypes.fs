namespace HamicomEmu.Apu

module Types =

  type EnvelopeState = {
    volume: byte
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

  type PulseChannel = {
    channel: PulseChannelNumber
    volume: byte
    duty: byte
    loopAndHalt: bool // エンベロープのループと長さカウンタ停止フラグ兼用
    isConstant: bool

    sweep: SweepState
    // timer は値が低いほど周波数が高くなる
    timer: uint16

    envelope: EnvelopeState

    // 長さカウンタ内部状態
    lengthCounter: byte

    phase: float
  }

  type TriangleChannel = {
    linearCounterLoad: byte
    ctrlAndHalt: bool

    timer: uint16

    linearCounter: byte
    linearReloadFlag: bool

    lengthCounter: byte

    phase: float
  }

  type NoiseChannel = {
    volume: byte
    loopAndHalt : bool
    isConstant : bool

    periodIndex: byte
    isShortMode: bool

    envelope: EnvelopeState

    lengthCounter: byte
    shift: uint16
    phase: float
  }

  type DeltaModulationChannel = {
    irqEnabled: bool
    isLoop: bool
    rateIndex: byte

    sampleAddress: byte
    sampleLength: byte

    sampleBuffer: byte
    lengthCounter: byte
    currentAddress: uint16 // $C000 - $FFFF
    bytesRemaining: byte
    shiftResister: byte
    bitCounter: int

    outputLevel: byte
    timer: int
    isSilence: bool
  }

  type FrameCounterMode = FourStep | FiveStep

  type FrameCounter = {
    mode: FrameCounterMode
    irqInhibit: bool
  }

  type ApuStep =
    | Step1
    | Step2
    | Step3
    | Step4
    | Step5

  type ApuState = {
    pulse1: PulseChannel
    pulse2: PulseChannel
    triangle: TriangleChannel
    noise: NoiseChannel
    // TODO: DPCM
    status: byte
    frameCounter: FrameCounter
    mutable cycle: uint
    mutable step: ApuStep
    irq: bool
  }
