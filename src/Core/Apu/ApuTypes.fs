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

  type PulseState = {
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

  type TriangleState = {
    linearCounterLoad: byte
    ctrlAndHalt: bool

    timer: uint16

    linearCounter: byte
    linearReloadFlag: bool

    lengthCounter: byte

    phase: float
  }

  type NoiseState = {
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

  type DmcState = {
    irqEnabled: bool
    isLoop: bool
    rateIndex: byte

    outputLevel: byte

    startAddress: byte
    sampleLength: byte

    currentAddress: uint16 // $C000 - $FFFF
    bytesRemaining: uint16
    buffer: byte option

    shiftRegister: byte
    bitsRemaining: int

    timer: uint16
    isSilence: bool

    irqRequested: bool

    outputBuffer: byte list
    lastOutput : byte
  }

  type ApuReadRequest =
  | DmcSampleRead of uint16

  type FrameCounterMode = FourStep | FiveStep

  type FrameCounter = {
    mode: FrameCounterMode
    irqInhibit: bool
    irqRequested: bool
  }

  type ApuStep =
    | Step1
    | Step2
    | Step3
    | Step4
    | Step5

  type ApuState = {
    pulse1: PulseState
    pulse2: PulseState
    triangle: TriangleState
    noise: NoiseState
    dmc: DmcState
    // TODO: DPCM
    status: byte
    frameCounter: FrameCounter
    mutable cycle: uint
    mutable step: ApuStep
    irq: bool
  }

  type DmcReadRequest = {
    addr: uint16
    onRead: byte -> DmcState
  }

  type TickResult = {
    apu: ApuState
    dmcRead: DmcReadRequest option
    // TODO: DMC の読み込みで CPU を止める処理の実装
    // stallCpuCycles: uint option
  }
