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

    deltaCounter: byte

    startAddress: byte
    sampleLength: byte

    buffer: byte option
    lengthCounter: byte
    currentAddress: uint16 // $C000 - $FFFF
    bytesRemaining: byte
    shiftResister: byte
    bitCounter: int

    outputLevel: byte
    timer: int
    isSilence: bool
  }

  type ApuReadRequest =
  | DmcSampleRead of uint16

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
