namespace HamicomEmu.EmulatorCore

module EmulatorCore =

  open HamicomEmu.Cpu
  open HamicomEmu.Bus

  type EmulatorState = {
    cpu: Cpu.CpuState
    bus: Bus.BusState
  }

  let initial rom = {
    cpu = Cpu.initial
    bus = Bus.initial rom
  }

  let reset emu =
    let cpu', bus' = Cpu.reset emu.cpu emu.bus
    { cpu = cpu'; bus = bus' }

  /// TODO: Cpu.step と Bus.tick の分離
  let tick emu =
    let cpu', bus', cycles = Cpu.step emu.cpu emu.bus
    { cpu = cpu'; bus = bus' }, cycles
