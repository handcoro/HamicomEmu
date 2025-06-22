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

  /// ステップの実行回数指定をできるようにしてあるけど使わないかも
  /// TODO: Bus.tick を 1 ずつ回すようにした影響で再現度は上がったけど実行速度が犠牲に！適宜各種状態の mutable 化を検討中
  let rec tickN n emu =
    let rec loop n emu consumedTotal =
      if n <= 0 then emu, consumedTotal
      else
      match emu.bus.pendingStallCpuCycles with
      | Some stall when stall > 0u ->
        // ストール中は CPU 実行を止めて Bus/APU/PPU だけ進める
        let bus' = Bus.tick emu.bus
        let newStall = stall - 1u
        let bus'' = { bus' with pendingStallCpuCycles = if newStall = 0u then None else Some newStall }
        loop (n-1) { emu with bus = bus'' } (consumedTotal + 1u)
      | _ ->
        // 通常進行
        let cpu', bus, consumed = Cpu.step emu.cpu emu.bus
        let bus' = Bus.tickNTimes (int consumed) bus
        let emu' = { emu with cpu = cpu'; bus = bus' }
        loop (n-1) emu' (consumedTotal + consumed)
    loop n emu 0u

  let tick emu =
    let emu', cycles = tickN 1 emu
    emu', cycles
