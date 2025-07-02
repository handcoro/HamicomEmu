namespace HamicomEmu.EmulatorCore

module EmulatorCore =

  open HamicomEmu.Cpu
  open HamicomEmu.Bus
  open HamicomEmu.Ppu
  open HamicomEmu.Ppu.Types

  type EmulatorState = {
    cpu: Cpu.CpuState
    bus: Bus.BusState
    mutable ppuSnapshot: PpuPublicState
  }

  let initial rom = {
    cpu = Cpu.initial
    bus = Bus.initial rom
    ppuSnapshot = PpuPublicState.initial
  }

  let reset emu =
    let cpu', bus' = Cpu.reset emu.cpu emu.bus
    { cpu = cpu'; bus = bus' ; ppuSnapshot = PpuPublicState.initial }

  let updateSnapshot (ppu: PpuState) (emu: EmulatorState) : EmulatorState =
    { emu with ppuSnapshot = PpuPublicState.fromPpu ppu }

  /// ステップの実行回数指定をできるようにしてあるけど使わないかも
  /// TODO: Bus.tick を 1 ずつ回すようにした影響で再現度は上がったけど実行速度が犠牲に！適宜各種状態の mutable 化を検討中
  let rec tickN n emu (trace : EmulatorState -> unit) =
    let rec loop n emu consumedTotal =
      if n <= 0 then emu, consumedTotal
      else
      match emu.bus.pendingStallCpuCycles with
      | Some stall when stall > 0u ->
        // ストール中は CPU 実行を止めて Bus/APU/PPU だけ進める
        let bus' = Bus.tick emu.bus
        let newStall = stall - 1u
        let bus'' = Bus.updatePendingStallCpuCycles newStall bus'
        loop (n-1) { emu with bus = bus'' } (consumedTotal + 1u)
      | _ ->
        // NOTE: ここで割り込み判定をしているけど正確にはもっと複雑らしい？ https://www.nesdev.org/wiki/CPU_interrupts
        let irq = emu.bus.apu.irq
        let interruptDisabled = Cpu.interruptDisabled emu.cpu
        let cpu, bus, consumed =
          match Bus.pollNmiStatus emu.bus, irq && not interruptDisabled with
          | (b, Some _), _ -> // NMI
            Cpu.interruptNmi emu.cpu b
          | (b, None), true -> // IRQ
            Cpu.irq emu.cpu b
          | (b, None), false -> // 通常進行
            let c, b, con = Cpu.step emu.cpu b
            // 通常進行の場合はトレース実行
            trace { emu with cpu = c; bus = b }
            c, b, con
        let bus = Bus.tickNTimes (int consumed) bus
        let ppu = bus.ppu
        // PPU が 1 フレーム分処理したら非同期描画のためにスナップショットをコピーする
        if ppu.frameJustCompleted then
          emu.ppuSnapshot <- PpuPublicState.fromPpu ppu
          ppu.frameJustCompleted <- false

        let emu' = { emu with cpu = cpu; bus = bus }

        loop (n-1) emu' (consumedTotal + consumed)
    loop n emu 0u

  let tick emu trace =
    let emu', cycles = tickN 1 emu trace
    emu', cycles
