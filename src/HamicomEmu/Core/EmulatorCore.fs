namespace HamicomEmu.EmulatorCore

module EmulatorCore =

    open HamicomEmu.Cpu
    open HamicomEmu.Bus
    open HamicomEmu.Mapper

    type EmulatorState = {
        cpu: Cpu.CpuState
        bus: Bus.BusState
    }

    let powerOn cart =
        let cpu, bus = Cpu.powerOn cart

        {
            cpu = cpu
            bus = bus
        }

    let reset emu =
        let cpu', bus' = Cpu.reset emu.cpu emu.bus

        {
            cpu = cpu'
            bus = bus'
        }

    let fetchRamData emu =
        Mapper.getPrgRam emu.bus.cartridge

    let storeRamData data emu =
        let cart = Mapper.setPrgRam data emu.bus.cartridge
        { emu with bus.cartridge = cart }

    /// ステップの実行回数指定をできるようにしてあるけど使わないかも
    /// TODO: Bus.tick を 1 ずつ回すようにした影響で再現度は上がったけど実行速度が犠牲に！適宜各種状態の mutable 化を検討中
    let tick emu (trace: EmulatorState -> unit) =
        match emu.bus.pendingStallCpuCycles with
        | Some stall when stall > 0u ->
            // ストール中は CPU 実行を止めて Bus/APU/PPU だけ進める
            let bus' = Bus.tick emu.bus
            let newStall = stall - 1u
            let bus'' = Bus.updatePendingStallCpuCycles newStall bus'
            { emu with bus = bus'' }, 1u
        | _ ->
            // NOTE: ここで割り込み判定をしているけど正確にはもっと複雑らしい？ https://www.nesdev.org/wiki/CPU_interrupts
            let irq = Bus.pollIrqStatus emu.bus
            let interruptDisabled = Cpu.interruptDisabled emu.cpu
            let suppressIrq = Cpu.isSuppressIrq emu.cpu

            let cpu, bus, consumed =
                match Bus.pollNmiStatus emu.bus, irq && not interruptDisabled && not suppressIrq with
                | (b, Some _), _ -> // NMI
                    Cpu.interruptNmi emu.cpu b
                | (b, None), true -> // IRQ
                    Cpu.irq emu.cpu b
                | (b, None), false -> // 通常進行
                    // 通常進行の場合はトレース実行
                    trace emu
                    // TODO: CLI, SEI, PLP の IRQ 抑制は後でもっといい仕組みを考える
                    let c = if suppressIrq then Cpu.clearSuppressIrq emu.cpu else emu.cpu
                    let c', b, con = Cpu.step c b
                    c', b, con

            let bus = Bus.updatePendingStallCpuCycles consumed bus

            { emu with cpu = cpu; bus = bus }, 0u

