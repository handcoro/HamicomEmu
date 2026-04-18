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

    let tick emu (trace: EmulatorState -> unit) =
        match emu.bus.pendingStallCpuCycles with
        | Some stall when stall > 0u ->
            // ストール中は CPU 実行を止めて Bus/APU/PPU だけ進める
            let bus' =
                emu.bus
                |> Bus.tick
                |> Bus.updatePendingStallCpuCycles (stall - 1u)
            { emu with bus = bus' }, 1u
        | _ ->
            let nmiBus, nmi = Bus.pollNmiStatus emu.bus
            let irq = emu.cpu.irqLine

            let suppressIrq = Cpu.isSuppressIrq emu.cpu
            let irqAllowed =
                not (Cpu.interruptDisabled emu.cpu)
                && not suppressIrq

            // NMI > IRQ > 通常進行 の優先順位で CPU を実行
            let cpu', bus', consumedCycles =
                match nmi, irq && irqAllowed with
                | Some _, _ -> // NMI
                    Cpu.nmi emu.cpu nmiBus
                | None, true -> // IRQ
                    Cpu.irq emu.cpu nmiBus
                | None, false -> // 通常進行
                    // 通常進行の場合はトレース実行
                    trace emu
                    let cpu =
                        if suppressIrq then
                            Cpu.clearSuppressIrq emu.cpu
                        else
                            emu.cpu
                    Cpu.step cpu nmiBus

            { emu with cpu = cpu'; bus = bus' }, consumedCycles

