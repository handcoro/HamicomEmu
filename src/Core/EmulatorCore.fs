namespace HamicomEmu.EmulatorCore

module EmulatorCore =

    open HamicomEmu.Cpu
    open HamicomEmu.Bus
    open HamicomEmu.Ppu
    open HamicomEmu.Ppu.Types
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
    let rec tickN n emu (trace: EmulatorState -> unit) =
        let rec loop n emu consumedTotal =
            if n <= 0 then
                emu, consumedTotal
            else
                match emu.bus.pendingStallCpuCycles with
                | Some stall when stall > 0u ->
                    // ストール中は CPU 実行を止めて Bus/APU/PPU だけ進める
                    let bus' = Bus.tick emu.bus
                    let newStall = stall - 1u
                    let bus'' = Bus.updatePendingStallCpuCycles newStall bus'
                    loop (n - 1) { emu with bus = bus'' } (consumedTotal + 1u)
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
                            // FIXME: CLI, SEI, PLP の IRQ 抑制は後でもっといい仕組みを考える
                            let c = if suppressIrq then Cpu.clearSuppressIrq emu.cpu else emu.cpu
                            let c', b, con = Cpu.step c b
                            c', b, con

                    let bus = Bus.tickNTimes (int consumed) bus

                    let emu' = { emu with cpu = cpu; bus = bus }

                    loop (n - 1) emu' (consumedTotal + consumed)

        loop n emu 0u

    let tick emu trace =
        let emu', cycles = tickN 1 emu trace
        emu', cycles
