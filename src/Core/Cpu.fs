namespace HamicomEmu.Cpu

module Cpu =

    open HamicomEmu.Cpu.Instructions
    open HamicomEmu.Bus
    open HamicomEmu.Common.BitUtils

    module Flags =
        let C = 0b0000_0001uy // Carry
        let Z = 0b0000_0010uy // Zero
        let I = 0b0000_0100uy // Interrupt Disable
        let D = 0b0000_1000uy // Decimal Mode
        let B = 0b0001_0000uy // Break
        let U = 0b0010_0000uy // Unused (常にセットされることが多い)
        let V = 0b0100_0000uy // Overflow
        let N = 0b1000_0000uy // Negative

    // pc: program counter
    // sp: stack pointer
    // p: processor status (flags)
    type CpuState = {
        a: byte
        x: byte
        y: byte
        pc: uint16
        sp: byte
        p: byte
        // NOTE: ブランチ命令用のサイクル増分 本当はもっと例外なくスマートにやりたい
        cyclePenalty: uint
        // NOTE: CLI, SEI, PLP 後の IRQ を抑制する
        suppressIrq: bool
    }

    let initial = {
        a = 0uy
        x = 0uy
        y = 0uy
        pc = 0us
        sp = 0xFDuy
        p = Flags.I ||| Flags.U
        cyclePenalty = 0u
        suppressIrq = false
    }

    let isPageCrossed (a: uint16) (b: uint16) = (a &&& 0xFF00us) <> (b &&& 0xFF00us)

    /// アドレッシングモードからオペランドとアドレスを割り出す
    let getOperandAddress cpu bus pc mode =
        match mode with
        | Accumulator -> failwithf "Unsupported mode: %A" mode
        | Immediate -> pc
        | ZeroPage ->
            let addr, _ = Bus.memRead pc bus
            addr |> uint16
        | Absolute ->
            let addr, _ = Bus.memRead16 pc bus
            addr |> uint16
        | ZeroPage_X ->
            let pos, _ = Bus.memRead pc bus
            let addr = pos + cpu.x |> uint16
            addr
        | ZeroPage_Y ->
            let pos, _ = Bus.memRead pc bus
            let addr = pos + cpu.y |> uint16
            addr
        | Absolute_X ->
            let bpos, _ = Bus.memRead16 pc bus
            let addr = bpos + (cpu.x |> uint16)
            addr
        | Absolute_Y ->
            let bpos, _ = Bus.memRead16 pc bus
            let addr = bpos + (cpu.y |> uint16)
            addr
        | Indirect_X ->
            let bpos, _ = Bus.memRead pc bus
            let ptr = bpos + cpu.x
            let addr, _ = Bus.memRead16ZeroPage ptr bus
            addr
        | Indirect_Y ->
            let bpos, _ = Bus.memRead pc bus
            let deRefBase, _ = Bus.memRead16ZeroPage bpos bus
            let deRef = deRefBase + (cpu.y |> uint16)
            deRef
        | Indirect ->
            let bpos, _ = Bus.memRead16 pc bus
            let addr, _ = Bus.memRead16Wrap bpos bus // JMP のページ境界バグ
            addr
        | Relative ->
            let offset, _ = Bus.memRead pc bus
            let baseAddr = pc + 1us
            let addr = int baseAddr + (offset |> int8 |> int)
            uint16 addr
        | Implied -> pc
    // | NoneAddressing ->
    //   failwithf "Unsupported mode: %A" mode

    /// 値によってZNフラグをセットする
    let setZeroNegativeFlags value p =
        p
        |> updateFlag Flags.Z (value = 0uy)
        |> updateFlag Flags.N (value &&& 0b1000_0000uy <> 0uy)

    /// プログラムカウンタを進める
    let advancePC offset cpu bus = { cpu with pc = cpu.pc + offset }, bus

    /// ビット論理演算
    let logicalInstr op mode cpu bus = // AND, EOR, ORA
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let value, bus' = Bus.memRead addr bus

        let a =
            match op with
            | AND -> (&&&) cpu.a value
            | EOR -> (^^^) cpu.a value
            | ORA -> (|||) cpu.a value
            | _ -> failwithf "Unsupported logical instruction: %A" op

        let p = cpu.p |> setZeroNegativeFlags a
        { cpu with a = a; p = p }, bus'

    /// 加算
    let adc mode cpu bus = // Add with Carry
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let value, bus' = Bus.memRead addr bus
        let carry = if hasFlag Flags.C cpu.p then 1 else 0
        let sum = int cpu.a + int value + carry
        let result = byte sum
        let isCarry = sum > 0xFF

        let isOverflow =
            // オーバーフローが発生する条件:
            // 1. A と value の符号が同じ
            // 2. 結果の符号が A と異なる
            cpu.a ^^^ value &&& 0x80uy = 0uy && cpu.a ^^^ result &&& 0x80uy <> 0uy

        let p =
            cpu.p
            |> updateFlag Flags.C isCarry
            |> updateFlag Flags.V isOverflow
            |> setZeroNegativeFlags result

        { cpu with a = result; p = p }, bus'

    /// 減算
    let sbc mode cpu bus = // Subtract with Carry
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let value, bus' = Bus.memRead addr bus
        let carry = if hasFlag Flags.C cpu.p then 1 else 0
        let valueInverted = ~~~value
        let sum = int cpu.a + int valueInverted + carry
        let result = byte sum
        let isCarry = sum > 0xFF

        let isOverflow =
            // ADC とオーバーフローの条件が違うので注意
            cpu.a ^^^ value &&& 0x80uy <> 0uy && cpu.a ^^^ result &&& 0x80uy <> 0uy

        let p =
            cpu.p
            |> updateFlag Flags.C isCarry
            |> updateFlag Flags.V isOverflow
            |> setZeroNegativeFlags result

        { cpu with a = result; p = p }, bus'

    let ld setReg mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let v, bus = Bus.memRead addr bus
        let p = setZeroNegativeFlags v cpu.p
        let cpu = cpu |> setReg v
        { cpu with p = p }, bus

    /// Load Accumulator
    let lda mode cpu bus =
        (cpu, bus) ||> ld (fun v cpu -> { cpu with a = v }) mode

    /// Load X Register
    let ldx mode cpu bus =
        (cpu, bus) ||> ld (fun v cpu -> { cpu with x = v }) mode

    /// Load Y Register
    let ldy mode cpu bus =
        (cpu, bus) ||> ld (fun v cpu -> { cpu with y = v }) mode

    /// Store Accumulator
    let sta mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        cpu, bus |> Bus.memWrite addr cpu.a

    /// Store X Register
    let stx mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        cpu, bus |> Bus.memWrite addr cpu.x

    /// Store Y Register
    let sty mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        cpu, bus |> Bus.memWrite addr cpu.y

    /// Transfer Accumulator to X
    let tax _ cpu bus =
        let x = cpu.a
        let p = cpu.p |> setZeroNegativeFlags x
        { cpu with x = x; p = p }, bus

    /// Transfer Accumulator to Y
    let tay _ cpu bus =
        let y = cpu.a
        let p = cpu.p |> setZeroNegativeFlags y
        { cpu with y = y; p = p }, bus

    /// Transfer X to Accumulator
    let txa _ cpu bus =
        let a = cpu.x
        let p = cpu.p |> setZeroNegativeFlags a
        { cpu with a = a; p = p }, bus

    /// Transfer Y to Accumulator
    let tya _ cpu bus =
        let a = cpu.y
        let p = cpu.p |> setZeroNegativeFlags a
        { cpu with a = a; p = p }, bus

    /// Transfer Y to Accumulator
    let tsx _ cpu bus =
        let x = cpu.sp
        let p = cpu.p |> setZeroNegativeFlags x
        { cpu with x = x; p = p }, bus

    /// Transfer X to Stack Pointer
    let txs _ cpu bus =
        let sp = cpu.x
        { cpu with sp = sp }, bus

    /// 左シフトしてキャリーフラグ設定
    let shiftLeftAndUpdate value status =
        let carry = value &&& 0b1000_0000uy > 0uy
        let result = value <<< 1
        let p = status |> updateFlag Flags.C carry
        result, p

    /// 右シフトしてキャリーフラグ設定
    let shiftRightAndUpdate value status =
        let carry = value &&& 0b0000_0001uy > 0uy
        let result = value >>> 1
        let p = status |> updateFlag Flags.C carry
        result, p

    /// シフト演算
    /// carryInFn はすでに立ってるキャリーフラグを演算結果に含める関数
    let modifyWithShift mode shiftFn carryInFn cpu bus =
        let cIn = hasFlag Flags.C cpu.p

        let value, writeBack =
            match mode with
            | Accumulator -> cpu.a, fun r -> { cpu with a = r }, bus
            | _ ->
                let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
                let v, b' = Bus.memRead addr bus
                v, fun r -> cpu, b' |> Bus.memWrite addr r

        let shifted, p = shiftFn value cpu.p
        let result = carryInFn shifted cIn
        let p' = setZeroNegativeFlags result p
        let cpu', bus' = writeBack result
        { cpu' with p = p' }, bus'

    let asl mode cpu bus =
        (cpu, bus) ||> modifyWithShift mode shiftLeftAndUpdate (fun r _ -> r)

    let lsrInstr mode cpu bus =
        (cpu, bus) ||> modifyWithShift mode shiftRightAndUpdate (fun r _ -> r)

    let rol mode cpu bus =
        (cpu, bus)
        ||> modifyWithShift mode shiftLeftAndUpdate (fun r carryBefore -> r ||| (if carryBefore then 1uy else 0uy))

    let ror mode cpu bus =
        (cpu, bus)
        ||> modifyWithShift mode shiftRightAndUpdate (fun r carryBefore ->
            r ||| (if carryBefore then 0b1000_0000uy else 0uy))

    /// 分岐はこの関数内で PC の進みを管理する
    /// TODO: 精密な再現ではオペランドフェッチ前とページ跨ぎ前のサイクルで割り込みを受け付ける https://www.nesdev.org/wiki/CPU_interrupts#Branch_instructions_and_interrupts
    let branch mode flag expected cpu bus =
        let pcAfterInstruction = cpu.pc + 2us
        let target = mode |> getOperandAddress cpu bus (cpu.pc + 1us)

        if hasFlag flag cpu.p <> expected then
            (cpu, bus) ||> advancePC 2us
        else
            // 条件達成で +1, ページ境界跨ぎでさらに +1
            let pen = 1u + if isPageCrossed pcAfterInstruction target then 1u else 0u

            {
                cpu with
                    pc = target
                    cyclePenalty = pen
            },
            bus

    /// BCC - Branch if Carry Clear
    let bcc mode cpu bus =
        (cpu, bus) ||> branch mode Flags.C false

    /// BCS - Branch if Carry Set
    let bcs mode cpu bus = (cpu, bus) ||> branch mode Flags.C true

    /// BEQ - Branch if Equal
    let beq mode cpu bus = (cpu, bus) ||> branch mode Flags.Z true

    /// BNE - Branch if Not Equal
    let bne mode cpu bus =
        (cpu, bus) ||> branch mode Flags.Z false

    /// Branch if Minus
    let bmi mode cpu bus = (cpu, bus) ||> branch mode Flags.N true

    /// Branch if Positive
    let bpl mode cpu bus =
        (cpu, bus) ||> branch mode Flags.N false

    /// Branch if Overflow Clear
    let bvc mode cpu bus =
        (cpu, bus) ||> branch mode Flags.V false

    /// Branch if Overflow Set
    let bvs mode cpu bus = (cpu, bus) ||> branch mode Flags.V true

    /// Clear Carry Flag
    let clc _ cpu bus =
        let p = clearFlag Flags.C cpu.p
        { cpu with p = p }, bus

    /// Set Carry Flag
    let sec _ cpu bus =
        let p = setFlag Flags.C cpu.p
        { cpu with p = p }, bus

    /// Clear Decimal Mode
    let cld _ cpu bus =
        let p = clearFlag Flags.D cpu.p
        { cpu with p = p }, bus

    /// Set Decimal Flag
    let sed _ cpu bus =
        let p = setFlag Flags.D cpu.p
        { cpu with p = p }, bus

    /// Clear Interrupt Disable
    let cli _ cpu bus =
        let p = clearFlag Flags.I cpu.p
        { cpu with p = p; suppressIrq = true }, bus

    /// Set Interrupt Disable
    let sei _ cpu bus =
        let p = setFlag Flags.I cpu.p
        { cpu with p = p; suppressIrq = true }, bus

    /// Clear Overflow Flag
    let clv _ cpu bus =
        let p = clearFlag Flags.V cpu.p
        { cpu with p = p }, bus

    /// BIT - Bit Test
    let bit mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let value, bus' = Bus.memRead addr bus

        let p =
            cpu.p
            |> updateFlag Flags.Z (cpu.a &&& value = 0uy)
            |> updateFlag Flags.N (value &&& Flags.N <> 0uy)
            |> updateFlag Flags.V (value &&& Flags.V <> 0uy)

        { cpu with p = p }, bus'

    /// アキュムレータやレジスタとメモリ上の値を比較
    let compare lhs mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let value, bus' = Bus.memRead addr bus
        let result = lhs - value
        let p = cpu.p |> updateFlag Flags.C (lhs >= value) |> setZeroNegativeFlags result

        { cpu with p = p }, bus'

    /// Compare
    let cmp mode cpu bus = (cpu, bus) ||> compare cpu.a mode

    /// Compare X Register
    let cpx mode cpu bus = (cpu, bus) ||> compare cpu.x mode

    /// Compare Y Register
    let cpy mode cpu bus = (cpu, bus) ||> compare cpu.y mode

    /// Decrement Memory
    let dec mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let result, bus' = Bus.memRead addr bus

        cpu
        |> fun c ->
            { c with
                p = cpu.p |> setZeroNegativeFlags (result - 1uy) },
            bus' |> Bus.memWrite addr (result - 1uy)

    /// Decrement X Register
    let dex _ cpu bus =
        let x = cpu.x - 1uy

        {
            cpu with
                x = x
                p = cpu.p |> setZeroNegativeFlags x
        },
        bus

    /// Decrement Y Register
    let dey _ cpu bus =
        let y = cpu.y - 1uy

        {
            cpu with
                y = y
                p = cpu.p |> setZeroNegativeFlags y
        },
        bus

    /// Increment Memory
    let inc mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let result, bus' = Bus.memRead addr bus

        {
            cpu with
                p = cpu.p |> setZeroNegativeFlags (result + 1uy)
        },
        bus' |> Bus.memWrite addr (result + 1uy)

    /// Increment X Register
    let inx _ cpu bus =
        let x = cpu.x + 1uy

        {
            cpu with
                x = x
                p = cpu.p |> setZeroNegativeFlags x
        },
        bus

    /// Increment Y Register
    let iny _ cpu bus =
        let y = cpu.y + 1uy

        {
            cpu with
                y = y
                p = cpu.p |> setZeroNegativeFlags y
        },
        bus

    /// Jump
    let jmp mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        { cpu with pc = addr }, bus

    /// Push a value to stack
    let push value cpu bus =
        let addr = 0x0100us + uint16 cpu.sp
        let bus' = Bus.memWrite addr value bus
        let sp' = cpu.sp - 1uy
        { cpu with sp = sp' }, bus'

    /// Push a 16-bit value to the stack (high byte first)
    let push16 value cpu bus =
        let hi = byte (value >>> 8)
        let lo = byte (value &&& 0xFFus)
        let cpu', bus' = push hi cpu bus
        push lo cpu' bus'

    /// Pull a value from stack
    let pull cpu bus =
        let sp' = cpu.sp + 1uy
        let addr = 0x0100us + uint16 sp'
        let value, bus' = Bus.memRead addr bus
        value, { cpu with sp = sp' }, bus'

    /// Pull a 16-bit value from the stack (low byte first)
    let pull16 cpu bus =
        let lo, cpu', bus' = pull cpu bus
        let hi, cpu'', bus'' = pull cpu' bus'
        let value = uint16 hi <<< 8 ||| uint16 lo
        value, cpu'', bus''

    /// Jump to Subroutine
    let jsr mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let cpu', bus' = (cpu, bus) ||> push16 (cpu.pc + 2us) // リターンポイント -1
        { cpu' with pc = addr }, bus'

    /// Return from Subroutine
    let rts _ cpu bus =
        let pc, cpu', bus' = pull16 cpu bus
        { cpu' with pc = pc + 1us }, bus' // +1 してリターンポイントにする

    /// Push Accumulator
    let pha _ cpu bus = (cpu, bus) ||> push cpu.a

    /// Push Processor Status
    let php _ cpu bus =
        let p = cpu.p |> setFlag (Flags.B ||| Flags.U)
        (cpu, bus) ||> push p

    /// Pull Processor Status
    let plp _ cpu bus =
        let p, cpu', bus' = pull cpu bus
        let p' = p |> clearFlag Flags.B |> setFlag Flags.U
        { cpu' with p = p'; suppressIrq = true }, bus'

    /// Pull Accumulator
    let pla _ cpu bus =
        let a, cpu', bus' = pull cpu bus
        let p = cpu'.p |> setZeroNegativeFlags a
        { cpu' with a = a; p = p }, bus'

    /// BRK - Force Break
    let brk _ cpu bus =
        if hasFlag Flags.B cpu.p then
            cpu, bus
        else
            let cpu, bus = (cpu, bus) ||> push16 (cpu.pc + 2us) ||> push cpu.p

            let pc, bus' = Bus.memRead16 0xFFFEus bus // BRK の場合は 0xFFFE に飛ぶ
            let p = cpu.p |> setFlag Flags.I

            { cpu with p = p; pc = pc }, bus'

    /// Return from Interrupt
    let rti _ cpu bus =
        let p, cpu', bus' = pull cpu bus
        let pc, cpu'', bus'' = pull16 cpu' bus'
        let p' = p |> clearFlag Flags.B |> setFlag Flags.U
        { cpu'' with p = p'; pc = pc }, bus''

    /// No Operation
    let nop _ cpu bus = cpu, bus

    /// Unofficial NOP
    /// メモリを読んで値は捨てる。副作用が起こる可能性がある NOP
    let nopWithMemRead mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let _, bus' = bus |> Bus.memRead addr
        cpu, bus'

    /// LDA -> TAX
    let lax mode cpu bus = (cpu, bus) ||> lda mode ||> tax mode

    /// Store A and X
    let sax mode cpu bus =
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let value = cpu.a &&& cpu.x
        cpu, bus |> Bus.memWrite addr value

    /// DEC -> CMP
    let dcp mode cpu bus = (cpu, bus) ||> dec mode ||> cmp mode

    /// (ISB) INC -> SBC
    let isc mode cpu bus = (cpu, bus) ||> inc mode ||> sbc mode

    /// ASL -> ORA
    let slo mode cpu bus =
        (cpu, bus) ||> asl mode ||> logicalInstr ORA mode

    /// ROL -> AND
    let rla mode cpu bus =
        (cpu, bus) ||> rol mode ||> logicalInstr AND mode

    /// ROR -> ADC
    let rra mode cpu bus = (cpu, bus) ||> ror mode ||> adc mode

    /// LSR -> EOR
    let sre mode cpu bus =
        (cpu, bus) ||> lsrInstr mode ||> logicalInstr EOR mode

    // A AND X -> ボローなし減算 -> X
    // NOTE: 本当に合ってるか確認したい
    let axs mode cpu bus =
        let x = cpu.a &&& cpu.x
        // ボローなし減算
        let addr = mode |> getOperandAddress cpu bus (cpu.pc + 1us)
        let value, bus' = Bus.memRead addr bus
        let valueInverted = ~~~value
        let sum = int x + int valueInverted
        let result = byte sum
        let isCarry = sum > 0xFF

        let p = cpu.p |> updateFlag Flags.C isCarry |> setZeroNegativeFlags result

        { cpu with x = result; p = p }, bus'

    // AND -> LSR
    // NOTE: 本当に合ってるか確認したい
    let alr mode cpu bus =
        (cpu, bus) ||> logicalInstr AND mode ||> lsrInstr mode

    let interruptDisabled cpu = hasFlag Flags.I cpu.p

    /// 主に APU からの割り込み要求
    /// 消費サイクル数も返す
    /// NOTE: 7 サイクル消費らしい
    /// NOTE: 割り込み禁止フラグの判定は呼び出し前にやる
    let irq cpu bus =

        let cpu, bus =
            (cpu, bus)
            ||> push16 cpu.pc
            ||> push (cpu.p |> clearFlag Flags.B |> setFlag Flags.U)

        let pc, bus' = Bus.memRead16 0xFFFEus bus // IRQ は 0xFFFE に飛ぶ
        let p = cpu.p |> setFlag Flags.I

        { cpu with p = p; pc = pc }, bus', 7u

    /// NMI 発生処理
    /// 消費サイクル数も返す
    /// NOTE: 7 サイクル消費らしい
    let interruptNmi cpu bus =
        let cpu', bus' = push16 cpu.pc cpu bus
        let p = cpu.p |> clearFlag Flags.B |> setFlag Flags.U

        let cpu2, bus2 = push p cpu' bus'
        let p' = p |> setFlag Flags.I
        let pc, busF = Bus.memRead16 0xFFFAus bus2
        { cpu2 with p = p'; pc = pc }, busF, 7u

    let clearSuppressIrq cpu = { cpu with suppressIrq = false }

    let isSuppressIrq cpu = cpu.suppressIrq

    let reset cpu bus =
        let pc, bus' = Bus.memRead16 0xFFFCus bus // リセットベクタを読み込む

        {
            cpu with
                a = 0uy
                x = 0uy
                y = 0uy
                p = Flags.I ||| Flags.U
                pc = pc
        },
        bus'

    /// 命令を関数に対応させる（プログラムカウンタを通常加算させる命令）
    let execMap =
        Map [ 
            ADC, adc
            AND, logicalInstr AND
            ASL, asl
            BIT, bit
            CLC, clc
            CLD, cld
            CLI, cli
            CLV, clv
            CMP, cmp
            CPX, cpx
            CPY, cpy
            DEC, dec
            DEX, dex
            DEY, dey
            EOR, logicalInstr EOR
            INC, inc
            INX, inx
            INY, iny
            LDA, lda
            LDX, ldx
            LDY, ldy
            LSR, lsrInstr
            NOP, nop
            ORA, logicalInstr ORA
            PHA, pha
            PHP, php
            PLA, pla
            PLP, plp
            ROL, rol
            ROR, ror
            SEC, sec
            SED, sed
            SEI, sei
            SBC, sbc
            STA, sta
            STX, stx
            STY, sty
            TAX, tax
            TAY, tay
            TSX, tsx
            TXS, txs
            TXA, txa
            TYA, tya

            // 非公式命令
            LAX_, lax
            SAX_, sax
            DCP_, dcp
            ISC_, isc
            NOP_, nop
            DOP_, nopWithMemRead // Double NOP
            TOP_, nopWithMemRead // Triple NOP
            SLO_, slo
            RLA_, rla
            RRA_, rra
            SBC_, sbc
            SRE_, sre
            AXS_, axs
            ALR_, alr
        ]
    
    let jumpMap =
        Map [
            BRK, brk
            BCC, bcc
            BCS, bcs
            BEQ, beq
            BMI, bmi
            BNE, bne
            BPL, bpl
            BVC, bvc
            BVS, bvs
            JMP, jmp
            JSR, jsr
            RTI, rti
            RTS, rts
        ]

    /// CPU を 1 命令だけ実行する
    let step cpu (bus: Bus.BusState) =

        let opcode, bus' = Bus.memRead cpu.pc bus
        let op, mode, size, cycles, penalty = decodeOpcode opcode

        let execInstr f m c b =
            let crsd =
                match m with
                | Absolute_X
                | Absolute_Y
                | Indirect_X
                | Indirect_Y ->
                    let addr = getOperandAddress c b c.pc m // 命令内と合わせて2回呼ぶことになるのはできればどうにかしたい
                    isPageCrossed c.pc addr
                | _ -> false

            (c, b) ||> f m ||> advancePC size, crsd

        let (cpu', bus''), crossed =
            match Map.tryFind op execMap with
            | Some f -> execInstr f mode cpu bus'
            | None ->
                match Map.tryFind op jumpMap with
                | Some f -> f mode cpu bus', false  // ブランチ系は命令内で追加サイクルを処理する
                | _ -> failwithf "Unsupported opcode: %A" op

        // サイクル追加発生可能性のある命令でページ境界をまたいだ場合はサイクル追加
        let pen = if penalty && crossed then 1u else 0u

        let consumed = cycles + pen + cpu'.cyclePenalty
        let cpu'' = { cpu' with cyclePenalty = 0u }
        cpu'', bus'', consumed
