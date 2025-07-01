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

  // PC: program counter
  // SP: stack pointer
  // P: processor status (flags)
  type CpuState =
    { A: byte
      X: byte
      Y: byte
      PC: uint16
      SP: byte
      P: byte
      // NOTE: ブランチ命令用のサイクル増分 本当はもっと例外なくスマートにやりたい
      cyclePenalty: uint
    }

  let initial =
    { A = 0uy
      X = 0uy
      Y = 0uy
      PC = 0us
      SP = 0xFDuy
      P = Flags.I ||| Flags.U
      cyclePenalty = 0u
    }

  let isPageCrossed (a: uint16) (b: uint16) =
    (a &&& 0xFF00us) <> (b &&& 0xFF00us)

  /// アドレッシングモードからオペランドとアドレスを割り出す
  let getOperandAddress cpu bus pc mode =
    match mode with
    | Accumulator ->
      failwithf "Unsupported mode: %A" mode
    | Immediate ->
      pc
    | ZeroPage ->
      let addr, _ = Bus.memRead pc bus
      addr |> uint16
    | Absolute ->
      let addr, _ = Bus.memRead16 pc bus
      addr |> uint16
    | ZeroPage_X ->
      let pos, _ = Bus.memRead pc bus
      let addr = pos + cpu.X |> uint16
      addr
    | ZeroPage_Y ->
      let pos, _ = Bus.memRead pc bus
      let addr = pos + cpu.Y |> uint16
      addr
    | Absolute_X ->
      let bpos, _ = Bus.memRead16 pc bus
      let addr = bpos + (cpu.X |> uint16)
      addr
    | Absolute_Y ->
      let bpos, _ = Bus.memRead16 pc bus
      let addr = bpos + (cpu.Y |> uint16)
      addr
    | Indirect_X ->
      let bpos, _ = Bus.memRead pc bus
      let ptr = bpos + cpu.X
      let addr, _ = Bus.memRead16ZeroPage ptr bus
      addr
    | Indirect_Y ->
      let bpos, _ = Bus.memRead pc bus
      let deRefBase, _ = Bus.memRead16ZeroPage bpos bus
      let deRef = deRefBase + (cpu.Y |> uint16)
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
    | Implied ->
      pc
    // | NoneAddressing ->
    //   failwithf "Unsupported mode: %A" mode

  /// 値によってZNフラグをセットする
  let setZeroNegativeFlags value p = 
    p
    |> updateFlag Flags.Z (value = 0uy)
    |> updateFlag Flags.N (value &&& 0b1000_0000uy <> 0uy)

  /// プログラムカウンタを進める
  let advancePC offset cpu bus =
    { cpu with PC = cpu.PC + offset }, bus

  /// ビット論理演算
  let logicalInstr op mode cpu bus = // AND, EOR, ORA
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let value, bus' = Bus.memRead addr bus

    let a =
      match op with
      | AND -> (&&&) cpu.A value
      | EOR -> (^^^) cpu.A value
      | ORA -> (|||) cpu.A value
      | _   -> failwithf "Unsupported logical instruction: %A" op

    let p = cpu.P |> setZeroNegativeFlags a
    { cpu with A = a; P = p }, bus'

  /// 加算
  let adc mode cpu bus = // Add with Carry
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let value, bus' = Bus.memRead addr bus
    let carry = if hasFlag Flags.C cpu.P then 1 else 0
    let sum = int cpu.A + int value + carry
    let result = byte sum
    let isCarry = sum > 0xFF
    let isOverflow =
      // オーバーフローが発生する条件:
      // 1. A と value の符号が同じ
      // 2. 結果の符号が A と異なる
      cpu.A ^^^ value &&& 0x80uy = 0uy && cpu.A ^^^ result &&& 0x80uy <> 0uy

    let p =
      cpu.P
      |> updateFlag Flags.C isCarry
      |> updateFlag Flags.V isOverflow
      |> setZeroNegativeFlags result

    { cpu with A = result; P = p }, bus'

  /// 減算
  let sbc mode cpu bus = // Subtract with Carry
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let value, bus' = Bus.memRead addr bus
    let carry = if hasFlag Flags.C cpu.P then 1 else 0
    let valueInverted = ~~~ value
    let sum = int cpu.A + int valueInverted + carry
    let result = byte sum
    let isCarry = sum > 0xFF
    let isOverflow =
      // ADC とオーバーフローの条件が違うので注意
      cpu.A ^^^ value &&& 0x80uy <> 0uy && cpu.A ^^^ result &&& 0x80uy <> 0uy

    let p =
      cpu.P
      |> updateFlag Flags.C isCarry
      |> updateFlag Flags.V isOverflow
      |> setZeroNegativeFlags result

    { cpu with A = result; P = p }, bus'

  let ld setReg mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let v, bus = Bus.memRead addr bus
    let p = setZeroNegativeFlags v cpu.P
    let cpu = cpu |> setReg v
    { cpu with P = p }, bus

  /// Load Accumulator
  let lda mode cpu bus =
    (cpu, bus) ||> ld (fun v cpu -> { cpu with A = v }) mode

  /// Load X Register
  let ldx mode cpu bus =
    (cpu, bus) ||> ld (fun v cpu -> { cpu with X = v }) mode

  /// Load Y Register
  let ldy mode cpu bus =
    (cpu, bus) ||> ld (fun v cpu -> { cpu with Y = v }) mode

  /// Store Accumulator
  let sta mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    cpu, bus |> Bus.memWrite addr cpu.A

  /// Store X Register
  let stx mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    cpu, bus |> Bus.memWrite addr cpu.X

  /// Store Y Register
  let sty mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    cpu, bus |> Bus.memWrite addr cpu.Y

  /// Transfer Accumulator to X
  let tax _ cpu bus =
    let x = cpu.A
    let p = cpu.P |> setZeroNegativeFlags x
    { cpu with X = x; P = p }, bus

  /// Transfer Accumulator to Y
  let tay _ cpu bus =
    let y = cpu.A
    let p = cpu.P |> setZeroNegativeFlags y
    { cpu with Y = y; P = p }, bus

  /// Transfer X to Accumulator
  let txa _ cpu bus =
    let a = cpu.X
    let p = cpu.P |> setZeroNegativeFlags a
    { cpu with A = a; P = p }, bus

  /// Transfer Y to Accumulator
  let tya _ cpu bus =
    let a = cpu.Y
    let p = cpu.P |> setZeroNegativeFlags a
    { cpu with A = a; P = p }, bus

  /// Transfer Y to Accumulator
  let tsx _ cpu bus =
    let x = cpu.SP
    let p = cpu.P |> setZeroNegativeFlags x
    { cpu with X = x; P = p }, bus

  /// Transfer X to Stack Pointer
  let txs _ cpu bus =
    let sp = cpu.X
    { cpu with SP = sp }, bus

  /// 左シフトしてキャリーフラグ設定
  let shiftLeftAndUpdate value status =
    let carry = value &&& 0b1000_0000uy > 0uy
    let result = value <<< 1
    let p =
      status
      |> updateFlag Flags.C carry
    result, p

  /// 右シフトしてキャリーフラグ設定
  let shiftRightAndUpdate value status =
    let carry = value &&& 0b0000_0001uy > 0uy
    let result = value >>> 1
    let p =
      status
      |> updateFlag Flags.C carry
    result, p

  /// シフト演算
  /// carryInFn はすでに立ってるキャリーフラグを演算結果に含める関数
  let modifyWithShift mode shiftFn carryInFn cpu bus =
    let cIn = hasFlag Flags.C cpu.P

    let value, writeBack =
      match mode with
      | Accumulator ->
          cpu.A, fun r -> { cpu with A = r }, bus
      | _ ->
          let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
          let v, b' = Bus.memRead addr bus
          v, fun r -> cpu, b' |> Bus.memWrite addr r

    let shifted, p = shiftFn value cpu.P
    let result = carryInFn shifted cIn
    let p' = setZeroNegativeFlags result p
    let cpu', bus' = writeBack result
    { cpu' with P = p' }, bus'

  let asl mode cpu bus =
    (cpu, bus) ||> modifyWithShift mode shiftLeftAndUpdate (fun r _ -> r)

  let lsrInstr mode cpu bus =
    (cpu, bus) ||> modifyWithShift mode shiftRightAndUpdate (fun r _ -> r)

  let rol mode cpu bus =
    (cpu, bus) ||> modifyWithShift mode shiftLeftAndUpdate (fun r carryBefore ->
      r ||| (if carryBefore then 1uy else 0uy))

  let ror mode cpu bus =
    (cpu, bus) ||> modifyWithShift mode shiftRightAndUpdate (fun r carryBefore ->
      r ||| (if carryBefore then 0b1000_0000uy else 0uy))

  /// 分岐はこの関数内で PC の進みを管理する
  let branch mode flag expected cpu bus =
    let PcAfterInstruction = cpu.PC + 2us 
    let target = mode |> getOperandAddress cpu bus (cpu.PC + 1us)

    if hasFlag flag cpu.P <> expected then
      (cpu, bus) ||> advancePC 2us
    else
      // 条件達成で +1, ページ境界跨ぎでさらに +1
      let pen = 1u + if isPageCrossed PcAfterInstruction target then 1u else 0u
      { cpu with PC = target; cyclePenalty = pen }, bus

  /// BCC - Branch if Carry Clear
  let bcc mode cpu bus =
    (cpu, bus) ||> branch mode Flags.C false

  /// BCS - Branch if Carry Set
  let bcs mode cpu bus =
    (cpu, bus) ||> branch mode Flags.C true

  /// BEQ - Branch if Equal
  let beq mode cpu bus =
    (cpu, bus) ||> branch mode Flags.Z true

  /// BNE - Branch if Not Equal
  let bne mode cpu bus =
    (cpu, bus) ||> branch mode Flags.Z false

  /// Branch if Minus
  let bmi mode cpu bus =
    (cpu, bus) ||> branch mode Flags.N true

  /// Branch if Positive
  let bpl mode cpu bus =
    (cpu, bus) ||> branch mode Flags.N false

  /// Branch if Overflow Clear
  let bvc mode cpu bus =
    (cpu, bus) ||> branch mode Flags.V false

  /// Branch if Overflow Set
  let bvs mode cpu bus =
    (cpu, bus) ||> branch mode Flags.V true

  /// Clear Carry Flag
  let clc _ cpu bus =
    let p = clearFlag Flags.C cpu.P
    { cpu with P = p }, bus

  /// Set Carry Flag
  let sec _ cpu bus =
    let p = setFlag Flags.C cpu.P
    { cpu with P = p }, bus

  /// Clear Decimal Mode
  let cld _ cpu bus =
    let p = clearFlag Flags.D cpu.P
    { cpu with P = p }, bus

  /// Set Decimal Flag
  let sed _ cpu bus =
    let p = setFlag Flags.D cpu.P
    { cpu with P = p }, bus

  /// Clear Interrupt Disable
  let cli _ cpu bus =
    let p = clearFlag Flags.I cpu.P
    { cpu with P = p }, bus

  /// Set Interrupt Disable
  let sei _ cpu bus =
    let p = setFlag Flags.I cpu.P
    { cpu with P = p }, bus

  /// Clear Overflow Flag
  let clv _ cpu bus =
    let p = clearFlag Flags.V cpu.P
    { cpu with P = p }, bus

  /// BIT - Bit Test
  let bit mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let value, bus' = Bus.memRead addr bus
    let p =
      cpu.P
      |> updateFlag Flags.Z (cpu.A &&& value    = 0uy)
      |> updateFlag Flags.N (value &&& Flags.N <> 0uy)
      |> updateFlag Flags.V (value &&& Flags.V <> 0uy)
    { cpu with P = p }, bus'

  /// アキュムレータやレジスタとメモリ上の値を比較
  let compare lhs mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let value, bus' = Bus.memRead addr bus
    let result = lhs - value
    let p =
      cpu.P
      |> updateFlag Flags.C (lhs >= value)
      |> setZeroNegativeFlags result

    { cpu with P = p }, bus'

  /// Compare
  let cmp mode cpu bus =
    (cpu, bus) ||> compare cpu.A mode

  /// Compare X Register
  let cpx mode cpu bus =
    (cpu, bus) ||> compare cpu.X mode

  /// Compare Y Register
  let cpy mode cpu bus =
    (cpu, bus) ||> compare cpu.Y mode

  /// Decrement Memory
  let dec mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let result, bus' = Bus.memRead addr bus
    cpu |> fun c -> { c with P = cpu.P |> setZeroNegativeFlags (result - 1uy) }, bus' |> Bus.memWrite addr (result - 1uy)

  /// Decrement X Register
  let dex _ cpu bus =
    let x = cpu.X - 1uy
    { cpu with X = x; P = cpu.P |> setZeroNegativeFlags x }, bus

  /// Decrement Y Register
  let dey _ cpu bus =
    let y = cpu.Y - 1uy
    { cpu with Y = y; P = cpu.P |> setZeroNegativeFlags y }, bus

  /// Increment Memory
  let inc mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let result, bus' = Bus.memRead addr bus
    { cpu with P = cpu.P |> setZeroNegativeFlags (result + 1uy) }, bus' |> Bus.memWrite addr (result + 1uy)

  /// Increment X Register
  let inx _ cpu bus =
    let x = cpu.X + 1uy
    { cpu with X = x; P = cpu.P |> setZeroNegativeFlags x }, bus

  /// Increment Y Register
  let iny _ cpu bus =
    let y = cpu.Y + 1uy
    { cpu with Y = y; P = cpu.P |> setZeroNegativeFlags y }, bus

  /// Jump
  let jmp mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    { cpu with PC = addr }, bus

  /// Push a value to stack
  let push value cpu bus =
    let addr = 0x0100us + uint16 cpu.SP
    let bus' = Bus.memWrite addr value bus
    let sp' = cpu.SP - 1uy
    { cpu with SP = sp' }, bus'

  /// Push a 16-bit value to the stack (high byte first)
  let push16 value cpu bus =
    let hi = byte (value >>> 8)
    let lo = byte (value &&& 0xFFus)
    let cpu', bus' = push hi cpu bus
    push lo cpu' bus'

  /// Pull a value from stack
  let pull cpu bus =
    let sp' = cpu.SP + 1uy
    let addr = 0x0100us + uint16 sp'
    let value, bus' = Bus.memRead addr bus
    value, { cpu with SP = sp' }, bus'

  /// Pull a 16-bit value from the stack (low byte first)
  let pull16 cpu bus =
    let lo, cpu', bus' = pull cpu bus
    let hi, cpu'', bus'' = pull cpu' bus'
    let value = uint16 hi <<< 8 ||| uint16 lo
    value, cpu'', bus''

  /// Jump to Subroutine
  let jsr mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let cpu', bus' = (cpu, bus) ||> push16 (cpu.PC + 2us) // リターンポイント -1
    { cpu' with PC = addr }, bus'

  /// Return from Subroutine
  let rts _ cpu bus =
    let pc, cpu', bus' = pull16 cpu bus
    { cpu' with PC = pc + 1us }, bus' // +1 してリターンポイントにする

  /// Push Accumulator
  let pha _ cpu bus =
    (cpu, bus) ||> push cpu.A

  /// Push Processor Status
  let php _ cpu bus =
    let p = cpu.P |> setFlag (Flags.B ||| Flags.U)
    (cpu, bus) ||> push p

  /// Pull Processor Status
  let plp _ cpu bus =
    let p, cpu', bus' = pull cpu bus
    let p' =
      p
      |> clearFlag Flags.B
      |> setFlag Flags.U
    { cpu' with P = p'}, bus'

  /// Pull Accumulator
  let pla _ cpu bus =
    let a, cpu', bus' = pull cpu bus
    let p = cpu'.P |> setZeroNegativeFlags a
    { cpu' with A = a; P = p }, bus'

  /// BRK - Force Break
  let brk _ cpu bus =
    // まだ未完成
    if hasFlag Flags.I cpu.P then
      cpu, bus
    else
      let cpu, bus =
        (cpu, bus)
        ||> push16 (cpu.PC + 1us)
        ||> push cpu.P

      let pc, bus' = Bus.memRead16 0xFFFEus bus // BRK の場合は 0xFFFE に飛ぶ
      let p = cpu.P |> setFlag Flags.B

      { cpu with P = p; PC = pc }, bus'

  /// Return from Interrupt
  let rti _ cpu bus =
    let p, cpu', bus' = pull cpu bus
    let pc, cpu'', bus'' = pull16 cpu' bus'
    let p' =
      p
      |> clearFlag Flags.B
      |> setFlag Flags.U
    { cpu'' with P = p' ; PC = pc }, bus''

  /// No Operation
  let nop _ cpu bus =
    cpu, bus

  /// LDA -> TAX
  let lax mode cpu bus =
    (cpu, bus) ||> lda mode ||> tax mode

  /// Store A and X
  let sax mode cpu bus =
    let addr = mode |> getOperandAddress cpu bus (cpu.PC + 1us)
    let value = cpu.A &&& cpu.X
    cpu, bus |> Bus.memWrite addr value

  /// DEC -> CMP
  let dcp mode cpu bus =
    (cpu, bus) ||> dec mode ||> cmp mode

  /// (ISC) INC -> SBC
  let isb mode cpu bus =
    (cpu, bus) ||> inc mode ||> sbc mode

  /// ASL -> ORA
  let slo mode cpu bus =
    (cpu, bus) ||> asl mode ||> logicalInstr ORA mode

  /// ROL -> AND
  let rla mode cpu bus =
    (cpu, bus) ||> rol mode ||> logicalInstr AND mode

  /// ROR -> ADC
  let rra mode cpu bus =
    (cpu, bus) ||> ror mode ||> adc mode

  /// LSR -> EOR
  let sre mode cpu bus =
    (cpu, bus) ||> lsrInstr mode ||> logicalInstr EOR mode

  /// 主に APU からの割り込み要求
  let irq cpu bus =
    if hasFlag Flags.I cpu.P then
      cpu, bus
    else
      let cpu, bus =
        (cpu, bus)
        ||> push16 (cpu.PC + 1us)
        ||> push cpu.P

      let pc, bus' = Bus.memRead16 0xFFFEus bus // IRQ は 0xFFFE に飛ぶ
      let p = cpu.P |> clearFlag Flags.B

      let busF = Bus.clearIrqStatus bus' // 割り込み要求を消す

      { cpu with P = p; PC = pc }, busF

  /// NMI 発生処理
  let interruptNmi cpu bus =
    let cpu', bus' = push16 cpu.PC cpu bus
    let p =
      cpu.P |> clearFlag Flags.B
            |> setFlag Flags.U

    let cpu2, bus2 = push p cpu' bus'
    let p' = p |> setFlag Flags.I

    let bus3 = bus2 |> Bus.tick |> Bus.tick
    let pc, busF = Bus.memRead16 0xFFFAus bus3
    { cpu2 with P = p'; PC = pc }, busF


  let reset cpu bus =
    let pc, bus' = Bus.memRead16 0xFFFCus bus // リセットベクタを読み込む
    { cpu with
        A = 0uy
        X = 0uy
        Y = 0uy
        P = Flags.I ||| Flags.U
        PC = pc 
    }, bus'

  /// 命令を関数に対応させる
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
      ISB_, isb
      NOP_, nop
      SLO_, slo
      RLA_, rla
      RRA_, rra
      SBC_, sbc
      SRE_, sre
    ]


  /// CPU を 1 命令だけ実行する
  let step cpu (bus : Bus.BusState) =

    let cpu, bus = // NMI
      match Bus.pollNmiStatus bus with
      | b, Some _ -> interruptNmi cpu b
      | b, None -> cpu, b

    let cpu, bus = // IRQ
      if bus.apu.irq then
        irq cpu bus
      else
        cpu, bus

    let opcode, bus' = Bus.memRead cpu.PC bus
    let op, mode, size, cycles, penalty = decodeOpcode opcode
    let execInstr f m c b =
      let crsd =
        match m with
        | Absolute_X | Absolute_Y | Indirect_X | Indirect_Y ->
          let addr = getOperandAddress c b c.PC m // 命令内と合わせて2回呼ぶことになるのはできればどうにかしたい
          isPageCrossed c.PC addr
        | _ -> false
      (c, b) ||> f m ||> advancePC size, crsd

    let (cpu', bus''), crossed =
      match Map.tryFind op execMap with
      | Some f -> execInstr f mode cpu bus'
      | None ->
        match op with
        | BCC -> bcc mode cpu bus', false // ブランチ系は命令内で追加サイクルを処理する
        | BCS -> bcs mode cpu bus', false
        | BEQ -> beq mode cpu bus', false
        | BMI -> bmi mode cpu bus', false
        | BNE -> bne mode cpu bus', false
        | BPL -> bpl mode cpu bus', false
        | BVC -> bvc mode cpu bus', false
        | BVS -> bvs mode cpu bus', false
        | JMP -> jmp mode cpu bus', false
        | JSR -> jsr mode cpu bus', false
        | RTI -> rti mode cpu bus', false
        | RTS -> rts mode cpu bus', false
        | BRK -> execInstr brk mode cpu bus'
        | _ -> failwithf "Unsupported opcode: %A" op
    
    // サイクル追加発生可能性のある命令でページ境界をまたいだ場合はサイクル追加
    let pen = if penalty && crossed then 1u else 0u

    let consumed = cycles + pen + cpu'.cyclePenalty
    let cpu'' = { cpu' with cyclePenalty = 0u }
    cpu'', bus'', consumed
