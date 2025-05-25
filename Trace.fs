module Trace

open Instructions
open Cpu
open Bus

open Microsoft.FSharp.Reflection

(*
  imm: #$xx
  zp:  $xx
  abs: $xxxx
  zpx: $xx,X
  zpy: $xx,Y
  abx: $xxxx,X
  aby: $xxxx,Y
  inx: ($xx,X)
  iny: ($xx),Y
  ind: $xxxx

*)
let formatAddress mode (args: byte[]) =
  match mode with
  | Accumulator ->
    "A"
  | Immediate ->
    sprintf "#$%02X" args[0]
  | ZeroPage ->
    sprintf "$%02X" args[0]
  | ZeroPage_X ->
    sprintf "$%02X,X" args[0]
  | ZeroPage_Y ->
    sprintf "$%02X,Y" args[0]
  | Absolute ->
    sprintf "$%02X%02X" args[1] args[0]
  | Absolute_X ->
    sprintf "$%02X%02X,X" args[1] args[0]
  | Absolute_Y ->
    sprintf "$%02X%02X,Y" args[1] args[0]
  | Indirect_X ->
    sprintf "($%02X,X)" args[0]
  | Indirect_Y ->
    sprintf "($%02X),Y" args[0]
  | Indirect ->
    sprintf "$%02X%02X" args[1] args[0]
  | Relative -> // 未実装
    ""
  | Implied ->
    ""
  | NoneAddressing ->
    failwithf "Unsupported mode: %A" mode

let formatMemoryAccess cpu bus mode (args: byte[]) = // 作りかけ
  match mode with
  | Indirect_X ->
    let bpos = cpu.PC |> memRead bus
    let ptr = bpos + cpu.X |> uint16
    let addr = ptr |> memRead16 bus
    let value = addr |> memRead bus
    sprintf "@ %02X = %04X = %02X" ptr addr value
  | Indirect_Y ->
    let bpos = args[0] |> uint16
    let deRefBase = bpos |> memRead16 bus
    let deRef = deRefBase + (cpu.Y |> uint16)
    let value = deRef |> memRead bus
    sprintf "= %04X @ %04X = %02X" deRefBase deRef value
  | _ -> ""

let formatInstructionBytes opcode (args: byte[]) =
  Array.concat [ [| opcode |]; args ]
  |> Array.map (fun b -> b.ToString("X2"))
  |> String.concat " "

let formatCpuStatus cpu =
  sprintf "A:%02X X:%02X Y:%02X P:%02X SP:%02X" cpu.A cpu.X cpu.Y cpu.P cpu.SP

let getMnemonicName (x: 'T) =
  match FSharpValue.GetUnionFields(x, typeof<'T>) with
    | case, _ -> case.Name

let trace cpu bus =
  let opcode = cpu.PC |> memRead bus
  let op, mode, size, cycles = decodeOpcode opcode
  match op with
  | BRK -> "" // BRK の場合とりあえずトレースは飛ばしておく
  | _ ->
    let args = Array.init (int size - 1) (fun i -> cpu.PC + uint16(i + 1) |> memRead bus)
    let bin = formatInstructionBytes opcode args
    let addr = formatAddress mode args
    let mem = formatMemoryAccess cpu bus mode args

    let pc = sprintf "%04X" cpu.PC
    let mn = sprintf "%3s" (getMnemonicName op)
    let st = formatCpuStatus cpu

    sprintf "%-6s%-10s%-4s%-8s%-20s%s" pc bin mn addr mem st