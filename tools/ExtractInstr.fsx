#r "nuget: HtmlAgilityPack"

open System
open System.IO
open System.Text.RegularExpressions
open HtmlAgilityPack

/// 以下のファイルを事前にダウンロードする必要があります
// https://www.nesdev.org/obelisk-6502-guide/reference.html
// 公式命令HTMLファイルパス
let officialPath = "reference.html"

// https://www.nesdev.org/undocumented_opcodes.txt
// 非公式命令Textファイルパス
let unofficialPath = "undocumented_opcodes.txt"

/// 命令情報の型
type OpcodeInfo = {
    code: string
    mnemonic: string
    modeDu: string
    bytes: int
    cycles: int
    hasPenalty: bool
}

/// 公式・非公式の両方で使うモード変換（必要に応じて拡張してください）
let addressingModeBytes =
    Map [
        "Accumulator", 1
        "Immediate", 2
        "ZeroPage", 2
        "ZeroPage_X", 2
        "ZeroPage_Y", 2
        "Absolute", 3
        "Absolute_X", 3
        "Absolute_Y", 3
        "Indirect", 3
        "Indirect_X", 2
        "Indirect_Y", 2
        "Relative", 2
        "Implied", 1
        "NoneAddressing", 1
    ]

/// 公式命令用：テキスト整形
let normalize (s: string) =
    s.Replace("&nbsp;", " ")
     .Replace("\r", " ").Replace("\n", " ").Replace("\t", " ")
     .Replace("(", "").Replace(")", "").Replace(",", " ")
    |> fun x -> Regex.Replace(x, " +", " ")
    |> fun x -> x.Trim()

/// 公式命令用：名前を型ケース風に変換（例："ZeroPage_X" など）
let toDuCase (s: string) =
    let s = s.Trim()
    // 末尾の X Y を検出
    let suffix =
        if s.EndsWith("X") || s.EndsWith("Y") then
            Some (s.Substring(s.Length - 1))
        else None

    // suffixを取り除いた本体部分
    let core =
        match suffix with
        | Some suf -> s.Substring(0, s.Length - 1)
        | None -> s

    // core は _ と空白で分割し、単語ごとにキャピタライズして連結
    let coreProcessed =
        core.Split([| ' '; '_' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun w -> w.Substring(0,1).ToUpper() + w.Substring(1).ToLower())
        |> String.concat ""

    match suffix with
    | Some suf ->
        coreProcessed + "_" + suf
    | None -> coreProcessed

let parseCyclesAndPenalty (text: string) : int * bool =
    let baseText = text.Trim()
    let parts = baseText.Split(' ')
    let baseCycles =
        match System.Int32.TryParse(parts[0]) with
        | true, n -> n
        | _ -> 0
    let hasPenalty = parts.Length > 1
    baseCycles, hasPenalty

/// 必要に応じてファイル名やパスを変更してください
let parseOfficialHtml (path: string) : OpcodeInfo list =
    let doc = HtmlDocument()
    doc.Load(path)

    doc.DocumentNode.SelectNodes("//h3")
    |> Seq.choose (fun h3 ->
        let title = h3.InnerText.Trim()
        if not (title.Contains(" - ")) then None else
        let mnemonic = title.Split(" - ").[0].Trim().ToUpper()
        let table =
            h3.SelectNodes("following-sibling::table")
            |> Seq.tryFind (fun t -> t.InnerText.Contains("Addressing Mode"))
        table |> Option.map (fun tbl -> mnemonic, tbl))
    |> Seq.collect (fun (mnemonic, table) ->
        table.SelectNodes(".//tr") |> Seq.cast<HtmlNode> |> Seq.skip 1
        |> Seq.choose (fun row ->
            let cols = row.SelectNodes("td")
            if cols.Count < 4 then None else
            let rawMode = normalize cols[0].InnerText
            let codeHex = cols[1].InnerText.Trim().TrimStart('$')
            let cycles, penalty = parseCyclesAndPenalty cols[3].InnerText
            if not (Regex.IsMatch(codeHex, @"^[0-9A-Fa-f]{2}$")) then None else
            let modeCase = toDuCase rawMode
            let bytes = addressingModeBytes.TryFind modeCase |> Option.defaultValue 1
            Some {
                code = codeHex.ToUpper();
                mnemonic = mnemonic;
                modeDu = modeCase;
                bytes = bytes;
                cycles = cycles
                hasPenalty = penalty
            }
        ))
    |> Seq.toList

/// アドレッシングモードの変換
let keyMap =
    dict [
        "Implied", "Implied"
        "Immediate", "Immediate"
        "Zero Page", "ZeroPage"
        "Zero Page,X", "ZeroPage_X"
        "Zero Page,Y", "ZeroPage_Y"
        "Absolute", "Absolute"
        "Absolute,X", "Absolute_X"
        "Absolute,Y", "Absolute_Y"
        "(Indirect,X)", "Indirect_X"
        "(Indirect),Y", "Indirect_Y"
    ]

/// 非公式命令の解析関数
let parseUnofficialText (path: string) : OpcodeInfo list =
    let lines = File.ReadAllLines(path) |> Array.toList

    // 区切り行 (=3D=3D=3D=3D... の行) のインデックスを取得
    let sectionIndices =
        lines
        |> List.mapi (fun i line -> i, line)
        |> List.choose (fun (i, line) ->
            if line = "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D" then Some i else None)

    // 区切りの範囲で分割し、各ブロックの先頭行を取得して命令名とその詳細行群を抽出
    let blocks =
        sectionIndices
        |> List.pairwise
        |> List.map (fun (startIdx, nextStartIdx) ->
            // 命令名は区切り行の1つ前
            let op = lines[startIdx - 1]
            let m = Regex.Match(op, @"^(.+?)\s")
            if m.Success then
                let name =
                    match m.Groups[1].Value with
                    | "AAC" -> "ANC"
                    | "AAX" -> "SAX"
                    | "ASR" -> "ALR"
                    | v -> v

                // この区間の行を取り出す（区切り行の次から次の区切り行まで）
                let blockLines = lines |> List.skip (startIdx + 1) |> List.take (nextStartIdx - startIdx - 1)

                // "------------" で始まる行の位置を探す
                match blockLines |> List.tryFindIndex (fun v -> v.StartsWith("------------")) with
                | Some sepIndex ->
                    // sepIndexの次の行から空行までを取得
                    let afterSep = blockLines |> List.skip (sepIndex + 1)
                    let relevantLines =
                        match afterSep |> List.tryFindIndex ((=) "") with
                        | Some emptyLineIndex -> afterSep |> List.take emptyLineIndex
                        | None -> afterSep
                    Some (name, relevantLines)
                | None -> None
            else None)
        |> List.choose id

    // 最後の区切り行からファイル末尾までの処理（最後のブロック）
    let lastBlockOpt =
        match List.tryLast sectionIndices with
        | Some lastIdx when lastIdx + 1 < lines.Length ->
            let op = lines[lastIdx - 1]
            let m = Regex.Match(op, @"^(.+?)\s")
            if m.Success then
                let name = m.Groups[1].Value
                let blockLines = lines |> List.skip (lastIdx + 1)
                match blockLines |> List.tryFindIndex (fun v -> v.StartsWith("------------")) with
                | Some sepIndex ->
                    let afterSep = blockLines |> List.skip (sepIndex + 1)
                    let relevantLines =
                        match afterSep |> List.tryFindIndex ((=) "") with
                        | Some emptyLineIndex -> afterSep |> List.take emptyLineIndex
                        | None -> afterSep
                    Some (name, relevantLines)
                | None -> None
            else None
        | _ -> None

    // 最後のブロックがあれば追加
    let allDefs = 
        match lastBlockOpt with
        | Some b -> blocks @ [b]
        | None -> blocks

    // keyMap は既に定義されている前提

    // 命令情報の抽出
    allDefs
    |> List.collect (fun (name, lines) ->
        lines
        |> List.choose (fun line ->
            let parts = line.Split('|') |> Array.map (fun s -> s.Trim())
            if parts.Length >= 5 then
                match keyMap.TryGetValue(parts[0]) with
                | true, mode ->
                    let code = parts[2].Replace("$", "").ToUpper()
                    let bytes = int parts[3]
                    let cyclePart = parts[4].Replace(" ", "")
                    let hasPageCrossPenalty = cyclePart.Contains("*")
                    let cyclesStr = cyclePart.Replace("*", "").Replace("-", "0")
                    let cycles = int cyclesStr
                    Some {
                        code = code
                        mnemonic = name + "_"
                        modeDu = mode
                        bytes = bytes
                        cycles = cycles
                        hasPenalty = hasPageCrossPenalty
                    }
                | _ -> None
            else None))

/// 命令一覧をまとめて F# モジュールを生成する関数
let generateInstructionsFs (opcodes: OpcodeInfo list) =
    let isUnofficial (mnem: string) = mnem.EndsWith("_")

    let formatMnemonics (mnemonics: string list) : string =
        mnemonics
        |> List.distinct
        |> List.sort
        |> List.groupBy (fun name -> name[0])
        |> List.sortBy fst
        |> List.map (fun (_, group) ->
            group
            |> List.sort
            |> String.concat " | "
            |> sprintf "        | %s")
        |> String.concat "\n"

    let officialMnemonics =
        opcodes
        |> List.filter (fun o -> not (isUnofficial o.mnemonic))
        |> List.map (fun o -> o.mnemonic)
        |> formatMnemonics

    let unofficialMnemonics =
        opcodes
        |> List.filter (fun o -> isUnofficial o.mnemonic)
        |> List.map (fun o -> o.mnemonic)
        |> formatMnemonics

    let allModes =
        opcodes
        |> List.map (fun o -> o.modeDu)
        |> List.distinct
        |> List.sort
        |> List.map (sprintf "        | %s")
        |> String.concat "\n"

    let opcodeEntries =
        opcodes
        |> List.sortBy (fun o -> Convert.ToByte(o.code, 16))
        |> List.map (fun o ->
            sprintf "            0x%suy, (%s, %s, %dus, %du, %b)" o.code o.mnemonic o.modeDu o.bytes o.cycles o.hasPenalty)
        |> String.concat "\n"

    $"""namespace HamicomEmu.Cpu

module Instructions =

    type mnemonics =
{officialMnemonics}
{if unofficialMnemonics <> "" then "\n// 非公式命令\n" + unofficialMnemonics else ""}

    type addressingMode =
{allModes}

    /// 命令情報テーブル
    // コード, (ニーモニック, アドレッシングモード, バイト長, サイクル数, 追加サイクルが発生しうるかのフラグ)
    let opcodeTable : Map<byte, mnemonics * addressingMode * uint16 * uint * bool> =
        Map [
{opcodeEntries}
        ]

    /// オペコードをデコードして命令情報を取得（存在しない場合は例外）
    let decodeOpcode (opcode: byte) =
        opcodeTable[opcode]
"""

/// メイン処理

let officialOpcodes = parseOfficialHtml officialPath
let unofficialOpcodes = parseUnofficialText unofficialPath

let allOpcodes = officialOpcodes @ unofficialOpcodes

let writeCRLF (path: string) (content: string) =
    use writer = new StreamWriter(path, false, System.Text.Encoding.UTF8)
    content.Split('\n')
    |> Array.iter (fun line ->
        writer.Write(line.TrimEnd())
        writer.Write("\r\n"))

let outputPath = "Instructions.fs"
let fsCode = generateInstructionsFs allOpcodes
writeCRLF outputPath fsCode

printfn "✔ Instructions.fs に公式・非公式命令をまとめて出力しました。"
