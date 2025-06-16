# famicom-emulator-fs
のんびり作られるファミコンエミュレータです。

## 開発めも
関数型言語習得を何回も挫折してた初心者が、F# とゲーム機内部仕様の勉強をしながらファミコンエミュレータをつくっていくリポジトリとなっております。ゴールは決めてません。

関数型に慣れるためになるべくクラスは使わない方針で行きます。

とりあえずの画面表示のために MonoGame を使用してます。

### 進捗状況

- CPU 命令実装
- 非公式 CPU 命令不完全実装
- 限定的なスクロール機能実装
- 8x8 スプライト表示機能実装
- 矩形波x2、三角波、ノイズ各音声チャンネル実装

### 実行オプション
オプションなし:

```nes/Alter_Ego.nes``` を実行します

```ファイルパス```:

指定された NES ファイルを実行します

|入力対応  |↑|↓|←|→|Select|Start|B|A|
|:--------:|-|-|-|-|------|-----|-|-|
|キーボード|↑|↓|←|→|Space|Enter|S|A|

## ビルド方法
.NET 8 SDK が入っていればコマンドラインから dotnet コマンドでビルドできます。

必要なもの
- .NET 8 SDK
- Windows / Linux / macOS（MonoGame DesktopGL なのでたぶん動くはずです）

### とりあえず実行してみる
```dotnet run```

### ビルド
```dotnet build```

## 参考
- [F# for Fun and Profit eBook 日本語訳](https://matarillo.github.io/fsharp_for_fun_and_profit-ja/index.html
  "読んでて楽しく役立つ入門書です おすすめです！")
- [Writing NES Emulator in Rust](https://bugzmanov.github.io/nes_ebook/)
- [プラスウイングTV: ファミコンエミュレータを作る (Make a NES Emulator in Rust)](https://www.youtube.com/watch?v=B-0bw4q6Pxo&list=PLp_EUEO9JJP1cMwbqzOHFOI9gPH_zoO0U
  "動画を見ながらコーディングをしています 心強いです")
- [NESDev: 6502 Instruction Reference](https://www.nesdev.org/obelisk-6502-guide/reference.html
  "CPUの仕様の参考に また Instruction.fs はこのページ内容から抽出して自動生成してます")
- [GitHub: Famicom-Fsharp - instructions.fs](https://github.com/kxkx5150/Famicom-Fsharp/blob/main/src/emulator/instructions.fs
  "型の使い方を参考にさせていただきました")
- etc.