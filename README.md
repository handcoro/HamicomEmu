# HamicomEmu
F# でのんびり作られるファミコンエミュレータです。

## 開発めも
関数型言語習得を何回も挫折してた初心者が、F# とゲーム機内部仕様の勉強をしながらファミコンエミュレータをつくっていくリポジトリとなっております。ゴールは決めてません。なにかユニークな機能がつけられればいいですね。

関数型に慣れるためになるべくクラスは使わない方針で行きます。

とりあえずの画面表示や入力や音声出力のために MonoGame を使用してます。

---

## 最近の進捗・現状メモ（2025 年 7 月）

- 各種マッパー（MMC1, NROM, UxROM, VRC1, J87）に対応
- スプライト機能の強化(優先度・8x16 サイズ対応)
- 音声出力を実機に近づけるために APU の各チャンネルごとに tick を導入
- 割り込み処理の再現度向上（CLI, SEI, PLP の IRQ 抑制など）

---

### もうちょっと詳しい開発の現状

- 副作用はできるだけコンパクトに封じ込めたい
- スクロールまわりのしっかりとした実装を継続中です
- 遅延に対処するために音声出力は MonoGame を介さない仕組みも考えてます
- コードの見通し・保守性も意識して、細かいリファクタやファイル整理をちょこちょこやっています

---

### TODO

- GUI の整備
- スクロールも含めて画面表示の正確さの向上
- PPU のパフォーマンス最適化
- tick の高速化と正確性の両立
- サウンドの再現度向上
- サウンドのフィルタリング方法の模索
- サウンドの遅延に対処したい
- 非公式命令は必要に応じて実装を検討していきます
- テスト ROM で互換性を確認しつつ、ちょっとずつ修正
- 気になったところから修正や実装をやっていきたいですね

---

### 実行オプション
オプションなし:

```nes/Alter_Ego.nes``` を実行します

```ファイルパス```:

指定された NES ファイルを実行します

|入力対応  |↑|↓|←|→|Select|Start|B|A|
|:--------:|-|-|-|-|------|-----|-|-|
|キーボード|↑|↓|←|→|Space|Enter|S|A|
|ゲームパッド（Xbox 配置表記）|↑|↓|←|→|Back|Start|A|B|

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

- [F# for Fun and Profit eBook 日本語訳](https://matarillo.github.io/fsharp_for_fun_and_profit-ja/index.html)
- [Writing NES Emulator in Rust](https://bugzmanov.github.io/nes_ebook/)
- [プラスウイングTV: ファミコンエミュレータを作る (Make a NES Emulator in Rust)](https://www.youtube.com/watch?v=B-0bw4q6Pxo&list=PLp_EUEO9JJP1cMwbqzOHFOI9gPH_zoO0U)
- [NES on FPGA](https://pgate1.at-ninja.jp/NES_on_FPGA/index.html)
- [NESDev](https://www.nesdev.org/)
- [GitHub: Famicom-Fsharp - instructions.fs](https://github.com/kxkx5150/Famicom-Fsharp/blob/main/src/emulator/instructions.fs)
- ChatGPT
