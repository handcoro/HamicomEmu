# HamicomEmu

F# でのんびり作られるファミコンエミュレータです。

## 開発めも

関数型言語習得を何回も挫折してた初心者が、F# とゲーム機内部仕様の勉強をしながらファミコンエミュレータをつくっていくリポジトリとなっております。ゴールは決めてません。なにかユニークな機能がつけられればいいですね。

関数型に慣れるためにライブラリ使用時以外はクラスは使わない方針で行きます。

---

## 最近の進捗・現状メモ（2026 年 2 月）

- OAM DMA (スプライトの直接転送) 中に CPU だけを止めるようにした
- Namco 163 を音声出力機能を除いて実装
- 2コン入力にマイクを除いて対応

---

### もうちょっと詳しい開発の現状

- 副作用はできるだけコンパクトに封じ込めたい
- スクロールまわりのしっかりとした実装を継続中です
- コードの見通し・保守性も意識して、細かいリファクタやファイル整理をちょこちょこやっています
- フレームワークを .NET 10 SDK にアップグレード
- テストプロジェクトを分離しました

---

### TODO

- GUI の整備
- スプライト描画の正確化
- PPU のパフォーマンス最適化
- tick の高速化と正確性の両立
- サウンドの再現度向上
- サウンドのフィルタリング方法の模索
- 非公式命令は必要に応じて実装を検討していきます
- テスト ROM で互換性を確認しつつ、ちょっとずつ修正
- 気になったところから修正や実装をやっていきたいですね

---

### 実行オプション

オプションなし:

`nes/Alter_Ego.nes` を実行します

`ファイルパス`:

指定された NES ファイルを実行します

|           入力対応            | ↑ | ↓ | ← | → | Select | Start | B | A |
|:-----------------------------:|---|---|---|---|--------|-------|---|---|
|        キーボード(1P)         | ↑ | ↓ | ← | → | Space  | Enter | S | A |
| ゲームパッド（Xbox 配置表記） | ↑ | ↓ | ← | → | Back   | Start | A | B |
|        キーボード(2P)         | Y | H | G | J |        |       | K | L |

|機能操作  |リセット|電源トグル|
|:--------:|--------|----------|
|キーボード|R       |P         |

## ビルド方法

.NET 10 SDK が入っていればコマンドラインから dotnet コマンドでビルドできます。

必要なもの

- .NET 10 SDK
- Windows / Linux / macOS（MonoGame DesktopGL なのでたぶん動くはずです）

### とりあえず実行してみる

```pwsh
dotnet run --project ./src/HamicomEmu/HamicomEmu.fsproj
```

### ビルド

```pwsh
dotnet build --project ./src/HamicomEmu/HamicomEmu.fsproj
```

## 参考

- [F# for Fun and Profit eBook 日本語訳](https://matarillo.github.io/fsharp_for_fun_and_profit-ja/index.html)
- [Writing NES Emulator in Rust](https://bugzmanov.github.io/nes_ebook/)
- [プラスウイングTV: ファミコンエミュレータを作る (Make a NES Emulator in Rust)](https://www.youtube.com/watch?v=B-0bw4q6Pxo&list=PLp_EUEO9JJP1cMwbqzOHFOI9gPH_zoO0U)
- [NES on FPGA](https://pgate1.at-ninja.jp/NES_on_FPGA/index.html)
- [NESDev](https://www.nesdev.org/)
- [GitHub: Famicom-Fsharp - instructions.fs](https://github.com/kxkx5150/Famicom-Fsharp/blob/main/src/emulator/instructions.fs)
- ChatGPT
