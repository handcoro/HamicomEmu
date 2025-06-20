# famicom-emulator-fs
のんびり作られるファミコンエミュレータです。

## 開発めも
関数型言語習得を何回も挫折してた初心者が、F# とゲーム機内部仕様の勉強をしながらファミコンエミュレータをつくっていくリポジトリとなっております。ゴールは決めてません。

関数型に慣れるためになるべくクラスは使わない方針で行きます。

とりあえずの画面表示のために MonoGame を使用してます。

---

## 最近の進捗・現状メモ（2025 年 6 月）

- 副作用をなるべく排した関数型設計を大事に進行中
- CPU 命令（公式・非公式）ほぼ実装。
- **DMC による CPU ストールはまだ未実装**。`Cpu.step` と `Bus.tick` の分離作業が終わったあとで検討予定
- PPU（画面描画）は 8x8 スプライトや限定的なスクロールまで動作 
  ただし VBlank/NMI タイミングや奇数フレームスキップの精密さは今後の課題
- APU（サウンド）は矩形波 2ch・三角波・ノイズ・DMC の合成まで進む
- 内部の状態と tick/step の進行制御を分離し、ピュアな関数で流れを管理
- コア部分（src/Core）とプラットフォーム依存部分（src/Platform）で整理中

---

### もうちょっと詳しい開発の現状

- バスやPPU・APUも、1 サイクルごと進めるtick関数の設計を中心に組み立て中
- スクロールや内部レジスタ（v, t, x, w）まわりはこれからじっくり着手
- 実機に近いタイミング再現はてごわい今後の課題です
- コードの見通し・保守性も意識して、細かいリファクタやファイル整理をちょこちょこやっています

---

### TODO

- **ストール（DMC 等による CPU 一時停止）の実装**
- PPU の VBlank/NMI や奇数フレームスキップの正確な再現
- スクロール内部レジスタの実装
- tickの高速化と正確性の両立
- サウンド（APU/DMC）のバランス調整やタイミング精度向上
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