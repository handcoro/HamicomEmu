# famicom-emulator-fs
のんびり作られるファミコンエミュレータです。

## 開発めも
関数型言語習得を何回も挫折してた初心者が、F# とゲーム機内部仕様の勉強をしながらファミコンエミュレータをつくっていくリポジトリとなっております。ゴールは決めてません。

関数型に慣れるためになるべくクラスは使わない方針で行きます。

とりあえずの画面表示やのために MonoGame を使用してます。

---

## 最近の進捗・現状メモ（2025 年 6 月末）

- 副作用は要所で利用する形で関数型設計を大事に進行中
- CPU 命令（公式・非公式）ほぼ実装済み
- DMC によるCPUストールを実装（再現性は大きく向上。ただし実行速度はやや犠牲に）
- PPU（画面描画）はアドレスレジスタ・スクロールレジスタを統合し、より実機寄りに進化  
  VBlank/NMIや奇数フレームスキップのタイミングも有効化し、細かな挙動の再現精度がアップしたはず…
  ただし一部処理のパフォーマンスは今後さらに改善予定
- APU（サウンド）は矩形波 2ch・三角波・ノイズ・DMCの合成に加え、DMC・APU のtickも 1 サイクル単位で正確化
- 入力対応も拡張し、ゲームパッドに対応
- 内部状態・tick/step分離など、設計面でも引き続きリファクタや最適化を進行中

---

### もうちょっと詳しい開発の現状

- スクロールまわりのしっかりとした実装を継続中です
- 実機に近いタイミング再現はてごわい今後の課題です
- コードの見通し・保守性も意識して、細かいリファクタやファイル整理をちょこちょこやっています

---

### TODO

- スクロールも含めて画面表示の正確さの向上
- PPU のパフォーマンス最適化
- tickの高速化と正確性の両立
- サウンドの再現度向上
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
- [NESDev: 6502 Instruction Reference](https://www.nesdev.org/obelisk-6502-guide/reference.html)
- [GitHub: Famicom-Fsharp - instructions.fs](https://github.com/kxkx5150/Famicom-Fsharp/blob/main/src/emulator/instructions.fs)
- etc.