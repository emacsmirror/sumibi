# LLMが使えない環境でのMozcの利用

OpenAIなどのリモートAPIに接続できない環境でもSumibiを使えるようにMozcをバックエンドの変換エンジンとして利用できるようにしました。Sumibi Ver3.2.0で細かい問題は解消できました。Emacsでmozc.elを使っている皆さんは、一度Sumibiも試していただければ幸いです。
Sumibiの最大の特徴は、従来のIMEのような「日本語入力モード」と「英語入力モード」の切り替えが不要な点です。このモードレスなインターフェースデザインにより、他のIMEには無い快適さを実現しています
利用するには、mozc_serverとmozcのヘルパーアプリをインストールしておく必要があります。

## 動作確認済みOS

- Ubuntu 24.x

## セットアップ手順

mozc関連のパッケージのインストールを行います。

```
sudo apt update
sudo apt upgrade
sudo apt install mozc-server emacs-mozc emacs-mozc-bin
```

## Emacsの設定

1. カスタマイズ変数 `sumibi-backend` を `mozc`に変更してください。

この設定によりSumibiからOpenAIやGoogleに接続することはなくなり、mozc_emacs_helperを経由してmozc_serverに接続します。
※ mozc_serverは必要になった時点で自動起動するため、サーバーの起動設定は不要です。

2. まだ設定していない場合は、`~/.emacs.d/init.el`にSumibiの基本設定を追記してください。

```lisp
(require 'sumibi)
(global-sumibi-mode 1)
```

## トラブルシューティング

- Emacsを起動しモードラインが以下のようになれば成功です。

```
   Sumibi[mozc_server|mozc]
```

- Ctrl+Jを押したとき、以下のエラーが表示された場合は失敗です。

mozc_emacs_helperがインストールされていないか、起動に失敗しています。

```
mozc.el: Failed to start mozc-helper-process.
```

## 変換候補の一位の決まり方

Sumibiでは、Mozcの変換候補順位を安定化する機能を搭載しています。

### 問題の背景

Mozcは多くの要因（入力コンテキスト、統計データ、学習状況など）によって変換候補の順位を動的に変更します。これにより、同じローマ字入力でも毎回異なる候補が第一位に表示されることがあり、一貫性のない変換体験となる場合があります。

### Sumibiの解決方法

1. **履歴ベースの候補安定化**
   - 過去に確定した変換候補をSumibiが記録
   - 同じローマ字入力に対して、以前選択した候補を優先的に第一位に表示
   - Emacsセッション中は、この優先順位が維持される

2. **動作例**
   ```
   初回: "henkan" → Mozc候補: ["変感", "変換", "返還"] → ユーザーが "変換" を選択
   次回: "henkan" → Mozc候補: ["変感", "返還", "変換"] → Sumibiが自動調整 → ["変換", "変感", "返還"]
   ```

### 技術的な仕組み

- Mozcの変換セッション管理には一切干渉しない
- 変換結果の候補リストのみを並び替える安全な実装
- `sumibi-history-stack`に保存された履歴データを活用
- セッション終了時に履歴はリセットされる

この機能により、Mozcの学習機能と共存しながら、より予測可能で一貫した変換体験を提供します。

## mozcの個人設定について

MozcはUIツールで設定変更できます。
変換結果が意図通りでないと感じたら、`Clear all history`などを実行して履歴をリセットしてください。

```
/usr/lib/mozc/mozc_tool --mode=config_dialog
```
