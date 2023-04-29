# SumibiGPT

Japanese input method using OpenAI GPT

## SumibiGPTとは

Emacs用の日本語入力システム(IME)です。

SumibiGPTはモードレスです。
日本語入力モードに切り替えることなく日本語を入力できます。

## 利用可能なEmacsバージョン

Emacs version 28.x (Windows/Linux/macOS) で動作します。Emacs以外の追加ソフトウェアは不要です。

## Emacsクライアントのインストール

1. OpenAI AIのサブスクリプションを契約します。

[https://platform.openai.com/account/api-keys](https://platform.openai.com/account/api-keys)
![image.png](./images/img_8.png)

2. 環境変数 OPENAI\_API\_KEY にOpenAPIのAPIキーを登録します。
3. melpa から以下のパッケージをインストールします。

```
popup 0.5.9
unicode-escapeo 20230109.1222
```

4. sumibigpt.el を \~/.emacs.d/ に保存します。
5. \~/.emacs.d/init.el に以下のコードを追加します。

```lisp
(require 'sumibigpt)
(global-sumibigpt-mode 1)
```

## インストールが成功したかどうかの確認方法

Emacsを再起動するとSumibiGPTがステータスバーに表示されます。
[gpt-3.5-turbo] はOpenAI API 呼び出しで使用しているGPTのモデルです。
![image.png](./images/img_9.png)

## 変換方法

1. ローマ字で書いた文章の最後にカーソルを合わせて、Ctrl-J を入力すると日本語の文章に置き換わります。
    ![image.png](./images/img_15.png)
    ![image.png](./images/img_16.png)
2. 変換結果が気に入らない場合は、そのまま Ctrl-J を入力すると変換候補のポップアップが表示されるので、その中から選択できます。
    ![image.png](./images/img_11.png)

## Undo

変換結果が気に入らない時は、ESC-u キーを入力することでUndoできます。

または、変換結果に原文ままの選択肢がありますので「原文まま」を選択します。
![image.png](./images/img_10.png)

## 英語から日本語への変換

英語の文章の最後にカーソルを合わせて、Ctrl-J を入力すると日本語の文章に翻訳されます。
![image.png](./images/img_13.png)
![image.png](./images/img_14.png)

# History

### Ver 1.0.0

- ファーストリリース

### Ver 1.1.0

- region指定した状態でCtrl-J文字入力すると読み(ひらがな・カタカナ)と同音異議語を成成できるようにした。

- OpenAI API呼び出しのタイムアウトエラーを実装した。

- 入力した文章にダブルクオーテーションや改行が入っていると変換エラーとなるバグを修正した。
