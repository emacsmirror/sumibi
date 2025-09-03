# Sumibi お試しキャンペーン開始！

🎉 **AI搭載のEmacs日本語入力メソッド「Sumibi」を無料でお試しいただけます！** 🎉

## 🚀 キャンペーン内容

**Twitterで @kiyoka までご連絡いただくだけで、Sumibiを7日間完全無料でお試しいただけるLLMアクセスキーをプレゼント！**
**お気軽にご連絡ください。**

✨ **特典**
- **7日間無料** でSumibiの全機能をご利用いただけます
- **変換回数制限なし** で思う存分お試しください
- モードレス入力の革新的な体験をご堪能ください

## ⚠️ ご利用上の注意

- 提供するキーはSumibi専用です（他のAPI呼び出しにはご使用いただけません）
- 7日間の期限終了後は自動的にご利用できなくなります

## 🎯 こんな方におすすめ

- **Emacsユーザーの方** - 普段からEmacsをお使いの方に最適です
- **新しい入力体験をお求めの方** - Sumibiのモードレス入力方式に興味をお持ちの方
- **AI技術に興味のある方** - 最新のAI技術を活用した日本語入力を体験したい方

https://github.com/user-attachments/assets/0e66d428-a35e-4920-a816-2bb0c6cc99c9

## 📝 お申し込み方法

Twitterでお気軽にご連絡ください！すぐにお試しキーをお送りいたします。

## Sumibiの設定方法

1. MELPAからパッケージ「sumibi」をインストールします。

2. \~/.emacs.d/init.el に以下のコードを続き、Emacsを再起動します。

(xxxxxxxxxx の部分に、プレゼントされたLLMアクセスキーを入れてください)

```lisp
;; Sumibi trial
(setenv "SUMIBI_AI_API_KEY" "xxxxxxxxxx")
(setenv "SUMIBI_AI_BASEURL" "https://ai-endpoint.sumibi.org/")
(setenv "SUMIBI_AI_MODEL" "claude-sonnet-4-20250514")
(require 'sumibi)
(global-sumibi-mode 1)
```

3. 設定が完了したら、Emacsのmode lineが次のようになります。

![modeline](/images/sumibi-trial-modeline.png)

補足: SUMIBI_AI_MODELには、"gpt-oss-120b" と "claude-sonnet-4-20250514"のどちらかを指定できます。IMEの変換精度の比較ができます。

## 操作方法

詳しい操作方法は [README](README.md)をご覧ください。

---
*この機会をお見逃しなく！Sumibiで次世代の日本語入力を体験してください。*
