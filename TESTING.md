# TESTING.md

このドキュメントは、Sumibiプロジェクトのユニットテストの実行方法とガイドラインを説明します。

## テストの実行方法

### 基本的なテスト実行（モック版、推奨）
```bash
# モック版でテスト実行（デフォルト、100%再現性）
make test
```

### 実際のMozc環境でのテスト
```bash
# 本物のmozc_serverでテスト実行
SUMIBI_TEST_USE_MOCK=0 make test
```

### 個別テスト実行
```bash
# 特定のテストのみ実行
emacs -batch -Q \
  -L lisp \
  -L test \
  -l test/sumibi-mozc-tests.el \
  --eval "(ert-run-tests-interactively \"sumibi-mozc-henkan\")"
```

## テスト環境の種類

### 1. モック環境（デフォルト、推奨）
- **条件**: デフォルト動作（`make test`）
- **特徴**: 
  - 100%再現性のある結果
  - mozc_serverへの接続なし
  - 高速実行
  - CI環境対応
- **用途**: 継続的インテグレーション、開発中のテスト

### 2. 実際のMozc環境
- **条件**: 環境変数 `SUMIBI_TEST_USE_MOCK=0` を設定
- **特徴**: 実際のmozc_serverと通信
- **用途**: 実際の環境での動作確認

### 3. Mozcなし環境
- **条件**: mozc.elがインストールされていない
- **特徴**: 関連テストがスキップされる
- **用途**: Mozcに依存しない機能のテスト

## テストの構成

### テストファイル
- `test/sumibi-mozc-tests.el` - Mozcバックエンドのテスト
- `test/mozc-mock.el` - Mozcのモック実装

### テストカテゴリ
1. **基本変換テスト** - ローマ字から日本語への変換
2. **エンドツーエンドテスト** - `*scratch*`バッファでの実際の変換
3. **候補安定化テスト** - 履歴に基づく候補順序調整
4. **モック専用テスト** - モック機能の動作確認

## テストガイドライン

### 新しいテストの追加

#### 1. 基本的なテストの追加
```elisp
(ert-deftest sumibi-mozc-your-test ()
  "Test description."
  (sumibi-test-with-mozc
   (let ((result (car (sumibi-roman-to-kanji-with-surrounding "input" "" 1 nil))))
     (should (string= result "期待値")))))
```

#### 2. モック専用テストの追加
```elisp
(ert-deftest sumibi-mozc-mock-your-test ()
  "Test description for mock-only functionality."
  (skip-unless sumibi-test-use-mozc-mock)
  (sumibi-test-with-mozc
   ;; テストコード
   ))
```

### テストデータの追加

モックの変換テーブルに新しいデータを追加する場合：

```elisp
;; test/mozc-mock.el の mozc-mock--conversion-table に追加
("newromaji" . ("新しい" "あたらしい" "ニューロマジ"))
```

### テストの命名規則

- **プレフィックス**: `sumibi-mozc-`
- **機能別**: `sumibi-mozc-henkan`, `sumibi-mozc-nihongo`
- **環境別**: `sumibi-mozc-scratch-*` (エンドツーエンドテスト)
- **モック専用**: `sumibi-mozc-mock-*`

## トラブルシューティング

### よくある問題と解決方法

#### 1. 括弧の不整合エラー
```bash
# 修正後は必ず括弧チェックを実行
agent-lisp-paren-aid-linux test/sumibi-mozc-tests.el
```

#### 2. モックが動作しない
```bash
# デフォルトでモック版が動作するはず
make test

# 実際のMozcを使用したい場合
export SUMIBI_TEST_USE_MOCK=0
make test
```

#### 3. テストが予期しない結果になる
- モック版と実際のMozc版で異なる結果になる場合がある
- モック版では固定の変換テーブルを使用
- 実際のMozc版では学習機能や辞書の状態に依存

#### 4. 依存関係の問題
```bash
# 必要なパッケージがインストールされているか確認
emacs -batch -Q \
  --eval "(progn (require 'package) (package-initialize) (require 'dash))"
```

## パフォーマンステスト

### 実行時間の測定
```bash
# 時間測定付きでテスト実行
time make test                    # モック版（デフォルト）
time SUMIBI_TEST_USE_MOCK=0 make test  # 実際のMozc版
```

### ベンチマーク比較
- **モック版（デフォルト）**: 平均 0.5-1秒
- **実際のMozc**: 平均 2-3秒
- **スキップ版**: 平均 0.1-0.2秒

## CI/CD での使用

### GitHub Actions での例
```yaml
- name: Run tests with mock (default)
  run: make test

- name: Run tests with real mozc (optional)
  run: |
    export SUMIBI_TEST_USE_MOCK=0
    make test
```

### 推奨事項
1. CI環境ではデフォルトのモック版を使用
2. 定期的に実際のMozc版でもテスト実行
3. テスト結果の再現性を重視

## テストの拡張

### 新機能のテスト追加時
1. まずモック版でテストを作成
2. 必要に応じて実際のMozc版でも検証
3. テストデータをモックテーブルに追加
4. ドキュメントを更新

### テストカバレッジの改善
- `sumibi-roman-to-kanji-with-surrounding`の全パターン
- エラーハンドリングの検証
- 学習機能の動作確認
- 設定変更時の動作確認

---

**注意**: テストファイルを編集した後は、必ず `agent-lisp-paren-aid-linux` で括弧の整合性を確認してください。