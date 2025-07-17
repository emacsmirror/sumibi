# Sumibi Development Plan - markersがnilの履歴によるローマ字変換停止問題の調査

## 問題の概要
ユーザーから報告された問題：「historyをファイルから読み込んで、markersがnil のものにぶつかるとローマ字を変換しなくなる不具合がはいったようです。」

## 実装済みの機能（GitHub issue #53）
✅ 履歴保存時にmarkersをnilに設定する機能
✅ 履歴読み込み時にmarkersがnilの場合の処理
✅ markerがnilの場合の比較処理を無効化
✅ 関連するテストの更新
✅ 括弧バランスチェック
✅ テストファイルの修正
✅ 全23テストが通過

## 問題の根本原因（特定済み）
`sumibi-history-search`関数で、markersがnilの履歴エントリを見つけた際に、
`load-flag`がtrueの場合、line 1878-1880で`move-marker`をnilに対して呼び出そうとしてエラーが発生する。

### 具体的な問題箇所
1. ユーザーがファイルから履歴を読み込む（markersはnilに設定される）
2. 日本語テキストの中でキー入力すると`sumibi-rK-trans`が呼ばれる
3. `sumibi-history-search`がload-flag=tで呼ばれる（line 1979）
4. nilのmarkersに対して`move-marker`を呼ぼうとしてエラー（line 1879）

### 調査済みの箇所
1. **sumibi-history-search関数** (lisp/sumibi.el:1846)
   - `unless (null markers)` でnilマーカーをスキップ済み
   - line 1860-1894の処理は安全

2. **sumibi-history-gc関数** (lisp/sumibi.el:1805)  
   - line 1814-1816でnilマーカーの適切な処理を追加済み
   - GCは正常動作

3. **基本的なテストシナリオ**
   - nilマーカーのみの履歴：正常動作
   - 混合マーカー（nilと有効）：正常動作
   - ファイル読み込みシミュレーション：正常動作

### 次の調査手順
1. **実際のinteractiveユースケースの再現**
   - `sumibi-rK-trans` (メインエントリーポイント) でのテスト
   - 実際のキー入力シミュレーション

2. **エラー発生箇所の特定**
   - exception catchingを含む詳細テスト
   - デバッグログの詳細分析

3. **疑われる相互作用**
   - 履歴読み込み後の新規変換との相互作用
   - `sumibi-history-search` が呼ばれる文脈での問題
   - line 1979: `(when (sumibi-history-search (point) t)` の処理

4. **調査すべき関数**
   - `sumibi-rK-trans`: メインエントリーポイント
   - history searchが呼ばれる全ての文脈
   - マーカー操作を行う他の関数

### 作成済みテストファイル
- `/tmp/test-nil-markers.el`: 基本的なnilマーカーテスト
- `/tmp/test-mixed-markers.el`: 混合マーカーテスト  
- `/tmp/test-file-load.el`: ファイル読み込みテスト
- `/tmp/test-interactive.el`: インタラクティブ使用テスト（未完了）

## 修正方針
現在のコードは、markersがnilの場合でもload処理を実行しようとするため、
`move-marker`の呼び出しでエラーになる。修正方針は以下の通り：

### 修正案1（実装予定）
line 1878-1880の部分で、markersがnilでない場合のみ`move-marker`を呼び出すように修正：

```elisp
(when load-flag
  (when (not (null markers))  ; nilチェックを追加
    (setq sumibi-markers (cons
                         (move-marker (car markers) start)
                         (cdr markers))))
  ; 他の変数は通常通り設定
  ...)
```

ただし、この修正では`sumibi-history-search`がtを返すが、`sumibi-markers`は更新されないため、
`sumibi-rK-trans`側で古いmarkers値を使う可能性がある。

### 修正案2（より安全）
markersがnilの履歴エントリは候補選択モードに入らないようにする。
つまり、line 1860の条件を満たさない場合は、そもそも`found`をtrueにしない。

## 現在のステータス
✅ 問題の根本原因を特定済み
✅ 修正実装完了
✅ テストケース作成・実行済み

## 実装した修正内容

### 1. sumibi-history-search関数の修正 (line 1878-1884)
markersがnilまたは無効な場合、`move-marker`を呼び出さないように修正：

```elisp
(when load-flag
  ;; markersがnilでない場合のみmarkersを更新
  (when (and (not (null markers))
             (markerp (car markers))
             (markerp (cdr markers)))
    (setq sumibi-markers (cons
                         (move-marker (car markers) start)
                         (cdr markers))))
  ;; その他の変数は常に更新
  ...)
```

### 2. sumibi-rK-trans関数の修正 (line 1987-1989)
`sumibi-markers`が有効な場合のみ候補選択モードに入るように修正：

```elisp
(when (sumibi-history-search (point) t)
  ;; markersが有効な場合のみ
  (when (and sumibi-markers
             (markerp (car sumibi-markers))
             (markerp (cdr sumibi-markers)))
    (setq sumibi-select-mode t)
    ...))
```

### 3. テストケースの追加
- `/mnt/c/Users/kiyok/OneDrive/ドキュメント/GitHub/Sumibi/test/sumibi-nil-markers-test.el`
- 3つのテストケース全て合格

## 修正の効果
- ファイルから読み込んだ履歴（markersがnil）があってもエラーが発生しない
- nilマーカーの履歴エントリは検索対象から除外される
- 有効なマーカーを持つ履歴エントリは正常に機能する

## 追加修正: テスト環境での履歴ファイル設定

### 4. 履歴ファイルパスの設定可能化
ユーザーリクエストに応じて、テスト時に`/tmp/.sumibi/history.jsonl`を使用するよう修正：

- `sumibi-history-file-path`変数を追加
- `sumibi-ensure-history-directory`、`sumibi-load-history-from-file`、`sumibi-save-history-to-file`を更新
- 全てのテストファイルで`/tmp/.sumibi/history.jsonl`を使用するよう設定

### テストファイルの更新
- `test/sumibi-nil-markers-test.el`: `/tmp/.sumibi/history.jsonl`を使用
- `test/sumibi-mozc-tests.el`: `/tmp/.sumibi/history.jsonl`を使用、既存のhistoryテストを簡素化

## 完了