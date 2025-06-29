# mozc.elの解析情報

sumibi.elからmozc.elを利用しています。
この資料では、変換候補の確定をmozc_serverにフィードバックする周辺の技術情報を記載しています。

## 目的

sumibi.elではmozcの変換候補の一覧を取得して、sumibi.elで変換候補から確定した結果をmozc.elの関数を使ってmozc_serverに伝える必要があります。
それには、mozc.elの内部構造を把握しておく必要があります。

## 技術詳細

`mozc.el`のコードを読み、`mozc-candidate-dispatch`関数とその周辺のコードを分析しました。

### `mozc-candidate-dispatch`の解説

`mozc-candidate-dispatch`は、変換候補ウィンドウの表示方法（スタイル）に応じて、実際の処理を行う関数を呼び出すためのディスパッチャ（振り分け役）です。

```lisp
(defun mozc-candidate-dispatch (method &rest args)
  "`mozc-candidate-style' に従ってメソッド呼び出しをディスパッチする。
METHODは `clean-up'、`clear'、`update' のいずれかのシンボルです。ARGSについては、
それぞれ `mozc-candidate-clean-up'、`mozc-candidate-clear'、`mozc-candidate-update' を参照してください。"
  (let* ((style (if (minibufferp)
                    'echo-area
                  mozc-candidate-style))
         (method-table (cdr (assq style
                                  mozc-candidate-dispatch-table)))
         (func (cdr (assq method method-table))))
    (if func
        (apply func args)
      (signal 'mozc-internal-error (list mozc-candidate-style method args)))))
```

この関数のポイントは以下の通りです。

1.  **`mozc-candidate-style`変数**: ユーザーは`mozc-candidate-style`という変数を設定することで、候補ウィンドウの表示方法を選択できます。デフォルトでは`overlay`（カーソル近くにポップアップ表示）と`echo-area`（画面下部のエコーエリアに表示）が用意されています。
2.  **`mozc-candidate-dispatch-table`**: この変数は、表示スタイルと、それに対応する関数のセットを関連付けるテーブルです。

    ```lisp
    (defvar mozc-candidate-dispatch-table
      '((overlay (clean-up . mozc-cand-overlay-clean-up)
                 (clear . mozc-cand-overlay-clear)
                 (update . mozc-cand-overlay-update))
        (echo-area (clean-up . mozc-cand-echo-area-clean-up)
                   (clear . mozc-cand-echo-area-clear)
                   (update . mozc-cand-echo-area-update)))
      "...")
    ```

    例えば、`mozc-candidate-style`が`overlay`の場合、`update`メソッドとして`mozc-cand-overlay-update`が呼び出されます。
3.  **メソッド**: `mozc-candidate-dispatch`は3つのメソッド（`clean-up`, `clear`, `update`）を受け付けます。
    *   `clean-up`: 候補表示セッションを完全に終了させます。
    *   `clear`: 候補表示を一時的に消去します。
    *   `update`: `mozc_server`から受け取った候補情報（`candidates`）を使って表示を更新します。

### 候補セッションについて

`mozc.el`における「候補セッション」とは、変換候補ウィンドウが表示されている状態を指します。これは`mozc-mode`が有効で、かつユーザーが文字を入力して変換が発生したときに開始されます。

*   **開始**: `mozc-handle-event`関数が`mozc_server`から候補情報（`candidates`）を含む応答を受け取ると、`(mozc-candidate-update candidates)`を呼び出し、候補セッションが開始（または更新）されます。
*   **更新**: 変換候補のページ送りや、別の候補を選択するなどの操作を行うと、`mozc-handle-event`は新しい候補情報を受け取り、再度`mozc-candidate-update`を呼び出して表示を更新します。
*   **終了**: 変換を確定（`Enter`キーなど）したり、入力状態をキャンセル（`Esc`キーなど）したりすると、`mozc_server`は候補情報を含まない応答を返します。その結果、`mozc-candidate-clear`や`mozc-candidate-clean-up`が呼び出され、候補ウィンドウが消去されてセッションが終了します。

### 外部LispからMozcの変換候補を操作する方法

ご要望の「別のEmacs Lispからmozcの変換候補の一覧を取得し、別のEmacs Lispで変換候補から確定した結果をmozc.elの関数を使ってmozc_serverに伝える」を実現するための手順は以下のようになります。

#### 1. 変換候補一覧の取得

`mozc.el`は、`mozc_server`から受け取った候補情報を`mozc-candidate-update`関数に渡して表示を更新します。この候補情報は`candidates`という引数で渡されるalist（連想リスト）です。

この`candidates`変数の内容を外部から取得するのが目的ですが、`mozc.el`はこれを内部で処理してしまい、外部から直接参照するためのAPIを提供していません。

そこで、**`mozc-candidate-update`関数を`advice-add`を使ってフックする**のが現実的な方法です。

```lisp
(defvar my-mozc-candidates nil
  "mozc.elから取得した候補情報を格納する変数")

(defun my-mozc-capture-candidates (candidates)
  "mozc-candidate-updateに渡された候補情報をキャプチャする"
  (setq my-mozc-candidates candidates))

;; mozc-candidate-updateが呼び出される「前」に、その引数をキャプチャする
(advice-add 'mozc-candidate-update :before #'my-mozc-capture-candidates)
```

これで、Mozcで変換を行うたびに、`my-mozc-candidates`変数に最新の候補情報が格納されるようになります。

候補情報`my-mozc-candidates`の構造は、`mozc.el`内の`mozc-protobuf-get`関数の使われ方から推測できます。例えば、`mozc-cand-echo-area-make-contents`関数を見ると、以下のようにして情報を取得しています。

*   ` (mozc-protobuf-get candidates 'candidate)`: 候補のリストを取得
*   ` (mozc-protobuf-get candidate 'value)`: 候補の文字列
*   ` (mozc-protobuf-get candidate 'index)`: 候補のインデックス
*   ` (mozc-protobuf-get candidate 'annotation 'description)`: 候補の注釈

#### 2. 候補を確定し、結果を`mozc_server`に伝える

`mozc.el`では、ユーザーのキー入力を`mozc-send-key-event`関数で`mozc_server`に送信しています。特定の候補を選択して確定する操作も、キーイベントとして送信されます。

例えば、候補リストの3番目を選択したい場合、通常は数字の`3`キーを押します。この操作をエミュレートするには、`mozc-send-key-event`を呼び出します。

```lisp
(defun my-mozc-select-candidate-by-index (index)
  "インデックスを指定してMozcの候補を確定する"
  (if (and mozc-mode (>= index 0))
      (let* ((shortcut-char (+ (string-to-char "1") index)) ; '1'の文字コード + index
             (output (mozc-send-key-event shortcut-char)))
        ;; mozc-handle-eventと同様の処理を行う
        (when output
          (let ((result (mozc-protobuf-get output 'result))
                (preedit (mozc-protobuf-get output 'preedit))
                (candidates (mozc-protobuf-get output 'candidate-window)))
            (if (not (or result preedit))
                (mozc-clean-up-changes-on-buffer)
              (when result
                (mozc-clean-up-changes-on-buffer)
                (insert (mozc-protobuf-get result 'value)))
              (if preedit
                  (mozc-preedit-update preedit candidates)
                (mozc-preedit-clear))
              (if candidates
                  (mozc-candidate-update candidates)
                (mozc-candidate-clear))))))
    (message "Mozc is not active or index is invalid.")))

;; 例：2番目の候補を選択（インデックスは0から始まるので1）
;; (my-mozc-select-candidate-by-index 1)
```

**解説:**

*   `mozc-send-key-event`はキーイベント（文字コードやシンボル）を引数に取ります。候補選択のショートカットキー（'1', '2', '3' ...）は文字コードで表現できます。
*   `mozc-send-key-event`は`mozc_server`からの応答（`output`）を返します。この`output`には確定された文字列（`result`）や、その後のプリエディット（`preedit`）、残りの候補（`candidates`）などが含まれています。
*   `mozc-handle-event`の処理を参考に、`output`の内容に応じてバッファの更新（確定文字列の挿入、プリエディットの更新、候補ウィンドウの更新/消去）を行う必要があります。上記の`my-mozc-select-candidate-by-index`は、その処理を模倣したものです。

### まとめ

*   **`mozc-candidate-dispatch`**: 候補ウィンドウの表示スタイルに応じて処理を振り分ける関数。
*   **候補セッション**: 変換候補が表示されている状態。
*   **候補一覧の取得**: `mozc-candidate-update`を`advice-add`でフックし、引数として渡される候補情報をキャプチャする。
*   **候補の確定**: `mozc-send-key-event`を使って、選択したい候補のショートカットキー（'1'など��を`mozc_server`に送信し、返ってきた結果を元にバッファを更新する。

この方法で、外部のLispからMozcの変換プロセスに介入し、独自の候補選択UIなどを実装することが可能になります。


