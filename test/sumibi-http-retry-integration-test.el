;;; sumibi-http-retry-integration-test.el --- Integration tests for HTTP retry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kiyoka Nishiyama

;; Author: Kiyoka Nishiyama <kiyoka@sumibi.org>
;; Keywords: japanese, ime

;;; Commentary:
;; `sumibi-http-retry-test.el' (単体テスト) が `url-retrieve-synchronously'
;; をスタブ化してリトライ「ロジック」を検証するのに対し、本ファイルは
;; 実際のループバック HTTP サーバーを起動し、`url-retrieve-synchronously'
;; を一切スタブ化せずに「接続が切れる → リトライ → 回復」という Issue #150
;; の実シナリオを end-to-end で検証します。
;;
;; テスト用サーバーは「最初の N 本の TCP 接続は応答せず即切断し、その後の
;; 接続には HTTP 200 を返す」という挙動で、keep-alive セッション終了直後の
;; 古い接続使い回しによる接続失敗を再現します。
;;
;; 実機検証で判明した重要な事実:
;;   - url.el (url-http) 自体が接続失敗時に内部で1回再接続する。そのため
;;     「1本だけ切れた」程度なら Sumibi 側リトライ無しでも url.el が回復する。
;;   - Sumibi の `sumibi-api-max-retries' は url.el の内部再接続でも回復
;;     できないケース (複数連続失敗) を救う第2の安全網として働く。
;;   - 接続失敗は多くの場合 nil 返却ではなく `file-error' の signal として
;;     現れ、`sumibi--http-retrieve-synchronously' の condition-case が捕捉する。
;;
;; これらのテストは url.el の内部再接続回数に依存しないよう、接続本数の厳密値
;; ではなく「最終的に 200 で回復したか / 全滅時に 504 を例外なく返すか」という
;; ユーザーから見える結果を検証します。

;;; Code:

(require 'ert)
(require 'cl-lib)

;; popup / unicode-escape / deferred は batch -Q の load-path に無いことが
;; あるため、単体テストと同様に最小スタブを用意してから sumibi を読み込む。
(unless (require 'popup nil 'noerror)
  (defvar popup-version "0-stub")
  (defun popup-menu* (&rest _args) (error "popup stub: not implemented"))
  (provide 'popup))

(unless (require 'unicode-escape nil 'noerror)
  (defun unicode-escape-to-string (s) s)
  (provide 'unicode-escape))

(unless (require 'deferred nil 'noerror)
  (defun deferred:succeed (&rest _args) nil)
  (defun deferred:nextc (&rest _args) nil)
  (defun deferred:error (&rest _args) nil)
  (provide 'deferred))

(require 'sumibi)
(require 'url)

;; ------------------------------------------------------------------
;; テスト用ローカル HTTP サーバー
;; ------------------------------------------------------------------

(defconst sumibi-itest--response-body
  "{\"choices\":[{\"message\":{\"content\":\"\\u3053\\u3093\\u306b\\u3061\\u306f\"}}]}"
  "テストサーバーが HTTP 200 で返す JSON ボディ。")

(defun sumibi-itest--start-server (drop-first-n)
  "ループバック HTTP サーバーを起動する。

最初の DROP-FIRST-N 本の TCP 接続は HTTP 応答を返さず即切断し
(keep-alive セッション終了直後の死んだ接続を再現)、それ以降の接続には
HTTP 200 と `sumibi-itest--response-body' を返す。

戻り値は plist (:server PROC :port PORT :counter CELL)。
CELL は接続本数を記録する1要素リスト (car で参照)。
ネットワークが使えず作成に失敗した場合は `error' を signal する。"
  (let* ((counter (list 0))
         (server
          (make-network-process
           :name "sumibi-itest-httpd"
           :server t
           :host 'local
           :service t                 ; ephemeral port
           :family 'ipv4
           :coding 'binary
           :filter
           (lambda (proc string)
             ;; ヘッダ終端 (\r\n\r\n) までクライアントごとに蓄積する。
             (let ((acc (concat (or (process-get proc :buf) "") string)))
               (process-put proc :buf acc)
               (when (string-match-p "\r\n\r\n" acc)
                 (setcar counter (1+ (car counter)))
                 (if (<= (car counter) drop-first-n)
                     ;; 応答せず切断 = 死んだ接続
                     (delete-process proc)
                   ;; 正常な HTTP 200 応答
                   (process-send-string
                    proc
                    (concat "HTTP/1.1 200 OK\r\n"
                            "Content-Type: application/json\r\n"
                            (format "Content-Length: %d\r\n"
                                    (string-bytes sumibi-itest--response-body))
                            "Connection: close\r\n"
                            "\r\n"
                            sumibi-itest--response-body))
                   (delete-process proc))))))))
    (list :server server
          :port (process-contact server :service)
          :counter counter)))

(defmacro sumibi-itest--with-server (drop-first-n vars &rest body)
  "DROP-FIRST-N 本切断するサーバーを起動して BODY を実行する。
VARS は (URL-VAR COUNTER-VAR) の形式。URL-VAR にテスト対象 URL、
COUNTER-VAR に接続本数セルが束縛される。サーバーは BODY 終了後に必ず停止する。
ネットワークが使えない環境では `ert-skip' でスキップする。"
  (declare (indent 2))
  (let ((url-var (nth 0 vars))
        (counter-var (nth 1 vars))
        (srv (make-symbol "server-info")))
    `(let ((,srv (condition-case err
                     (sumibi-itest--start-server ,drop-first-n)
                   (error
                    (ert-skip (format "ローカルサーバーを起動できません: %s"
                                      (error-message-string err)))))))
       (unwind-protect
           (let ((,url-var (format "http://127.0.0.1:%d/v1/chat/completions"
                                   (plist-get ,srv :port)))
                 (,counter-var (plist-get ,srv :counter)))
             ,@body)
         (let ((p (plist-get ,srv :server)))
           (when (process-live-p p) (delete-process p)))))))

;; ------------------------------------------------------------------
;; 1. 死んだ接続1本から回復して 200 を返す (Issue #150 の実シナリオ)
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-integration-recovers-from-dropped-connection ()
  "最初の接続が応答なしで切断されても、最終的に HTTP 200 を返す。
keep-alive セッション終了直後に古い接続を使い回して失敗するケースに相当。"
  (let ((sumibi-api-max-retries 1)
        (sumibi-api-timeout 5))
    (sumibi-itest--with-server 1 (url counter)
      (let ((result (sumibi--http-retrieve-synchronously url)))
        (should (equal (car result) "200"))
        (should (string= (cdr result) sumibi-itest--response-body))
        ;; 少なくとも1本は切断されているので、再接続が発生している。
        (should (>= (car counter) 2))))))

;; ------------------------------------------------------------------
;; 2. 連続失敗を Sumibi 側リトライで押し切って回復する
;;    (url.el の内部再接続だけでは足りないケース)
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-integration-recovers-after-multiple-failures ()
  "最初の3本の接続が切断されても、`sumibi-api-max-retries' により
最終的に HTTP 200 を返す。url.el の内部再接続 (約1回) だけでは4本目に
到達できないため、Sumibi のリトライループが複数回 url-retrieve を呼んだ
ことの証拠として、接続本数が4本以上になることも確認する。"
  (let ((sumibi-api-max-retries 5)
        (sumibi-api-timeout 5))
    (sumibi-itest--with-server 3 (url counter)
      (let ((result (sumibi--http-retrieve-synchronously url)))
        (should (equal (car result) "200"))
        (should (string= (cdr result) sumibi-itest--response-body))
        (should (>= (car counter) 4))))))

;; ------------------------------------------------------------------
;; 3. すべての接続が失敗しても、例外を投げず 504 を返す
;;    (url.el が signal する実物の file-error を condition-case が捕捉する)
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-integration-all-fail-returns-504 ()
  "サーバーが常に切断する場合、`url-retrieve-synchronously' が
実際に file-error を signal するが、それを捕捉して例外を投げずに
504 TIMEOUT ERROR を返す (変換コマンド全体が中断しないことを保証)。"
  (let ((sumibi-api-max-retries 1)
        (sumibi-api-timeout 5))
    ;; 事実上すべての接続を切断する
    (sumibi-itest--with-server most-positive-fixnum (url counter)
      ;; ここで result が返ってくること自体が「例外が外に漏れていない」証拠。
      (let ((result (sumibi--http-retrieve-synchronously url)))
        (should (string= (car result) "504"))
        (should (string-match-p "TIMEOUT ERROR" (cdr result)))
        (should (>= (car counter) 1))))))

;; ------------------------------------------------------------------
;; 4. リトライ無効 (max-retries=0) で全切断なら 504 (回復しない)
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-integration-no-retry-all-fail-returns-504 ()
  "`sumibi-api-max-retries' が 0 でサーバーが常に切断する場合、
url-retrieve は1回だけ呼ばれ、回復せずに 504 を返す。"
  (let ((sumibi-api-max-retries 0)
        (sumibi-api-timeout 5))
    (sumibi-itest--with-server most-positive-fixnum (url counter)
      (let ((result (sumibi--http-retrieve-synchronously url)))
        (should (string= (car result) "504"))
        (should (>= (car counter) 1))))))

(provide 'sumibi-http-retry-integration-test)
;;; sumibi-http-retry-integration-test.el ends here
