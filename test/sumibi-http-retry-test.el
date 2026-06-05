;;; sumibi-http-retry-test.el --- Tests for synchronous HTTP retry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kiyoka Nishiyama

;; Author: Kiyoka Nishiyama <kiyoka@sumibi.org>
;; Keywords: japanese, ime

;;; Commentary:
;; このファイルは、同期 HTTP リクエストの keep-alive 無効化とリトライ
;; (`sumibi--http-retrieve-synchronously') の単体テストを提供します。
;;
;; Issue #150: HTTP が成功していても、古い keep-alive 接続の使い回しで
;; `url-retrieve-synchronously' が nil を返し、候補に !!TIMEOUT ERROR!! が
;; 混入することがある問題への対策を検証します。
;;
;; 実際のネットワーク通信は行わず、`url-retrieve-synchronously' と
;; `sumibi-parse-http-body' をスタブ化して以下を検証します:
;;   1. 1回目で成功すれば再試行しない
;;   2. 接続失敗 (nil) の場合に新しい接続で再試行する
;;   3. すべて失敗した場合は 504 TIMEOUT ERROR を返し、試行回数は 1+リトライ回数
;;   4. リクエスト中は `url-http-attempt-keepalives' が nil に束縛されている
;;   5. `sumibi-api-max-retries' が 0 のときは再試行しない

;;; Code:

(require 'ert)

;; Provide minimal stubs for dependencies if not available
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

;; ------------------------------------------------------------------
;; テスト用ユーティリティ
;; ------------------------------------------------------------------

(defmacro sumibi-test--with-retrieve-stub (return-values call-counter keepalive-seen &rest body)
  "`url-retrieve-synchronously' をスタブ化して BODY を実行する。
RETURN-VALUES は呼び出しごとの成否を表すリスト (例: '(nil t))。
nil は接続失敗を、非 nil は成功を表す。成功時には実バッファを返し、
`sumibi-parse-http-body' は \(\"200\" . \"OK\") を返すようにスタブ化する。
CALL-COUNTER は呼び出し回数を記録する変数シンボル。
KEEPALIVE-SEEN は呼び出し時の `url-http-attempt-keepalives' の値を
記録する変数シンボル。"
  (declare (indent 3))
  `(let ((sumibi-test--queue ,return-values)
         (sumibi-test--buf (get-buffer-create " *sumibi-test-http*")))
     (unwind-protect
         (cl-letf (((symbol-function 'url-retrieve-synchronously)
                    (lambda (&rest _args)
                      (setq ,call-counter (1+ ,call-counter))
                      (push url-http-attempt-keepalives ,keepalive-seen)
                      (and (pop sumibi-test--queue) sumibi-test--buf)))
                   ((symbol-function 'sumibi-parse-http-body)
                    (lambda (_buf) (cons "200" "OK"))))
           ,@body)
       (when (buffer-live-p sumibi-test--buf)
         (kill-buffer sumibi-test--buf)))))

;; ------------------------------------------------------------------
;; 1. 1回目で成功すれば再試行しない
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-success-first-try ()
  "1回目で応答が得られた場合、再試行せず結果を返す。"
  (let ((calls 0) (keepalive nil) (sumibi-api-max-retries 1))
    (sumibi-test--with-retrieve-stub '("buf") calls keepalive
      (let ((result (sumibi--http-retrieve-synchronously "http://example.com")))
        (should (equal result (cons "200" "OK")))
        (should (= calls 1))))))

;; ------------------------------------------------------------------
;; 2. 接続失敗 (nil) の場合に新しい接続で再試行して成功する
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-recovers-after-nil ()
  "1回目が nil (接続失敗) でも、再試行で応答が得られれば成功する。"
  (let ((calls 0) (keepalive nil) (sumibi-api-max-retries 1))
    (sumibi-test--with-retrieve-stub '(nil "buf") calls keepalive
      (let ((result (sumibi--http-retrieve-synchronously "http://example.com")))
        (should (equal result (cons "200" "OK")))
        (should (= calls 2))))))

;; ------------------------------------------------------------------
;; 3. すべて失敗した場合は 504 TIMEOUT ERROR を返す
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-all-fail-returns-timeout ()
  "すべての試行が nil の場合、504 TIMEOUT ERROR を返し、
試行回数は 1 + `sumibi-api-max-retries' になる。"
  (let ((calls 0) (keepalive nil) (sumibi-api-max-retries 2))
    (sumibi-test--with-retrieve-stub '(nil nil nil) calls keepalive
      (let ((result (sumibi--http-retrieve-synchronously "http://example.com")))
        (should (string= (car result) "504"))
        (should (string-match-p "TIMEOUT ERROR" (cdr result)))
        (should (= calls 3))))))

;; ------------------------------------------------------------------
;; 4. リクエスト中は keep-alive が無効化されている
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-disables-keepalive ()
  "`url-retrieve-synchronously' 呼び出し時、
`url-http-attempt-keepalives' は nil に束縛されている。"
  (let ((calls 0) (keepalive nil) (sumibi-api-max-retries 1)
        (url-http-attempt-keepalives t)) ; 呼び出し前は t でも
    (sumibi-test--with-retrieve-stub '(nil "buf") calls keepalive
      (sumibi--http-retrieve-synchronously "http://example.com")
      ;; 記録された全ての呼び出しで keep-alive が nil
      (should (= 2 (length keepalive)))
      (should (cl-every #'null keepalive)))))

;; ------------------------------------------------------------------
;; 5. max-retries が 0 のときは再試行しない
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-zero-retries ()
  "`sumibi-api-max-retries' が 0 のときは再試行せず、1回だけ呼び出す。"
  (let ((calls 0) (keepalive nil) (sumibi-api-max-retries 0))
    (sumibi-test--with-retrieve-stub '(nil) calls keepalive
      (let ((result (sumibi--http-retrieve-synchronously "http://example.com")))
        (should (string= (car result) "504"))
        (should (= calls 1))))))

;; ------------------------------------------------------------------
;; 6. signal されたエラー (接続拒否など) も捕捉して再試行する
;;    (実機検証で url-retrieve-synchronously が接続拒否時に nil ではなく
;;     file-error を signal することが判明したため)
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-recovers-after-signaled-error ()
  "`url-retrieve-synchronously' がエラーを signal しても捕捉し、
後続の試行で応答が得られれば成功する。"
  (let ((calls 0) (sumibi-api-max-retries 2)
        (buf (get-buffer-create " *sumibi-test-http-sig*")))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve-synchronously)
                   (lambda (&rest _args)
                     (setq calls (1+ calls))
                     (if (= calls 1)
                         (signal 'file-error
                                 '("make client process failed" "Connection refused"))
                       buf)))
                  ((symbol-function 'sumibi-parse-http-body)
                   (lambda (_b) (cons "200" "OK"))))
          (let ((result (sumibi--http-retrieve-synchronously "http://example.com")))
            (should (equal result (cons "200" "OK")))
            (should (= calls 2))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest test-http-retry-all-signaled-errors-return-timeout ()
  "すべての試行がエラーを signal した場合でも、例外を投げずに
504 を返す (変換コマンド全体がエラーで中断しないことを保証する)。"
  (let ((calls 0) (sumibi-api-max-retries 1))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _args)
                 (setq calls (1+ calls))
                 (signal 'file-error
                         '("make client process failed" "Connection refused")))))
      (let ((result (sumibi--http-retrieve-synchronously "http://example.com")))
        (should (string= (car result) "504"))
        (should (= calls 2))))))

;; ------------------------------------------------------------------
;; 7. カスタマイズ変数のデフォルト値
;; ------------------------------------------------------------------

(ert-deftest test-http-retry-default-max-retries ()
  "`sumibi-api-max-retries' のデフォルト値が 1 であることを確認する。"
  (should (integerp (default-value 'sumibi-api-max-retries)))
  (should (= 1 (default-value 'sumibi-api-max-retries))))

(provide 'sumibi-http-retry-test)
;;; sumibi-http-retry-test.el ends here
