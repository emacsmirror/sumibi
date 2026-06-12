;;; sumibi-escape-json-test.el --- Tests for JSON escaping -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Claude Code
;; Keywords: tests

;;; Commentary:

;; Unit tests for `sumibi-escape-for-json'.
;;
;; Issue #152: 入力文字列に ^M (CR, U+000D) が混在していると、生の制御文字が
;; JSON 文字列リテラル中に残り、リクエストが不正な JSON になって変換に失敗する
;; (!!TIMEOUT ERROR!!)。CR を "\\r" にエスケープすることを検証する。

;;; Code:

(require 'ert)

;; Provide minimal stubs for dependencies if not available.
;; ASCII 文字に対して `unicode-escape' は何も変換しないため、利用できない環境
;; では恒等関数で代用する (本テストの入力はすべて ASCII)。
(unless (require 'popup nil 'noerror)
  (defvar popup-version "0-stub")
  (defun popup-menu* (&rest _args) (error "popup stub: not implemented"))
  (provide 'popup))

(unless (require 'unicode-escape nil 'noerror)
  (defun unicode-escape (s) s)
  (defun unicode-escape-to-string (s) s)
  (provide 'unicode-escape))

(unless (require 'deferred nil 'noerror)
  (defun deferred:succeed (&rest _args) nil)
  (defun deferred:nextc (&rest _args) nil)
  (defun deferred:error (&rest _args) nil)
  (provide 'deferred))

(unless (require 'dash nil 'noerror)
  (defun -filter (fn list)
    (let (result)
      (dolist (item list)
        (when (funcall fn item)
          (push item result)))
      (nreverse result)))
  (defun -map (fn list)
    (mapcar fn list))
  (provide 'dash))

;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))

(require 'sumibi)

;;; Tests for control character escaping

(ert-deftest test-escape-for-json-carriage-return ()
  "^M (CR, U+000D) は \\r にエスケープされること (Issue #152)."
  (should (string= "watashi\\rha"
                   (sumibi-escape-for-json "watashi\rha")))
  ;; 生の CR (char 13) が残っていないこと
  (should-not (cl-find ?\r (sumibi-escape-for-json "watashi\rha"))))

(ert-deftest test-escape-for-json-crlf ()
  "CRLF (\\r\\n) は両方エスケープされること."
  (should (string= "line1\\r\\nline2"
                   (sumibi-escape-for-json "line1\r\nline2"))))

(ert-deftest test-escape-for-json-newline ()
  "改行 (LF) は \\n にエスケープされること (既存動作の回帰確認)."
  (should (string= "a\\nb" (sumibi-escape-for-json "a\nb"))))

(ert-deftest test-escape-for-json-tab ()
  "タブは \\t にエスケープされること (既存動作の回帰確認)."
  (should (string= "a\\tb" (sumibi-escape-for-json "a\tb"))))

(ert-deftest test-escape-for-json-double-quote ()
  "ダブルクォートはエスケープされること (既存動作の回帰確認)."
  (should (string= "say \\\"hi\\\""
                   (sumibi-escape-for-json "say \"hi\""))))

(ert-deftest test-escape-for-json-backslash-removed ()
  "バックスラッシュは除去されること (既存動作の回帰確認)."
  (should (string= "ab" (sumibi-escape-for-json "a\\b"))))

(ert-deftest test-escape-for-json-plain-ascii ()
  "制御文字を含まない文字列はそのまま返ること."
  (should (string= "watashiha" (sumibi-escape-for-json "watashiha"))))

(ert-deftest test-escape-for-json-produces-valid-json ()
  "CR を含む入力でも、エスケープ結果が有効な JSON になること (Issue #152)."
  (let* ((content (sumibi-escape-for-json "watashi\rha"))
         (json (format "{\"content\": \"%s\"}" content))
         (parsed (json-parse-string json)))
    (should (string= "watashi\rha" (gethash "content" parsed)))))

(provide 'sumibi-escape-json-test)
;;; sumibi-escape-json-test.el ends here
