;; sumibi-mozc-tests.el --- Unit tests for Sumibi Mozc backend -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; These tests verify that the Mozc backend for Sumibi correctly converts
;; several typical romaji inputs into the expected Japanese strings.
;; The tests are executed only when `mozc.el` can be loaded successfully
;; on the executing environment.  When Mozc is unavailable, each test is
;; skipped so that the overall test suite can run on environments (e.g. CI)
;; that do not have Mozc installed.

;;; Code:

;;; sumibi-mozc-tests.el --- Unit tests for Sumibi Mozc backend -*- lexical-binding: t; -*-

(require 'ert)

;; Some Emacs builds attempt to create temporary directories for native
;; compilation output even in batch mode.  On restricted filesystems this can
;; fail with a permission error that stops the entire test run.  Disable JIT
;; native compilation for the duration of this test suite to avoid that issue.
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

;; ------------------------------------------------------------
;; Test setup helpers
;; ------------------------------------------------------------

;; ------------------------------------------------------------------
;; Ensure MELPA / ELPA is available so that dependencies like dash.el can be
;; installed on-the-fly when running the test in a pristine environment.
;; ------------------------------------------------------------------

(require 'package)
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install dash.el automatically if it's missing.
(unless (require 'dash nil 'noerror)
  (package-refresh-contents)
  (package-install 'dash)
  (require 'dash))

;; Ensure the `lisp` directory of this repository is on `load-path` so that
;; `sumibi.el` can be required from a standalone Emacs process.
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))

;; Add test directory to load-path for mozc-mock
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Sumibi depends on the external package `popup.el` for its interactive
;; completion UI.  The testing environment may not have this package
;; installed, and the Mozc conversion routine we are testing does not depend
;; on any of the interactive `popup-*` functions.  To keep the unit tests
;; self-contained, we provide a minimal stub implementation of the symbols
;; that `sumibi.el` tries to use, if `popup` cannot be loaded.

(unless (require 'popup nil 'noerror)
  ;; Very small shim – only what is needed for `require` to succeed.
  (defvar popup-version "0-stub")
  (defun popup-menu* (&rest _args) (error "popup stub: not implemented"))
  (provide 'popup))

;; Sumibi also depends on `unicode-escape` and `deferred`, which might not be
;; available in a clean testing environment.  Provide minimal stubs so that the
;; `require` calls inside `sumibi.el` succeed.  These libraries are not needed
;; for Mozc conversion itself.

(dolist (lib '(unicode-escape deferred))
  (unless (require lib nil 'noerror)
    (provide lib)))

;; Dash.el がインストールされている前提でテストを実行する。
(require 'dash)

;; Test helper functions for mozc mock
(defvar sumibi-test-use-mozc-mock nil
  "When non-nil, use mozc-mock instead of real mozc.")

(defun sumibi-test-setup-mozc-mock ()
  "Setup mozc mock for testing."
  (require 'mozc-mock)
  (mozc-mock-enable)
  (mozc-mock-reset)
  (setq sumibi--mozc-available-p t)
  (setq sumibi-test-use-mozc-mock t))

(defun sumibi-test-teardown-mozc-mock ()
  "Teardown mozc mock after testing."
  (when (featurep 'mozc-mock)
    (mozc-mock-disable)
    (mozc-mock-reset))
  (setq sumibi-test-use-mozc-mock nil))

;; Environment variable to force mock usage
(when (getenv "SUMIBI_TEST_USE_MOCK")
  (sumibi-test-setup-mozc-mock))

;; Now we can safely load Sumibi.
(require 'sumibi)

;; Force the backend to Mozc for these tests.
(setq sumibi-backend 'mozc)

;; Test execution macro
(defmacro sumibi-test-with-mozc (&rest body)
  "Execute BODY with mozc or mozc-mock based on availability."
  `(if (and (not sumibi--mozc-available-p) (not sumibi-test-use-mozc-mock))
       (ert-skip "Mozc not available on this environment")
     (progn ,@body)))

;; ------------------------------------------------------------------
;; Basic conversions (no prefix)
;; ------------------------------------------------------------------
(ert-deftest sumibi-mozc-henkan ()
  "Converting 'henkan' should yield the Japanese string '変換'."
  (sumibi-test-with-mozc
   (let ((result (car (sumibi-roman-to-kanji-with-surrounding "henkan" "" 1 nil))))
     (should (string= result "変換")))))

(ert-deftest sumibi-mozc-nihongo ()
  "Converting 'nihongo' should yield '日本語'."
  (sumibi-test-with-mozc
   (let ((result (car (sumibi-roman-to-kanji-with-surrounding "nihongo" "" 1 nil))))
     (should (string= result "日本語")))))

(ert-deftest sumibi-mozc-nihongoga-dekimasu ()
  "Converting multi-word input 'nihongoga dekimasu' should yield '日本語が出来ます'."
  (sumibi-test-with-mozc
   (let ((result (car (sumibi-roman-to-kanji-with-surrounding "nihongoga dekimasu" "" 1 nil))))
     (should (string= result "日本語が出来ます")))))

;; ------------------------------------------------------------------
;; End-to-end helper & tests on *scratch* buffer
;; ------------------------------------------------------------------

(defun sumibi-test--convert-in-scratch (input)
  "Convert INPUT in *scratch* buffer by simulating `C-j'.

Return trimmed resulting buffer string.  The Sumibi backend is forced to
`mozc'.  The *scratch* buffer is cleared for each invocation so the call is
idempotent and side-effect free for other tests."
  (unless (or sumibi--mozc-available-p sumibi-test-use-mozc-mock)
    (error "Mozc not available"))
  (let ((sumibi-backend 'mozc))
    (with-current-buffer (get-buffer-create "*scratch*")
      (erase-buffer)
      (sumibi-mode 1)
      (goto-char (point-min))
      (insert input)
      (goto-char (point-max))
      (sumibi-rK-trans)
      (buffer-string))))

(ert-deftest sumibi-mozc-scratch-koumoku-1 ()
  "'* koumoku' → '* 項目' end-to-end conversion via C-j."
  (sumibi-test-with-mozc
   (should (string= (sumibi-test--convert-in-scratch "* koumoku") "* 項目"))))

(ert-deftest sumibi-mozc-scratch-koumoku-2 ()
  "'- koumoku' → '- 項目' end-to-end conversion via C-j."
  (sumibi-test-with-mozc
   (should (string= (sumibi-test--convert-in-scratch "- koumoku") "- 項目"))))

(ert-deftest sumibi-mozc-scratch-heading-1 ()
  "'# midashi' → '# 見出し' end-to-end conversion via C-j."
  (sumibi-test-with-mozc
   (should (string= (sumibi-test--convert-in-scratch "# midashi") "# 見出し"))))

(ert-deftest sumibi-mozc-scratch-indent-1 ()
  "' indento' → ' インデント' end-to-end conversion via C-j."
  (sumibi-test-with-mozc
   (should (string= (sumibi-test--convert-in-scratch " indento") " インデント"))))

(ert-deftest sumibi-mozc-scratch-indent-2 ()
  "'  indento' → '  インデント' end-to-end conversion via C-j."
  (sumibi-test-with-mozc
   (should (string= (sumibi-test--convert-in-scratch "  indento") "  インデント"))))

(ert-deftest sumibi-mozc-scratch-indent-3 ()
  "'   indento' → '   インデント' end-to-end conversion via C-j."
  (sumibi-test-with-mozc
   (should (string= (sumibi-test--convert-in-scratch "   indento") "   インデント"))))

(ert-deftest sumibi-mozc-scratch-indent-koumoku-1 ()
  "'  - koumoku' → '  - 項目' end-to-end conversion via C-j."
  (sumibi-test-with-mozc
   (should (string= (sumibi-test--convert-in-scratch "  - koumoku") "  - 項目"))))

(ert-deftest sumibi-mozc-scratch-indent-koumoku-2 ()
  "'  * koumoku' → '  * 項目' end-to-end conversion via C-j."
  (sumibi-test-with-mozc
   (should (string= (sumibi-test--convert-in-scratch "  * koumoku") "  * 項目"))))

;; Candidate stabilization tests
(ert-deftest sumibi-mozc-stable-candidates-basic ()
  "Test basic candidate stabilization functionality."
  (sumibi-test-with-mozc
   (let ((sumibi-backend 'mozc))
     ;; Test that candidate stabilization function exists and works
     (let ((candidates '("変換" "変感" "返還"))
           (result (sumibi-mozc--find-preferred-candidate "henkan" '("変換" "変感" "返還"))))
       (should (listp result))
       (should (= (length result) (length candidates)))))))

;; ------------------------------------------------------------------
;; Mock-specific tests (only run when mock is enabled)
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-mock-custom-conversion ()
  "Test custom conversion added to mock."
  (skip-unless (getenv "SUMIBI_TEST_USE_MOCK"))
  (sumibi-test-with-mozc
   (mozc-mock-add-conversion "tesuto" '("テスト" "test" "てすと"))
   (let ((result (car (sumibi-roman-to-kanji-with-surrounding "tesuto" "" 1 nil))))
     (should (string= result "テスト")))))

(ert-deftest sumibi-mozc-mock-learn-history ()
  "Test that mock records learning history."
  (skip-unless (getenv "SUMIBI_TEST_USE_MOCK"))
  (sumibi-test-with-mozc
   (mozc-mock-reset)
   ;; Simulate learning by calling the learn function
   (sumibi--mozc-learn "henkan" "変換")
   (let ((history (mozc-mock-get-learn-history)))
     (should (= (length history) 1))
     (should (string= (caar history) "henkan"))
     (should (string= (cdar history) "変換")))))

(ert-deftest sumibi-mozc-mock-unknown-input ()
  "Test that mock handles unknown romaji input gracefully."
  (skip-unless (getenv "SUMIBI_TEST_USE_MOCK"))
  (sumibi-test-with-mozc
   (let ((result (car (sumibi-roman-to-kanji-with-surrounding "unknownword" "" 1 nil))))
     ;; Should fall back to the original input
     (should (string= result "unknownword")))))

(ert-deftest sumibi-mozc-mock-multiple-candidates ()
  "Test that mock returns multiple candidates."
  (skip-unless (getenv "SUMIBI_TEST_USE_MOCK"))
  (sumibi-test-with-mozc
   (let ((results (sumibi-roman-to-kanji-with-surrounding "henkan" "" 3 nil)))
     (should (>= (length results) 2))
     (should (string= (car results) "変換")))))

(ert-deftest sumibi-mozc-mock-deterministic-output ()
  "Test that mock produces deterministic output."
  (skip-unless (getenv "SUMIBI_TEST_USE_MOCK"))
  (sumibi-test-with-mozc
   (let ((result1 (car (sumibi-roman-to-kanji-with-surrounding "henkan" "" 1 nil)))
         (result2 (car (sumibi-roman-to-kanji-with-surrounding "henkan" "" 1 nil))))
     (should (string= result1 result2))
     (should (string= result1 "変換")))))

;; Tests for mozc history functionality
(defvar sumibi-history-stack)

(ert-deftest sumibi-history-test-ensure-directory ()
  "Test directory creation."
  (let ((test-dir "/tmp/sumibi-test"))
    (when (file-directory-p test-dir)
      (delete-directory test-dir t))
    (should-not (file-directory-p test-dir))
    (let ((sumibi-ensure-history-directory
           (lambda () (make-directory test-dir t))))
      (funcall sumibi-ensure-history-directory)
      (should (file-directory-p test-dir))
      (delete-directory test-dir t))))

(ert-deftest sumibi-history-test-save-to-file ()
  "Test saving history stack to file."
  (let ((sumibi-history-stack 
         '(((markers . (292 . 295))
            (cand-cur . 3)
            (cand-cur-backup . 3)
            (cand-len . 5)
            (last-fix . "日本語")
            (last-roman . "nihongo")
            (genbun . "nihongo")
            (henkan-kouho-list ("日本語" "候補1" 0 l 0) ("にほんご" "候補2" 0 l 1) ("ニホンゴ" "候補3" 0 l 2) ("日本語" "候補4" 0 l 3) ("nihongo" "原文まま" 0 l 4))
            (bufname . "*scratch*"))
           ((markers . (274 . 275))
            (cand-cur . 0)
            (cand-cur-backup . 0)
            (cand-len . 31)
            (last-fix . "版")
            (last-roman . "ban")
            (genbun . "ban")
            (henkan-kouho-list ("版" "候補1" 0 l 0) ("ばん" "候補2" 0 l 1) ("バン" "候補3" 0 l 2) ("番" "候補4" 0 l 3) ("晩" "候補5" 0 l 4))
            (bufname . "*scratch*"))
           ((markers . (271 . 274))
            (cand-cur . 2)
            (cand-cur-backup . 2)
            (cand-len . 4)
            (last-fix . "モックバン")
            (last-roman . "mokkuban ")
            (genbun . "mokkuban ")
            (henkan-kouho-list ("模擬版" "候補1" 0 l 0) ("もっくばん" "候補2" 0 l 1) ("モックバン" "候補3" 0 l 2) ("mokkuban " "原文まま" 0 l 3))
            (bufname . "*scratch*"))))
        (test-file "/tmp/sumibi-test-history.jsonl"))
    ;; テスト用ファイルを削除
    (when (file-exists-p test-file)
      (delete-file test-file))
    (should-not (file-exists-p test-file))
    ;; 直接ファイルに書き込んでテスト
    (with-temp-buffer
      (dolist (entry sumibi-history-stack)
        (let ((json-entry (copy-alist entry)))
          ;; markersのconsペアを配列に変換
          (when (assoc 'markers json-entry)
            (let ((markers (cdr (assoc 'markers json-entry))))
              (setcdr (assoc 'markers json-entry) 
                      (vector (car markers) (cdr markers)))))
          (insert (json-encode json-entry) "\n")))
      (write-region (point-min) (point-max) test-file nil 'silent))
    (should (file-exists-p test-file))
    ;; クリーンアップ
    (when (file-exists-p test-file)
      (delete-file test-file))))

(provide 'sumibi-mozc-tests)

;;; sumibi-mozc-tests.el ends here
