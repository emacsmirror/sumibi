;;; sumibi-nil-markers-test.el --- Test for nil markers issue -*- lexical-binding: t; -*-

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

;; Set up test configuration to use /tmp/.sumibi/history.jsonl
(setq sumibi-history-file-path "/tmp/.sumibi/history.jsonl")

(ert-deftest test-sumibi-history-search-with-nil-markers ()
  "Test that sumibi-history-search handles nil markers correctly."
  (with-temp-buffer
    ;; Setup buffer
    (rename-buffer "test-buffer")
    (insert "こんにちは")
    
    ;; Create history entry with nil markers (simulating loaded from file)
    (setq sumibi-history-stack
          '(((markers . nil)
             (last-fix . "こんにちは")
             (bufname . "test-buffer")
             (cand-cur . 0)
             (cand-cur-backup . 0)
             (cand-len . 1)
             (genbun . "konnichiwa")
             (henkan-kouho-list . (("こんにちは"))))))
    
    ;; Initialize sumibi-markers to ensure it starts with a valid value
    (setq sumibi-markers (cons (point-min-marker) (point-max-marker)))
    (let ((old-markers sumibi-markers))
      
      ;; Test: sumibi-history-search should NOT find entries with nil markers
      (goto-char 3)  ; Position within the text
      (let ((found (sumibi-history-search (point) t)))
        ;; Should NOT find the entry (because markers are nil)
        (should-not found)
        ;; Markers should not change
        (should (eq sumibi-markers old-markers))))))

(ert-deftest test-sumibi-rK-trans-with-nil-markers ()
  "Test that sumibi-rK-trans handles nil markers in history correctly."
  (with-temp-buffer
    ;; Setup buffer
    (rename-buffer "test-buffer")
    (insert "テスト")
    
    ;; Create history entry with nil markers
    (setq sumibi-history-stack
          '(((markers . nil)
             (last-fix . "テスト")
             (bufname . "test-buffer")
             (cand-cur . 0)
             (cand-cur-backup . 0)
             (cand-len . 1)
             (genbun . "tesuto")
             (henkan-kouho-list . (("テスト"))))))
    
    ;; Initialize variables
    (setq sumibi-select-mode nil)
    (setq sumibi-markers nil)  ; Start with nil markers
    
    ;; Position cursor within the text
    (goto-char 3)
    
    ;; Test: sumibi-rK-trans should not crash
    (condition-case err
        (progn
          (sumibi-rK-trans)
          ;; If we get here, no error occurred
          (should t)
          ;; select-mode should not be activated because markers are nil
          (should-not sumibi-select-mode))
      (error
       ;; If an error occurs, the test fails
       (should-not (format "Unexpected error: %s" err))))))

(ert-deftest test-sumibi-history-mixed-markers ()
  "Test history with both nil and valid markers."
  (with-temp-buffer
    ;; Setup buffer
    (rename-buffer "test-buffer")
    (insert "あい")
    
    ;; Create valid markers for second entry
    (let ((start-marker (copy-marker 1))
          (end-marker (copy-marker (+ 1 (length "あい")))))
      
      ;; Create history with mixed entries
      (setq sumibi-history-stack
            `(;; First entry with nil markers - should be skipped
              ((markers . nil)
               (last-fix . "あいうえお")
               (bufname . "test-buffer")
               (cand-cur . 0)
               (cand-cur-backup . 0)
               (cand-len . 1)
               (genbun . "aiueo")
               (henkan-kouho-list . (("あいうえお"))))
              ;; Second entry with valid markers
              ((markers . ,(cons start-marker end-marker))
               (last-fix . "あい")
               (bufname . "test-buffer")
               (cand-cur . 0)
               (cand-cur-backup . 0)
               (cand-len . 1)
               (genbun . "ai")
               (henkan-kouho-list . (("あい"))))))
      
      ;; Test: Should find the valid entry and skip the nil one
      (goto-char 2)  ; Position within "あい"
      (let ((found (sumibi-history-search (point) t)))
        ;; Should find the second entry
        (should found)
        ;; Should have valid markers from the second entry
        (should sumibi-markers)
        (should (markerp (car sumibi-markers)))
        (should (markerp (cdr sumibi-markers)))
        ;; Should have loaded the correct data
        (should (string= sumibi-last-fix "あい"))))))

(provide 'sumibi-nil-markers-test)
;;; sumibi-nil-markers-test.el ends here