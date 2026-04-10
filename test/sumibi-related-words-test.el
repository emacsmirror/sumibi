;;; sumibi-related-words-test.el --- Tests for related words parsing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is part of Sumibi.

;;; Commentary:
;; Tests for `sumibi--parse-related-words' (Issue #135).

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

(unless (require 'sumibi-localdic nil 'noerror)
  (defvar sumibi-localdic-data nil)
  (defun sumibi-localdic-get-candidates (_key) nil)
  (provide 'sumibi-localdic))

(require 'sumibi)

;;; Tests for sumibi--parse-related-words

(ert-deftest test-parse-related-words-standard ()
  "Standard 3x3 response is parsed into 9 (word . category) pairs."
  (let ((result
         (sumibi--parse-related-words
          "類義語: 綺麗な 麗しい 華やかな\n反対語: 醜い 不細工な みすぼらしい\nフォーマル: 優美な 端麗な 佳麗な")))
    (should (= 9 (length result)))
    (should (equal (nth 0 result) '("綺麗な" . "類義語")))
    (should (equal (nth 2 result) '("華やかな" . "類義語")))
    (should (equal (nth 3 result) '("醜い" . "反対語")))
    (should (equal (nth 6 result) '("優美な" . "フォーマル")))
    (should (equal (nth 8 result) '("佳麗な" . "フォーマル")))))

(ert-deftest test-parse-related-words-full-width-colon ()
  "Full-width colon (：) is accepted as category separator."
  (let ((result
         (sumibi--parse-related-words
          "類義語： 速い 急ぐ 早い")))
    (should (= 3 (length result)))
    (should (equal (car result) '("速い" . "類義語")))))

(ert-deftest test-parse-related-words-ideographic-space ()
  "Ideographic space (　) works as a word separator."
  (let ((result
         (sumibi--parse-related-words
          "類義語: 速い　急ぐ　早い")))
    (should (= 3 (length result)))))

(ert-deftest test-parse-related-words-error-response ()
  "Error-like strings return nil (no category lines match)."
  (should (null (sumibi--parse-related-words "!!ERROR!!")))
  (should (null (sumibi--parse-related-words "")))
  (should (null (sumibi--parse-related-words nil))))

(ert-deftest test-parse-related-words-partial ()
  "Only one category present still parses that category."
  (let ((result
         (sumibi--parse-related-words
          "反対語: 遅い 鈍い のろい")))
    (should (= 3 (length result)))
    (should (equal (car result) '("遅い" . "反対語")))))

(ert-deftest test-parse-related-words-max-input-length-constant ()
  "Threshold constant exists and is 10."
  (should (= sumibi-related-words-max-input-length 10)))

(provide 'sumibi-related-words-test)
;;; sumibi-related-words-test.el ends here
