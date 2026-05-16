;;; sumibi-punctuation-debounce-test.el --- Tests for punctuation auto-convert debounce -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kiyoka Nishiyama

;; Author: Kiyoka Nishiyama <kiyoka@sumibi.org>
;; Keywords: japanese, ime

;;; Commentary:
;; このファイルは、句読点トリガーの自動変換の遅延（debounce）機能のテストを提供します。
;; Issue #147 で導入された `sumibi-ambient-punctuation-delay' に関する単体テスト。
;;
;; 実際のキー入力を伴うエンドツーエンド動作（次キー入力でのキャンセル等）は
;; ERT のバッチ実行で再現するのが難しいため、ここでは以下を検証します:
;;   1. カスタマイズ変数のデフォルト値
;;   2. 遅延ありモードで `sumibi-check-particle-trigger' がタイマーをセットする
;;   3. 遅延なし（0）モードでは即時動作で変換が実行される（タイマーは未セット）
;;   4. `sumibi--ambient-pre-command-cancel' で保留タイマーがキャンセルされる
;;   5. `sumibi-setup-auto-convert-hook' が `pre-command-hook' を管理する

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

(defmacro sumibi-test--with-ambient-buffer (&rest body)
  "自動変換トリガー条件を満たすバッファ環境で BODY を実行する。
`sumibi-rK-trans' は副作用が大きいためスタブ化し、呼び出されたかどうかを
動的変数 `sumibi-test--rK-trans-called' に記録する。"
  (declare (indent 0))
  `(let ((sumibi-ambient-enable t)
         (sumibi-mode t)
         (sumibi-select-mode nil)
         (sumibi-test--rK-trans-called nil))
     (cl-letf (((symbol-function 'sumibi-rK-trans)
                (lambda (&rest _args)
                  (setq sumibi-test--rK-trans-called t))))
       (with-temp-buffer
         (text-mode)
         ,@body))))

;; ------------------------------------------------------------------
;; カスタマイズ変数のデフォルト値
;; ------------------------------------------------------------------

(ert-deftest test-punctuation-delay-default ()
  "`sumibi-ambient-punctuation-delay' のデフォルト値が 0.5 秒であることを確認する。"
  (should (numberp sumibi-ambient-punctuation-delay))
  (should (= 0.5 (default-value 'sumibi-ambient-punctuation-delay))))

;; ------------------------------------------------------------------
;; タイマー管理ヘルパーの動作
;; ------------------------------------------------------------------

(ert-deftest test-cancel-punctuation-timer-handles-nil ()
  "保留中のタイマーがない場合にキャンセル関数を呼んでもエラーにならない。"
  (with-temp-buffer
    (setq sumibi--ambient-punctuation-timer nil)
    (sumibi--ambient-cancel-punctuation-timer)
    (should-not sumibi--ambient-punctuation-timer)))

(ert-deftest test-cancel-punctuation-timer-cancels-pending ()
  "保留中のタイマーがある場合、キャンセル関数で nil に戻る。"
  (with-temp-buffer
    (setq sumibi--ambient-punctuation-timer
          (run-with-timer 60 nil #'ignore))
    (should (timerp sumibi--ambient-punctuation-timer))
    (sumibi--ambient-cancel-punctuation-timer)
    (should-not sumibi--ambient-punctuation-timer)))

(ert-deftest test-pre-command-cancel-cancels-pending ()
  "`sumibi--ambient-pre-command-cancel' は保留中タイマーをキャンセルする。"
  (with-temp-buffer
    (setq sumibi--ambient-punctuation-timer
          (run-with-timer 60 nil #'ignore))
    (sumibi--ambient-pre-command-cancel)
    (should-not sumibi--ambient-punctuation-timer)))

;; ------------------------------------------------------------------
;; 句読点トリガーの遅延／即時動作
;; ------------------------------------------------------------------

(ert-deftest test-punctuation-trigger-schedules-timer-when-delay-positive ()
  "遅延 > 0 の場合、句読点入力でタイマーがセットされ即時変換は実行されない。"
  (sumibi-test--with-ambient-buffer
    (let ((sumibi-ambient-punctuation-delay 0.5))
      (insert "watashi.")
      (unwind-protect
          (progn
            (sumibi-check-particle-trigger)
            (should (timerp sumibi--ambient-punctuation-timer))
            (should-not sumibi-test--rK-trans-called))
        (sumibi--ambient-cancel-punctuation-timer)))))

(ert-deftest test-punctuation-trigger-immediate-when-delay-zero ()
  "遅延 = 0 の場合、句読点入力でその場で変換が実行される（タイマー未使用）。"
  (sumibi-test--with-ambient-buffer
    (let ((sumibi-ambient-punctuation-delay 0))
      (insert "watashi.")
      (sumibi-check-particle-trigger)
      (should-not sumibi--ambient-punctuation-timer)
      (should sumibi-test--rK-trans-called))))

(ert-deftest test-punctuation-trigger-immediate-when-delay-nil ()
  "遅延が nil の場合も即時変換になる（後方互換）。"
  (sumibi-test--with-ambient-buffer
    (let ((sumibi-ambient-punctuation-delay nil))
      (insert "watashi.")
      (sumibi-check-particle-trigger)
      (should-not sumibi--ambient-punctuation-timer)
      (should sumibi-test--rK-trans-called))))

(ert-deftest test-punctuation-trigger-replaces-old-timer ()
  "句読点を連続入力した場合、古いタイマーはキャンセルされ新しいタイマーに差し替わる。"
  (sumibi-test--with-ambient-buffer
    (let ((sumibi-ambient-punctuation-delay 0.5))
      (insert "watashi.")
      (sumibi-check-particle-trigger)
      (let ((first-timer sumibi--ambient-punctuation-timer))
        (should (timerp first-timer))
        (insert "nihon,")
        (unwind-protect
            (progn
              (sumibi-check-particle-trigger)
              (should (timerp sumibi--ambient-punctuation-timer))
              (should-not (eq first-timer sumibi--ambient-punctuation-timer)))
          (sumibi--ambient-cancel-punctuation-timer))))))

;; ------------------------------------------------------------------
;; 助詞+スペーストリガーは遅延の影響を受けない
;; ------------------------------------------------------------------

(ert-deftest test-particle-space-trigger-not-delayed ()
  "助詞+スペーストリガーは `sumibi-ambient-punctuation-delay' の影響を受けず即時実行される。"
  (sumibi-test--with-ambient-buffer
    (let ((sumibi-ambient-punctuation-delay 0.5))
      (insert "watashiwa ")
      (sumibi-check-particle-trigger)
      (should-not sumibi--ambient-punctuation-timer)
      (should sumibi-test--rK-trans-called))))

;; ------------------------------------------------------------------
;; setup フックが pre-command-hook を管理する
;; ------------------------------------------------------------------

(ert-deftest test-setup-hook-adds-pre-command-hook ()
  "sumibi-mode 有効化時に `pre-command-hook' (バッファローカル) に
キャンセルハンドラが追加される。"
  (with-temp-buffer
    (let ((sumibi-mode t))
      (sumibi-setup-auto-convert-hook)
      (should (memq #'sumibi--ambient-pre-command-cancel
                    (or pre-command-hook (default-value 'pre-command-hook)))))))

(ert-deftest test-setup-hook-removes-pre-command-hook ()
  "sumibi-mode 無効化時に `pre-command-hook' からキャンセルハンドラが除去される。"
  (with-temp-buffer
    ;; 一旦追加
    (let ((sumibi-mode t))
      (sumibi-setup-auto-convert-hook))
    (should (memq #'sumibi--ambient-pre-command-cancel
                  (or pre-command-hook (default-value 'pre-command-hook))))
    ;; 無効化で除去
    (let ((sumibi-mode nil))
      (sumibi-setup-auto-convert-hook))
    (should-not (memq #'sumibi--ambient-pre-command-cancel
                      pre-command-hook))))

(ert-deftest test-setup-hook-cancels-timer-on-disable ()
  "sumibi-mode 無効化時に保留中タイマーもキャンセルされる。"
  (with-temp-buffer
    (setq sumibi--ambient-punctuation-timer
          (run-with-timer 60 nil #'ignore))
    (let ((sumibi-mode nil))
      (sumibi-setup-auto-convert-hook))
    (should-not sumibi--ambient-punctuation-timer)))

(provide 'sumibi-punctuation-debounce-test)
;;; sumibi-punctuation-debounce-test.el ends here
