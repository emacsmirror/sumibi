;;; mozc-mock.el --- Mock implementation of mozc.el for testing -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides a mock implementation of mozc.el for unit testing.
;; It replaces the actual mozc_server connections with deterministic
;; responses to ensure 100% test reproducibility.

;;; Code:

(defgroup mozc-mock nil
  "Mock implementation of mozc for testing."
  :group 'test)

(defvar mozc-mock--conversion-table
  '(;; Basic conversions
    ("henkan" . ("変換" "返還" "変感"))
    ("nihongo" . ("日本語" "ニホンゴ" "にほんご"))
    ("nihongoga" . ("日本語が" "ニホンゴガ" "にほんごが"))
    ("dekimasu" . ("出来ます" "できます" "デキマス"))
    ("nihongoga dekimasu" . ("日本語が出来ます" "ニホンゴガデキマス" "にほんごができます"))
    ("koumoku" . ("項目" "コウモク" "こうもく"))
    ("midashi" . ("見出し" "ミダシ" "みだし"))
    ("indento" . ("インデント" "indent" "いんでんと"))
    ;; Test data with readings
    ("tesuto" . ("テスト" "test" "てすと"))
    ("kanji" . ("漢字" "カンジ" "かんじ"))
    ("hiragana" . ("ひらがな" "平仮名" "ヒラガナ"))
    ("katakana" . ("カタカナ" "片仮名" "かたかな")))
  "Mock conversion table mapping romaji to candidate lists.")

(defvar mozc-mock--enabled nil
  "When non-nil, mozc mock is enabled.")

(defvar mozc-mock--session-active nil
  "Mock session state.")

(defvar mozc-mock--current-input ""
  "Current input buffer for the mock session.")

(defvar mozc-mock--current-candidates nil
  "Current candidates for the mock session.")

(defvar mozc-mock--candidate-index 0
  "Current candidate index.")

(defvar mozc-mock--learn-history nil
  "History of learned conversions for testing.")

;; Mock implementation of mozc functions
(defun mozc-session-create (&optional _arg)
  "Mock implementation of mozc-session-create."
  (when mozc-mock--enabled
    (setq mozc-mock--session-active t
          mozc-mock--current-input ""
          mozc-mock--current-candidates nil
          mozc-mock--candidate-index 0)))

(defun mozc-session-delete ()
  "Mock implementation of mozc-session-delete."
  (when mozc-mock--enabled
    (setq mozc-mock--session-active nil
          mozc-mock--current-input ""
          mozc-mock--current-candidates nil
          mozc-mock--candidate-index 0)))

(defun mozc-session-sendkey (key-list)
  "Mock implementation of mozc-session-sendkey."
  (when (and mozc-mock--enabled mozc-mock--session-active)
    (let ((key (car key-list)))
      (cond
       ;; Handle character input
       ((characterp key)
        (setq mozc-mock--current-input
              (concat mozc-mock--current-input (char-to-string key)))
        nil)
       
       ;; Handle space key (trigger conversion)
       ((eq key 'space)
        (let ((candidates (cdr (assoc mozc-mock--current-input
                                      mozc-mock--conversion-table))))
          (when candidates
            (setq mozc-mock--current-candidates candidates
                  mozc-mock--candidate-index 0)
            ;; Return mock protobuf structure
            `((candidates . ((candidate . ,(mapcar (lambda (cand)
                                                     `((value . ,cand)
                                                       (annotation . ((description . ,(mozc-mock--get-reading cand))))))
                                                   candidates))))))))
       
       ;; Handle up key
       ((eq key 'up)
        (when mozc-mock--current-candidates
          (setq mozc-mock--candidate-index 0))
        nil)
       
       ;; Handle down key
       ((eq key 'down)
        (when mozc-mock--current-candidates
          (setq mozc-mock--candidate-index
                (min (1+ mozc-mock--candidate-index)
                     (1- (length mozc-mock--current-candidates)))))
        nil)
       
       ;; Handle enter key (commit)
       ((eq key 'enter)
        (when (and mozc-mock--current-candidates
                   (>= mozc-mock--candidate-index 0)
                   (< mozc-mock--candidate-index (length mozc-mock--current-candidates)))
          (let ((committed (nth mozc-mock--candidate-index mozc-mock--current-candidates)))
            ;; Record the learning
            (push (cons mozc-mock--current-input committed) mozc-mock--learn-history)))
        (setq mozc-mock--current-input ""
              mozc-mock--current-candidates nil
              mozc-mock--candidate-index 0)
        nil)
       
       (t nil)))))

(defun mozc-protobuf-get (obj key)
  "Mock implementation of mozc-protobuf-get."
  (cdr (assq key obj)))

(defun mozc-mock--get-reading (candidate)
  "Get mock reading for CANDIDATE."
  (cond
   ((string-match-p "^[ぁ-ん]+$" candidate) candidate)  ; Already hiragana
   ((string-match-p "^[ァ-ヶー]+$" candidate) candidate) ; Katakana
   (t (concat "よみ" (substring candidate 0 1))))) ; Mock reading

;; Helper functions for testing
(defun mozc-mock-enable ()
  "Enable mozc mock."
  (setq mozc-mock--enabled t)
  (provide 'mozc))

(defun mozc-mock-disable ()
  "Disable mozc mock."
  (setq mozc-mock--enabled nil))

(defun mozc-mock-reset ()
  "Reset mozc mock state."
  (setq mozc-mock--session-active nil
        mozc-mock--current-input ""
        mozc-mock--current-candidates nil
        mozc-mock--candidate-index 0
        mozc-mock--learn-history nil))

(defun mozc-mock-get-learn-history ()
  "Get the learning history for testing."
  mozc-mock--learn-history)

(defun mozc-mock-add-conversion (romaji candidates)
  "Add a conversion rule to the mock table."
  (setq mozc-mock--conversion-table
        (cons (cons romaji candidates)
              (assq-delete-all romaji mozc-mock--conversion-table))))

(provide 'mozc-mock)

;;; mozc-mock.el ends here