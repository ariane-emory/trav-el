;;; trav.el --- Useful tools for the a classic science fiction RPG.
;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

;; Copyright (c) 2022 Ariane Emory <ariange.emory@protonmail.com/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(provide 'trav)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-each
  '( trav-alists
     trav-helpers
     trav-roll
     trav-uwp-class
     trav-uwp-getters-and-describers
     trav-uwp-constructor
     trav-uwp-parse
     trav-uwp-pretty-format
     trav-uwp-other-methods)
  (lambda (feat)
    "Remove each feature from features before requiring it so that evaluating this buffer will re-load every file in the package."
    (setq features (remove feat features))
    (require feat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-new-uwp (&optional describe)
  "Generate a new UWP and insert it at point."
  (interactive "P")
  (let ((u (create-uwp)))
    (insert (as-code u))
    (when describe 
      (insert
        "\n\n"
        (pretty-format u)
        "\n"))))

(defun describe-uwp-at-point ()
  "Insert a description of the UWP at point."
  (interactive)
  (setq u (parse-uwp (thing-at-point 'word 'no-properties)))
  (end-of-line)
  (newline)
  (insert (pretty-format u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar trav-mode-map
  (let ((trav-mode-map (make-sparse-keymap)))
    (define-key trav-mode-map
      (kbd "C-c C-u C-d") 'describe-uwp-at-point)
    (define-key trav-mode-map
      (kbd "C-c C-u C-i") 'insert-new-uwp)
    trav-mode-map)
  "Keymap used in trav-mode.")

(define-minor-mode trav-mode
  "A mode with useful tools for Traveller-like RPGs such as Cepheus Engine."
  :lighter " ✹")
