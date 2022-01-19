;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

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
    (setq features (remove feat features))
    (require feat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-new-uwp (&optional describe)
  (interactive "P")
  (let ((u (create-uwp)))
    (insert (as-code u))
    (when describe 
      (insert
        "\n\n"
        (pretty-format u)
        "\n"))))

(defun describe-uwp-at-point ()
  (interactive)
  (setq u (parse-uwp (thing-at-point 'word 'no-properties)))
  (unless (looking-at "\s*$") (forward-word))
  (insert "\n" (pretty-format u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-u C-d") 'describe-uwp-at-point)
(global-set-key (kbd "C-c C-u C-i") 'insert-new-uwp)
