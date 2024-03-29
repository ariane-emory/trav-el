;;; trav-helpers.el ends here
;; -*- lisp-indent-offset: 2; -*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restrict (min-val max-val num)
  (max min-val (min max-val num)))

(defun join-symbols (symbols)
  (intern (string-join
            (mapcar #'symbol-name symbols)
            "-")))

(provide 'trav-helpers)

;;; trav-helpers.el ends here
