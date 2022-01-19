;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-between-p (min max number)
  (and
    (>= number min)
    (<= number max)))

(defun restrict (min-val max-val num)
  (max min-val (min max-val num)))

(defmacro for-range (from to fun)
  `(let ((count ,from))
     (while (<= count ,to)
       (funcall ,fun count)
       (setq count (1+ count)))))

(defun line-printed (fun)
  (let ((f fun))
    (lambda (x)
      (insert (format number-to-string x) ": "
        (funcall f x) ".\n"))))

(defun print-on-lines (&rest lines)
  (mapcar (lambda (l) (insert ";; " l "\n")) lines))

(defun range-limit (min number max)
  (cond
    ((>= min number) min)
    ((<= max number) max)
    (t number)))

(defun join-symbols (symbols)
  (intern (string-join
            (mapcar #'symbol-name symbols)
            "-")))

(defun t-format (num)
  (upcase (format "%x" num)))

(provide 'trav-helpers)
