;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dice functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro roll (xdy)
  "Roll num side-sided dice and return the total."
  (let* ((parts (split-string (symbol-name xdy) "d"))
          (num (string-to-number (car parts)))
          (sides (string-to-number (cadr parts))))
    `(let* ((total 0))
       (dotimes (count ,num)
         (setq total (+ total 1 (random ,sides))))
       total)))

(provide 'trav-roll)
