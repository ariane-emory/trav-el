;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'uwp--zip-numbers-with-slot-names 
  (-partial #'-zip uwp--slot-names))

(defun uwp--code-to-number-list (string)
  (-map (lambda (char)
          (string-to-number (char-to-string char) 16))
    string))

(cl-defmethod set-from-alist ((obj uwp) alist)
  (-each alist
    (lambda (pair)
      (set-slot-value obj
        (car pair) (cdr (assoc (car pair) alist)))
      (set-slot-value obj
        (join-symbols (list (car pair) 'roll)) 0))))

(defun parse-uwp (string)
  (let* ( (obj (uwp))
          (numbers (uwp--code-to-number-list string)) 
          (alist (uwp--zip-numbers-with-slot-names
                   (cons
                     (cdr (assoc (substring string 0 1)
                            uwp--starport-values-reverse-alist))
                     (cdr numbers)))))
    (set-from-alist obj alist)
    obj))

(provide 'trav-uwp-parse)
