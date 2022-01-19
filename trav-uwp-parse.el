;;; trav-uwp-parse.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'uwp--zip-numbers-with-slot-names 
  (-partial #'-zip uwp--slot-names))

(defun uwp--code-to-number-list (string)
  (-map (lambda (char)
          (string-to-number (char-to-string char) 16))
    string))

(cl-defmethod uwp--set-slots-from-alist ((obj uwp) alist)
  "Set a UWP objects slots from an alist."
  (-each alist
    (lambda (pair)
      (set-slot-value obj
        (car pair) (alist-get (car pair) alist))
      (set-slot-value obj
        (join-symbols (list (car pair) 'roll)) 0))))

(defun parse-uwp (string)
  "Parse a string to create a UWP object "
  (let* ( (obj (uwp))
          (numbers (uwp--code-to-number-list string)) 
          (alist (uwp--zip-numbers-with-slot-names
                   (cons
                     (cdr (assoc (substring string 0 1)
                            uwp--starport-values-reverse-alist))
                     (cdr numbers)))))
    (uwp--set-slots-from-alist obj alist)
    obj))

(provide 'trav-uwp-parse)

;;; trav-uwp-parse.el ends here

