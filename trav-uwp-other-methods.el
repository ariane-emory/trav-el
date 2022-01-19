;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod map-slots ((obj uwp) fun)
  "Call fun on every slot of UWP in order."
  (mapcar fun uwp--slot-names))

(cl-defmethod as-code ((obj uwp))
  "Print a UWP object."
  (string-join
    (cons
      (alist-get (slot-value obj 'starport)
        uwp--starport-descriptions-alist)
      (cdr (map-slots obj
             (lambda (slot-name)
               (t-format (slot-value obj slot-name))))))))
  
(provide 'trav-uwp-other-methods)
