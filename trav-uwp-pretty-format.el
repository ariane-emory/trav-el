;; -*- lisp-indent-offset: 2; -*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod pretty-format ((obj uwp))
  (concat
    "\""
    (describe-starport obj) "\n"
    (describe-size obj) "\n"
    (describe-atmosphere obj) "\n"
    (describe-hydrographics obj) "\n"
    (describe-population obj) "\n"
    (describe-government obj) "\n"
    (describe-law-level obj) "\n"
    (describe-tech-level obj) "\n"
    "\""))

(setq-local debug-on-error 1)

(provide 'trav-uwp-pretty-format)

;;; trav-uwp-pretty-format.el ends here
