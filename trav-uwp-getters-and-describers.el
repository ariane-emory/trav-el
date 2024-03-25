;; -*- lisp-indent-offset: 2; -*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name makers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uwp--slot-description-alist-name (slot-name)
  "Generate the name of the description alist for a particular slot."
  (join-symbols (list 'uwp- slot-name 'descriptions 'alist)))

(defun uwp--slot-action-name (action-name slot-name)
  "Generate a method name based on an action-name and a slot-name."
  (join-symbols (list action-name slot-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method makers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro uwp--make-slot-formatter (slot-name fun)
  "Generate a formatting function for a UWP's slot."
  (let* ((fun-name (uwp--slot-action-name 'format slot-name))
          (alist-name (uwp--slot-description-alist-name
                        slot-name))
          (alist (symbol-value alist-name))
          (mapped-keys (map-keys alist))
          (mmax (apply #'max mapped-keys))
          (mmin (apply #'min mapped-keys)))
    `(defun ,fun-name (obj)
       (funcall ,fun
         (alist-get (restrict ,mmin ,mmax obj) ',alist)))))

(defmacro uwp--make-slot-getter (slot-name)
  "Generate a getter method for a UWP's slot."
  (let ((action-name (uwp--slot-action-name 'get slot-name)))
    `(cl-defmethod ,action-name ((obj uwp))
       (slot-value obj ',slot-name))))

(defmacro uwp--make-slot-describer (slot-name)
  "Generate a describer method for a UWP's slot."
  (let* ((format-slot-action-name
           (uwp--slot-action-name  'format slot-name))
          (get-slot-action-name
            (uwp--slot-action-name 'get slot-name))
          (describe-slot-action-name
            (uwp--slot-action-name 'describe slot-name))
          (description-alist
            (symbol-value (uwp--slot-description-alist-name
                            slot-name)))
          (mapped-keys (map-keys description-alist))
          (mmin
            (apply #'min mapped-keys))
          (mmax
            (apply #'max mapped-keys)))
    `(cl-defmethod ,describe-slot-action-name ((obj uwp))
       (funcall ',format-slot-action-name
         (car (assoc
                (restrict ,mmin ,mmax
                  (funcall #',get-slot-action-name obj))
                ',description-alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make the slot formatters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(uwp--make-slot-formatter size
  (lambda (pair)
    (concat
      "~"
      (number-to-string (car pair))
      " km radius, ~"
      (number-to-string (cdr pair))
      " Gs gravity.")))

(uwp--make-slot-formatter atmosphere
  (lambda (val)
    (string-join `(,val "atmosphere.") " ")))

(uwp--make-slot-formatter hydrographics
  (lambda (l)
    (concat
      (nth 2 l)
      ", "
      (number-to-string (nth 0 l))
      " to "
      (number-to-string (nth 1 l))
      "% of surface covered by water.")))

(uwp--make-slot-formatter population
  (lambda (l)
    (let ((numeric-population (cadr l)))
      (if (= 0 numeric-population)
        "No population."
        (concat
          (nth 0 l)
          " of people (at least "
          (number-to-string numeric-population)
          ").")))))

(uwp--make-slot-formatter starport
  (lambda (l)
    (if (equal "X" l)
      "No starport."
      (concat l "-class starport."))))

(uwp--make-slot-formatter government
  (lambda (l) (concat l " government.")))

(uwp--make-slot-formatter law-level
  (lambda (l) (concat l " law.")))

(uwp--make-slot-formatter tech-level
  (lambda (val) (concat "Tech Level " (number-to-string val) ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make the slot getters/describers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(uwp--make-slot-getter size)
(uwp--make-slot-describer size)
(uwp--make-slot-getter atmosphere)
(uwp--make-slot-describer atmosphere)
(uwp--make-slot-getter hydrographics)
(uwp--make-slot-describer hydrographics)
(uwp--make-slot-getter population)
(uwp--make-slot-describer population)
(uwp--make-slot-getter starport)
(uwp--make-slot-describer starport)
(uwp--make-slot-getter government)
(uwp--make-slot-describer government)
(uwp--make-slot-getter law-level)
(uwp--make-slot-describer law-level)
(uwp--make-slot-getter tech-level)
(uwp--make-slot-describer tech-level)

(provide 'trav-uwp-getters-and-describers)

;;; trav-uwp-getters-and-setters.el ends here
