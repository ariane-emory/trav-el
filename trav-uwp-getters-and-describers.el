;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name makers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uwp-slot-description-alist-name
  (slot-name)
  (join-symbols (list 'uwp- slot-name 'descriptions 'alist)))

(defun uwp-slot-action-name
  (action-name slot-name)
  (join-symbols (list action-name slot-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method makers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-slot-formatter (slot-name fun)
  (let* ((fun-name (uwp-slot-action-name 'format slot-name))
          (alist-name (uwp-slot-description-alist-name
                        slot-name))
          (alist (symbol-value alist-name))
          (mapped-keys (map-keys alist))
          (mmax (apply #'max mapped-keys))
          (mmin (apply #'min mapped-keys)))
    `(defun ,fun-name (obj)
       (funcall ,fun
         (cdr (assoc (restrict ,mmin ,mmax obj) ',alist))))))
(make-slot-formatter size #'identity)
(format-size 8)

(defmacro make-slot-getter (slot-name)
  (let ((action-name (uwp-slot-action-name 'get slot-name)))
    `(cl-defmethod ,action-name ((obj uwp))
       (slot-value obj ',slot-name))))

(uwp-slot-action-name 'get 'size)

(defmacro make-slot-describer (slot-name)
  (let* ((format-slot-action-name
           (uwp-slot-action-name  'format slot-name))
          (get-slot-action-name
            (uwp-slot-action-name 'get slot-name))
          (describe-slot-action-name
            (uwp-slot-action-name 'describe slot-name))
          (description-alist
            (symbol-value (uwp-slot-description-alist-name
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
;; Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod map-slots ((obj uwp) fun)
  "Call fun on every slot of UWP in order."
  (mapcar fun uwp--slot-names))

(cl-defmethod as-code ((obj uwp))
  "Print a UWP object."
  (join-strings
    (map-slots obj
      (lambda (slot-name)
        (t-format (slot-value obj slot-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make the slot formatters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-slot-formatter size
  (lambda (pair)
    (concat
      "~"
      (number-to-string (car pair))
      " km radius, ~"
      (number-to-string (cdr pair))
      " Gs gravity.")))

(make-slot-formatter atmosphere
  (lambda (val)
    (join-with-spaces val "atmosphere.")))

(make-slot-formatter hydrographics
  (lambda (l)
    (concat
      (nth 2 l)
      ", "
      (number-to-string (nth 0 l))
      " to "
      (number-to-string (nth 1 l))
      "% of surface covered by water.")))

(make-slot-formatter population
  (lambda (l)
    (let ((numeric-population (cadr l)))
      (if (= 0 numeric-population)
        "No population."
        (concat
          (nth 0 l)
          " of people (at least "
          (number-to-string numeric-population)
          ").")))))

(make-slot-formatter starport
  (lambda (l)
    (if (equal "X" l)
      "No starport."
      (concat l "-class starport."))))

(make-slot-formatter government
  (lambda (l) (concat l " government.")))

(make-slot-formatter law-level
  (lambda (l) (concat l " law.")))

(make-slot-formatter tech-level
  (lambda (val) (concat "Tech Level " (number-to-string val) ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make the slot getters/describers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-slot-getter size)
(make-slot-describer size)
(make-slot-getter atmosphere)
(make-slot-describer atmosphere)
(make-slot-getter hydrographics)
(make-slot-describer hydrographics)
(make-slot-getter population)
(make-slot-describer population)
(make-slot-getter starport)
(make-slot-describer starport)
(make-slot-getter government)
(make-slot-describer government)
(make-slot-getter law-level)
(make-slot-describer law-level)
(make-slot-getter tech-level)
(make-slot-describer tech-level)

(provide 'trav-uwp-getters-and-describers)
