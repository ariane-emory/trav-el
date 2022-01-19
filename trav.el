;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

(require 'cl-lib)

(require 'seq)
(require 'dash)
(provide 'trav)

(-each
  '( trav-alists
     trav-helpers)
  (lambda (feat)
    (setq features (remove feat features))
    (require feat)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass uwp ()
  (
    (starport
      :initarg value
      :type number
      )
    (starport-roll
      :initarg value
      :type number
      :initform (roll 2d6))

    (size
      :initarg value
      :type number
      )
    (size-roll
      :initarg value
      :type number
      :initform (roll 2d6))

    (atmosphere
      :initarg value
      :type number
      )
    (atmosphere-roll
      :initarg value
      :type number
      :initform (roll 2d6))

    (hydrographics
      :initarg value
      :type number
      )
    (hydrographics-roll
      :initarg value
      :type number
      :initform (roll 2d6))

    (population
      :initarg value
      :type number
      )
    (population-roll
      :initarg value
      :type number
      :initform (roll 2d6))

    (government
      :initarg value
      :type number
      )
    (government-roll
      :initarg value
      :type number
      :initform (roll 2d6))
    
    (law-level
      :initarg value
      :type number
      )
    (law-level-roll
      :initarg value
      :type number
      :initform (roll 2d6))

    (tech-level
      :initarg value
      :type number
      )
    (tech-level-roll
      :initarg value
      :type number
      :initform (roll 1d6))
    )
  :documentation "A class to represent a Traveller UWP.")

(setq uwp--slot-names 
  '( starport
     size
     atmosphere
     hydrographics
     population
     government
     law-level
     tech-level))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod pretty-format ((obj uwp))
  (concat
    (describe-starport obj) "\n"
    (describe-size obj) "\n"
    (describe-atmosphere obj) "\n"
    (describe-hydrographics obj) "\n"
    (describe-population obj) "\n"
    (describe-government obj) "\n"
    (describe-law-level obj) "\n"
    (describe-tech-level obj)))

(setq-local debug-on-error 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'Constructor'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod uwp--copy-raw-rolls ((obj uwp))
  (mapcar
    (lambda (sn)
      (set-slot-value obj sn
        (slot-value obj
          (join-symbols (list sn 'roll)))))
    uwp--slot-names))

(cl-defmethod uwp--init-size ((obj uwp))
  (with-slots (starport size atmosphere population
                government law-level tech-level) obj
    (set-slot-value obj 'size
      (restrict 0 #xF (- size 2)))))

(cl-defmethod uwp--init-atmosphere ((obj uwp))
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'atmosphere
      (restrict 0 #xF (+ -7 atmosphere size)))))

(cl-defmethod uwp--init-hydrographics ((obj uwp))
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'hydrographics
      (if (member atmosphere '(0 1))
        0 (restrict 0 #xF
            (+ hydrographics
              (cond
                ((member atmosphere
                   '(0 1 #xA #xB #xC)) -4)
                ((eq atmosphere #xE) -2)
                (t 0)))) 0))))

(cl-defmethod uwp--init-population ((obj uwp))
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'population
      (restrict 0 #xF
        (+ population
          (if (< size  2) -1 0)
          (cond
            ((> atmosphere #x9) -2)
            ((= atmosphere #x6) +3)
            ((= atmosphere #x5) +1)
            ((= atmosphere #x8) +1)
            (t 0))
          (if (and (eq hydrographics 0)
                (< atmosphere 3)) -2 0))))))

(cl-defmethod uwp--init-government ((obj uwp))
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'government
      (if (= 0 size)
        0 (restrict 0 #xF
            (+ government
              population))))))

(cl-defmethod uwp--init-law-level ((obj uwp))
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'law-level
      (if (= 0 population)
        0 (restrict 0 #xF
            (+ -7 government))))))

(cl-defmethod uwp--calc-starport (starport-roll population)
  (cdr (assoc
         (restrict 2 #xB
           (+ population (- starport-roll 7)))
         uwp--starport-values-alist)))

(cl-defmethod uwp--init-starport ((obj uwp))
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'starport
      (uwp--calc-starport starport population))))

(cl-defmethod uwp--init-tech-level ((obj uwp))
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'tech-level
      (restrict 0 #xF
        (+ tech-level
          (cond
            ((= starport #xA) 6)
            ((= starport #xB) 4)
            ((= starport #xC) 2)
            ((= starport #xD) 0)
            ((= starport #xE) 0)
            ((= starport #xE) -4)
            (t 0)))))))

(cl-defmethod init ((obj uwp))
  (uwp--copy-raw-rolls obj)
  (uwp--init-size obj)
  (uwp--init-atmosphere obj)
  (uwp--init-hydrographics obj)
  (uwp--init-population obj)
  (uwp--init-government obj)
  (uwp--init-law-level obj)
  (uwp--init-starport obj)
  (uwp--init-tech-level obj)
  obj)

(defun create-uwp ()
  (let ((obj (uwp)))
    (init obj)))

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
  (save-excursion
    (unless (looking-at "\s*$") (forward-word))
    (insert "\n" (pretty-format u))))

(global-set-key (kbd "C-c C-u C-d") 'describe-uwp-at-point)
(global-set-key (kbd "C-c C-u C-i") 'insert-new-uwp)
