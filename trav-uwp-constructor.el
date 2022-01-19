;;; trav-uwp-constructor.el ends here
;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'Constructor'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod uwp--copy-raw-rolls ((obj uwp))
  "Stash the original rolls in a set of shadow slots for debugging purposes."
  (mapcar
    (lambda (sn)
      (set-slot-value obj sn
        (slot-value obj
          (join-symbols (list sn 'roll)))))
    uwp--slot-names))

(cl-defmethod uwp--init-size ((obj uwp))
  "Initialize a UWP's size score as per the Cepheus SRD's rules."
  (with-slots (starport size atmosphere population
                government law-level tech-level) obj
    (set-slot-value obj 'size
      (restrict 0 #xF (- size 2)))))

(cl-defmethod uwp--init-atmosphere ((obj uwp))
  "Initialize a UWP's atmosphere score as per the Cepheus SRD's rules."
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'atmosphere
      (restrict 0 #xF (+ -7 atmosphere size)))))

(cl-defmethod uwp--init-hydrographics ((obj uwp))
  "Initialize a UWP's hydrographics score as per the Cepheus SRD's rules."
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'hydrographics
      (if (member hydrographics '(0 1))
        0
        (restrict 0 #xF
          (+ hydrographics
            (cond
              ((member atmosphere
                 '(#xA #xB #xC)) -4)
              ((eq atmosphere #xE) -2)
              (t 0))))))))

  (cl-defmethod uwp--init-population ((obj uwp))
    "Initialize a UWP's population score as per the Cepheus SRD's rules."
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
  "Initialize a UWP's government score as per the Cepheus SRD's rules."
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'government
      (if (= 0 population)
        0 (restrict 0 #xF
            (+ government
              population
              -7))))))

(cl-defmethod uwp--init-law-level ((obj uwp))
  "Initialize a UWP's law level score as per the Cepheus SRD's rules."
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'law-level
      (if (= 0 population)
        0 (restrict 0 #xF
            (+ law-level
              government
              -7))))))

(defun uwp--calc-starport (starport-roll population)
  "Calculate a UWP's starport score as per the Cepheus SRD's rules."
  (alist-get
    (restrict 2 #xB
      (+ population (- starport-roll 7)))
    uwp--starport-values-alist))

(cl-defmethod uwp--init-starport ((obj uwp))
  "Initialize a UWP's starport score as per the Cepheus SRD's rules."
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'starport
      (uwp--calc-starport starport population))))

(defun uwp--get-tech-level-minimum (atmosphere hydrographics population)
  "Get a planet's minimum TL based on it's other scores as per the Cepheus SRD's rules."
  (let ((f (lambda (slot-symbol slot-value)
             (let* ((slot-symbol slot-symbol)
                     (slot-value slot-value)
                     (matches
                       (-filter (lambda (x)
                                  (member slot-value (car x)))
                         (alist-get slot-symbol uwp--tech-level-minimums-list))))
               (if matches
                 (cdar matches)
                 0)))))
    (max (funcall f 'atmosphere atmosphere)
      (funcall f 'hydrographics hydrographics)
      (funcall f 'population population))
    ))

(defun uwp--calc-tech-level
  (tech-level-roll starport size atmosphere hydrographics population government)
  "Calculate a UWP's TL score as per the Cepheus SRD's rules."
  (let* ((tl-mods uwp--tech-level-modifiers-alist)
          (get-mod (lambda (slot-symbol slot-value)
                     (alist-get slot-symbol
                       (alist-get slot-value
                         tl-mods))))
          (starport-mod (funcall get-mod 'starport starport))
          (size-mod (funcall get-mod 'size size))
          (atmosphere-mod (funcall get-mod 'atmosphere atmosphere))
          (hydrographics-mod (funcall get-mod 'hydrographics hydrographics))
          (population-mod (funcall get-mod 'population population))
          (government-mod (funcall get-mod 'government government)))
    (if (= 0 population)
      0 (max (+ tech-level-roll starport-mod size-mod atmosphere-mod
               hydrographics-mod population-mod government-mod)
          (uwp--get-tech-level-minimum atmosphere hydrographics population)))))

(cl-defmethod uwp--init-tech-level ((obj uwp))
  "Initialize a UWP's TL score as per the Cepheus SRD's rules."
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'tech-level
      (uwp--calc-tech-level
        tech-level starport size atmosphere
        hydrographics population government))))

(cl-defmethod init ((obj uwp))
  "Initialize a newly created UWP and calculate it's scores as per the Cepheus SRD."
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
  "Create and new UWP."
  (let ((obj (uwp)))
    (init obj)))

(provide 'trav-uwp-constructor)

;;; trav-uwp-constructor.el ends here
