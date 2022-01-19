;; -*- lisp-indent-offset: 2; -*-
;; -*- fill-column: 60;-*-
;; -*- lexical-binding: t-*-

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

(cl-defmethod uwp--calc-tech-level
  (tech-level-roll starport size atmosphere)
  (restrict 0 #xF
    (+ tech-level-roll
      (cond
        ((= starport #xA) 6)
        ((= starport #xB) 4)
        ((= starport #xC) 2)
        ((= starport #xD) 0)
        ((= starport #xE) 0)
        ((= starport #xE) -4)
        (t 0)))))

(cl-defmethod uwp--init-tech-level ((obj uwp))
  (with-slots (starport size atmosphere hydrographics population
                government law-level tech-level) obj
    (set-slot-value obj 'tech-level
      (uwp--calc-tech-level
        tech-level starport size atmosphere))))

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

(provide 'trav-uwp-constructor)
