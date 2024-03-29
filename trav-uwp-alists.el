;; -*- lisp-indent-offset: 2; -*-
;; -*- lexical-binding: t-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description alists:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq uwp--empty-descriptions-alist
  '( (#x0 . ())
     (#x1 . ())
     (#x2 . ())
     (#x3 . ())
     (#x4 . ())
     (#x5 . ())
     (#x6 . ())
     (#x7 . ())
     (#x8 . ())
     (#x9 . ())
     (#xA . ())
     (#xB . ())
     (#xC . ())
     (#xD . ())
     (#xE . ())
     (#xF . ())))

(setq uwp--size-descriptions-alist
  '( (#x0 . (800 . 0.0))
     (#x1 . (1600 . 0.05))
     (#x2 . (3200 . 0.15))
     (#x3 . (4800 . 0.25))
     (#x4 . (6400 . 0.35))
     (#x5 . (8000 . 0.45))
     (#x6 . (9600 . 0.7))
     (#x7 . (11200 . 0.9))
     (#x8 . (12800 . 1.0))
     (#x9 . (14400 . 1.25))
     (#xA . (16000 . 1.4))))

(setq uwp--atmosphere-descriptions-alist
  '( (#x0 . "No")
     (#x1 . "Trace")
     (#x2 . "Very thin, tainted")
     (#x3 . "Very thin")
     (#x4 . "Thin, tainted")
     (#x5 . "Thin")
     (#x6 . "Standard")
     (#x7 . "Standard, tainted")
     (#x8 . "Dense")
     (#x9 . "Dense, tainted") 
     (#xA . "Exotic")
     (#xB . "Corrosive")
     (#xC . "Insidious")
     (#xD . "Dense, high")
     (#xE . "Thin, low")
     (#xF . "Unusual")))

(setq uwp--hydrographics-descriptions-alist
  '( (#x0 . (00  05 "Desert world"))
     (#x1 . (06  15 "Dry world"))
     (#x2 . (16  25 "A few small seas"))
     (#x3 . (26  35 "Small seas and oceans"))
     (#x4 . (36  45 "Wet world"))
     (#x5 . (46  55 "Large oceans"))
     (#x6 . (56  75 "Huge oceans"))
     (#x7 . (76  95 "Earth-like world"))
     (#x8 . (95 100 "Water world"))))

(setq uwp--population-descriptions-alist
  '( (#x0 . ("None" 0))
     (#x1 . ("Few" 10))
     (#x2 . ("Hundreds" 100))
     (#x3 . ("Thousands" 1000))
     (#x4 . ("Tens of thousands" 10000))
     (#x5 . ("Hundreds of thousands" 100000))
     (#x6 . ("Millions" 1000000))
     (#x7 . ("Tens of millions" 10000000))
     (#x8 . ("Hundreds of millions" 100000000))
     (#x9 . ("Billions" 1000000000))
     (#xA . ("Tens of billions" 10000000000))))

(setq uwp--starport-descriptions-alist
  '( (#x0 . "X")
     (#xA . "A")
     (#xB . "B")
     (#xC . "C")
     (#xD . "D")
     (#xE . "E")))

(setq uwp--starport-values-alist
  '( (#x2 . #x0)
     (#x3 . #xe)
     (#x4 . #xe)
     (#x5 . #xd)
     (#x6 . #xd)
     (#x7 . #xc)
     (#x8 . #xc)
     (#x9 . #xb)
     (#xA . #xb)
     (#xB . #xa)))

(setq uwp--starport-values-reverse-alist
  '( ("A" . #xa)
     ("B" . #xb)
     ("C" . #xc)
     ("D" . #xd)
     ("E" . #xe)
     ("X" . #x0)))

(setq uwp--government-descriptions-alist
  '( (#x0 . "No")
     (#x1 . "Company/corporation")
     (#x2 . "Participatory democracy")
     (#x3 . "Self-perpetuating oligarchy")
     (#x4 . "Representative democracy")
     (#x5 . "Feudal techocracy")
     (#x6 . "Captive")
     (#x7 . "Balkanized")
     (#x8 . "Civil service beaureaucracy")
     (#x9 . "Impersonal beaureaucracy")
     (#xA . "Charismatic dictator")
     (#xB . "Non-charismatic leader")
     (#xC . "Charismatic oligarchy")
     (#xD . "Religious dictatorship")
     (#xE . "Religious autocracy")
     (#xF . "Totalitarian oligarchy")))

(setq uwp--law-level-descriptions-alist
  '( (#x0 . "No")
     (#x1 . "Low")
     (#x2 . "Low")
     (#x3 . "Low")
     (#x4 . "Medium")
     (#x5 . "Medium")
     (#x6 . "Medium")
     (#x7 . "High")
     (#x8 . "High")
     (#x9 . "High")
     (#xA . "Extreme")))

(setq uwp--tech-level-descriptions-alist
  '( (#x0 . #x0)
     (#x1 . #x1)
     (#x2 . #x2)
     (#x3 . #x3)
     (#x4 . #x4)
     (#x5 . #x5)
     (#x6 . #x6)
     (#x7 . #x7)
     (#x8 . #x8)
     (#x9 . #x9)
     (#xA . #xA)
     (#xB . #xB)
     (#xC . #xC)
     (#xD . #xD)
     (#xE . #xE)
     (#xF . #xF)))

(setq uwp--tech-level-modifiers-alist
  '( (#x0 . ( (starport . -4) (size . 2) (atmosphere . 1) (hydrographics . 1) (population . 0) (government .  1)))
     (#x1 . ( (starport .  0) (size . 2) (atmosphere . 1) (hydrographics . 0) (population . 1) (government .  0)))
     (#x2 . ( (starport .  0) (size . 1) (atmosphere . 1) (hydrographics . 0) (population . 1) (government .  0)))
     (#x3 . ( (starport .  0) (size . 1) (atmosphere . 1) (hydrographics . 0) (population . 1) (government .  0)))
     (#x4 . ( (starport .  0) (size . 1) (atmosphere . 0) (hydrographics . 0) (population . 1) (government .  0)))
     (#x5 . ( (starport .  0) (size . 0) (atmosphere . 0) (hydrographics . 0) (population . 1) (government .  1)))
     (#x6 . ( (starport .  0) (size . 0) (atmosphere . 0) (hydrographics . 0) (population . 0) (government .  0)))
     (#x7 . ( (starport .  0) (size . 0) (atmosphere . 0) (hydrographics . 0) (population . 0) (government .  2)))
     (#x8 . ( (starport .  0) (size . 0) (atmosphere . 0) (hydrographics . 0) (population . 0) (government .  0)))
     (#x9 . ( (starport .  0) (size . 0) (atmosphere . 0) (hydrographics . 1) (population . 1) (government .  0)))
     (#xA . ( (starport .  6) (size . 0) (atmosphere . 1) (hydrographics . 2) (population . 2) (government .  0)))
     (#xB . ( (starport .  4) (size . 0) (atmosphere . 1) (hydrographics . 0) (population . 3) (government .  0)))
     (#xC . ( (starport .  2) (size . 0) (atmosphere . 1) (hydrographics . 0) (population . 4) (government .  0)))
     (#xD . ( (starport .  0) (size . 0) (atmosphere . 1) (hydrographics . 0) (population . 0) (government . -2)))
     (#xE . ( (starport .  0) (size . 0) (atmosphere . 1) (hydrographics . 0) (population . 0) (government . -2)))
     (#xF . ( (starport .  0) (size . 0) (atmosphere . 1) (hydrographics . 0) (population . 0) (government .  0)))))

(setq uwp--tech-level-minimums-list
  '( (atmosphere . ( ((0 1 2 3 10 11 12) . 7)
                     ((13 14) . 7)
                     ((4 7 9) . 5)))
     (hydrographics . ( ((0 10) . 4)
                        ((10) . 4)))
     (population . ( ((6 7 8 9 10 11 12) . 4)))))

(provide 'trav-uwp-alists)

;;; trav-uwp-alists.el ends here
