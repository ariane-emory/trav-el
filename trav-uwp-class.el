;; -*- lisp-indent-offset: 2; -*-
;; -*- lexical-binding: t-*-

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
  :documentation "A class to represent a Traveller-style UWP.")

(setq uwp--slot-names 
  '( starport
     size
     atmosphere
     hydrographics
     population
     government
     law-level
     tech-level))

(provide 'trav-uwp-class)

;;; trav-uwp-class.el ends here
