;;; trav.el --- Useful tools for the a classic science fiction RPG. -*- lexical-binding: t-*-

;; Copyright (c) 2022 Ariane Emory <ariane.emory@protonmail.com>

;; Author: Ariane Emory <ariane.emory@protonmail.com; URL: https://github.com/ariane-emory/trav-el
;; Package-Requires: (dash)
;; Package-Version: 20220119
;; Keywords: games
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A mode with useful tools for Traveller-like RPGs such as Cepheus Engine.
;;
;; Currently all it does is generate UWPs and produce descriptions for them, but it will probably do more things in the future, like generating entire SEC files and maybe trade results.
;;
;; This is a minor mode. Turn it on with:
;; ┌────
;; │ (trav-mode 1)
;; └────
;;
;; To insert a new, randomly generated UWP at point, hit 'C-c C-u C-i'.
;;
;; To produce a description of a UWP, place your cursor on it and hit 'C-c C-u C-d'.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(provide 'trav)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-each
  '( trav-helpers
     trav-roll
     trav-uwp-alists
     trav-uwp-class
     trav-uwp-getters-and-describers
     trav-uwp-constructor
     trav-uwp-parse
     trav-uwp-pretty-format
     trav-uwp-other-methods)
  (lambda (feat)
    "Remove each feature from features before requiring it so that evaluating this buffer will re-load every file in the package."
    (setq features (remove feat features))
    (require feat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-new-uwp (&optional describe)
  "Generate a new UWP and insert it at point."
  (interactive "P")
  (let ((u (create-uwp)))
    (insert (as-code u))
    (when describe 
      (insert
        "\n\n"
        (pretty-format u)
        "\n"))))

(defun describe-uwp-at-point ()
  "Insert a description of the UWP at point."
  (interactive)
  (setq u (parse-uwp (thing-at-point 'word 'no-properties)))
  (end-of-line)
  (newline)
  (insert (pretty-format u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar trav-mode-map
  (let ((trav-mode-map (make-sparse-keymap)))
    (define-key trav-mode-map
      (kbd "C-c C-u C-d") 'describe-uwp-at-point)
    (define-key trav-mode-map
      (kbd "C-c C-u C-i") 'insert-new-uwp)
    trav-mode-map)
  "Keymap used in trav-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode trav-mode
  "A mode with useful tools for Traveller-like RPGs such as Cepheus Engine."
  :lighter " ✹"
  (setq trav-mode-was-started 'yes))

;;; trav.el ends here
;; Local Variables:
;; lisp-indent-offset: 2
;; End:

