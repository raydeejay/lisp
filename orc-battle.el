;;; orc-battle.el --- fight!

;; Copyright (C) 2014 raydeejay

;; Author: raydeejay <raydeejay@mailinator.com>
;; Version: 0.1
;; Created: 2014-09-24
;; Package-Requires: ((eieio "0.1"))
;; Keywords: orc-battle

;; This file is not part of GNU Emacs.

;; MIT license.

;;; Code:
(require 'eieio)

(defvar orc-battle-player-health nil)
(defvar orc-battle-player-agility nil)
(defvar orc-battle-player-strength nil)

(defvar orc-battle-monsters nil)
(defvar orc-battle-monster-builders nil)
(defvar orc-battle-monster-num 12)

(defvar orc-battle-max-turns 0)
(defvar orc-battle-remaining-turns 0)
(defvar orc-battle-player-adjectives '("brave"
                                       "strong"
                                       "agile"
                                       "noble"
                                       "valiant"))
(defvar orc-battle-player-classes '("knight"
                                    "rogue"
                                    "warrior"
                                    "paladin"))
(defvar orc-battle-player-adjective nil)
(defvar orc-battle-player-class nil)

;; helpers
(defun insert-colored-text (str clr bright)
  "Inserts str at point, in color clr, bright or not."
  (interactive (list (read-string "String: ")
                     (read-string "Color: ")
                     (y-or-n-p    "Bright? ") ))
  (insert (propertize str 'font-lock-face
                      `(:weight ,(if bright 'bold 'normal) :foreground ,clr) )))

(defalias 'insertc 'insert-colored-text)

(defun randval (n)
  (1+ (random (max 1 n))))

;; command
(defun orc-battle ()
  (interactive)
  (switch-to-buffer "*orc-battle*")
  (orc-battle-mode)
  (font-lock-mode)
  (read-only-mode -1)
  (erase-buffer)
  (orc-battle-init)
  (let ((inhibit-read-only t))
    (orc-battle-cycle)))

(define-derived-mode orc-battle-mode
  special-mode "Orc Battle"
  "Major mode for playing the orc-battle game.
\\{orc-battle-mode-map}"
  :group "Orc-Battle"
  (define-key orc-battle-mode-map (kbd "s") 'attack-stab)
  (define-key orc-battle-mode-map (kbd "d") 'attack-double-swing)
  (define-key orc-battle-mode-map (kbd "r") 'attack-roundhouse)
  (define-key orc-battle-mode-map (kbd "n") 'orc-battle))

(defun orc-battle-init ()
  (init-monsters)
  (init-player))

(defun orc-battle-take-turn ()
  (if (or (monsters-dead-p) (player-dead-p))
      (end-game)
    (if (zerop orc-battle-remaining-turns)
        (monster-turn)
      (player-turn))))

(defun orc-battle-cycle ()
  (insertc "A new round begins!" "cyan" nil)
  (newline)
  (setq orc-battle-max-turns
        (1+ (truncate (/ (max 0 orc-battle-player-agility) 15))))
  (setq orc-battle-remaining-turns
        orc-battle-max-turns)
  (show-player)
  (orc-battle-take-turn))

(defun player-turn ()
  (decf orc-battle-remaining-turns)
  (unless (or (monsters-dead-p) (player-dead-p))
    (show-monsters)
    (insert "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
    (newline)
    (recenter -2))
  (newline))

(defun monster-turn ()
  (newline)
  (map 'list
       (lambda(m)
         (or (monster-dead-p m) (monster-attack m)))
       orc-battle-monsters)
  (newline)
  (orc-battle-cycle))

(defun end-game ()
  (when (player-dead-p)
    (insertc "You have been killed. Game Over." "red" nil))
  (when (monsters-dead-p)
    (insertc "Congratulations! You have vanquished all of your foes." "green" t))
  (newline)
  (insert "Press q to quit, or n to start a new game..")
  (newline))

(defun init-player ()
  (setq orc-battle-player-health 30)
  (setq orc-battle-player-agility 30)
  (setq orc-battle-player-strength 30)
  (setq orc-battle-player-class
        (nth (random (length orc-battle-player-classes))
             orc-battle-player-classes))
  (setq orc-battle-player-adjective
        (nth (random (length orc-battle-player-adjectives))
             orc-battle-player-adjectives)))

(defun player-dead-p ()
  (<= orc-battle-player-health 0))

(defun show-player ()
  (newline)
  (insert (format "You are a %s %s with a health of %d, an agility of %d and a strength of %d."
                  orc-battle-player-adjective
                  orc-battle-player-class
                  orc-battle-player-health
                  orc-battle-player-agility
                  orc-battle-player-strength))
  (newline))

;; Attacks
(defun attack-stab ()
  (interactive)
  (monster-hit (pick-monster)
               (+ 2 (randval (ash orc-battle-player-strength -1))))
  (orc-battle-take-turn))

(defun attack-double-swing ()
  (interactive)
  (let ((x (randval (truncate (/ orc-battle-player-strength 6)))))
    (newline)
    (insert (format "Your double swing has a strength of %d." x))
    (newline)
    (monster-hit (pick-monster) x)
    (unless (monsters-dead-p)
      (monster-hit (pick-monster) x)))
  (orc-battle-take-turn))

(defun attack-roundhouse ()
  (interactive)
  (dotimes (x (1+ (randval (truncate (/ orc-battle-player-strength 3)))))
    (unless (monsters-dead-p)
      (monster-hit (random-monster) 1)))
  (orc-battle-take-turn))

;; Choosing monsters
(defun random-monster ()
  (let ((m (aref orc-battle-monsters (random (length orc-battle-monsters)))))
    (if (monster-dead-p m)
        (random-monster)
        m)))

(defun pick-monster ()
  (insert "Monster #:")
  (recenter -2)
  (let ((x (read-number "Number: ")))
    (if (not (and (integerp x) (>= x 1) (<= x orc-battle-monster-num)))
        (progn (newline 2)
               (insert "That is not a valid monster number.")
               (newline 2)
               (pick-monster))
        (let ((m (aref orc-battle-monsters (1- x))))
          (if (monster-dead-p m)
              (progn (newline 2)
                     (insert "That monster is already dead.")
                     (newline 2)
                     (pick-monster))
              m)))))

(defun init-monsters ()
  (setq orc-battle-monsters
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length orc-battle-monster-builders))
                         orc-battle-monster-builders) "monster"))
             (make-vector orc-battle-monster-num nil))))

(defun monsters-dead-p ()
  (every 'monster-dead-p orc-battle-monsters))

(defun show-monsters ()
  (newline)
  (insertc "Your foes:" "red" t)
  (let ((x 0))
    (map 'list
         (lambda (m)
           (newline)
           (insert (format "%5d. " (incf x)))
           (if (monster-dead-p m)
               (insertc "**dead**" "purple" nil)
             (progn (insert (format "(Health=%2d) " (monster-health m)))
                    (monster-show m))))
         orc-battle-monsters))
  (newline 2))

;; base monster class
(defclass monster nil ((health :initform (randval 10)))
  "A monster")

(defmethod monster-dead-p (m)
  (<= (monster-health m) 0))

(defmethod monster-health (m)
  (oref m health))

(defmethod monster-hit (m x)
  (decf (oref m health) x)
  (if (monster-dead-p m)
      (progn (insertc (format "You killed the %s!"
                              (symbol-name (object-class m)))
                      "green" nil)
             (newline))
    (progn (insertc (format "You hit the %s, knocking off %d health points!"
                            (symbol-name (object-class m))
                            x)
                    "cyan" nil)
           (newline))))

(defmethod monster-show (m)
  (insert (format "A fierce %s." (symbol-name (object-class m)))))

(defmethod monster-attack (m))

;; orc
(defclass orc (monster) ((club-level :initform (randval 8)))
  "An orc")
(push 'orc orc-battle-monster-builders)

(defmethod monster-show ((m orc))
  (insert (format "A wicked orc with a level %d club" (oref m club-level))))

(defmethod monster-attack ((m orc))
  (let ((x (randval (oref m club-level))))
    (insert (format "An orc swings his club at you and knocks off %d of your health points." x))
       (newline)
       (decf orc-battle-player-health x)))

;; hydra
(defclass hydra (monster) nil
  "An hydra")
(push 'hydra orc-battle-monster-builders)

(defmethod monster-show ((m hydra))
  (insert (format "A malicious hydra with %d heads." (monster-health m))))

(defmethod monster-hit ((m hydra) x)
  (decf (oref m health) x)
  (if (monster-dead-p m)
      (progn (insert "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
             (newline))
    (progn (insert (format "You lop off %d of the hydra's heads!" x))
           (newline))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (insert (format "A hydra attacks you with %d of its heads!" x))
    (insert " It also grows back one more head!")
    (newline)
    (incf (oref m health))
    (decf orc-battle-player-health x)))

;; slime mold
(defclass slime-mold (monster) ((sliminess :initform (randval 5)))
  "A slime mold")
(push 'slime-mold orc-battle-monster-builders)

(defmethod monster-show ((m slime-mold))
  (insert (format "A slime mold with a sliminess of %d" (oref m sliminess))))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (oref m sliminess))))
    (insert (format "A slime mold wraps around your legs and decreases your agility by %d!" x))
    (newline)
    (decf orc-battle-player-agility x)
    (when (zerop (random 2))
      (insert "It also squirts in your face, taking away a health point! ")
      (newline)
      (decf orc-battle-player-health))))

;; brigand
(defclass brigand (monster) nil
  "A brigand")
(push 'brigand orc-battle-monster-builders)

(defmethod monster-attack ((m brigand))
  (let ((x (max orc-battle-player-health orc-battle-player-agility orc-battle-player-strength)))
    (cond ((= x orc-battle-player-health)
           (insert "A brigand hits you with his slingshot, taking off 2 health points! ")
           (newline)
           (decf orc-battle-player-health 2))
          ((= x orc-battle-player-agility)
           (insert "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (newline)
           (decf orc-battle-player-agility 2))
          ((= x orc-battle-player-strength)
           (insert "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (newline)
           (decf orc-battle-player-strength 2)))))

;;; orc-battle.el ends here
