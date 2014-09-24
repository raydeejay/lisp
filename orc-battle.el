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

(defvar orc-battle-player-turns 0)
(defvar orc-battle-player-current-turn 0)
(defvar orc-battle-player-classes '("knight"
                                    "rogue"
                                    "warrior"
                                    "paladin"))
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
  (define-key orc-battle-mode-map (kbd "r") 'attack-roundhouse))

(defun orc-battle-init ()
  (init-monsters)
  (init-player))

(defun orc-battle-cycle ()
    (show-player)
    (setq orc-battle-player-turns
          (1+ (truncate (/ (max 0 orc-battle-player-agility) 15))))
    (setq orc-battle-player-current-turn
          (1+ orc-battle-player-turns))
    (phase-one))

(defun phase-one ()
  (when (= orc-battle-player-current-turn 0) (phase-two))
  (unless (or (monsters-dead-p) (player-dead-p))
    (show-monsters)
    (newline)
    (decf orc-battle-player-current-turn)
    (insert "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
    (newline)
    (recenter -2))
  (newline))

(defun phase-two ()
  (newline)
  (map 'list
       (lambda(m)
         (or (monster-dead-p m) (monster-attack m)))
       orc-battle-monsters)
  (orc-battle-cycle))

(defun end-game ()
  (when (player-dead-p)
    (insertc "You have been killed. Game Over." "red" nil))
  (when (monsters-dead-p)
    (insertc "Congratulations! You have vanquished all of your foes." "green" t)))

(defun init-player ()
  (setq orc-battle-player-health 30)
  (setq orc-battle-player-agility 30)
  (setq orc-battle-player-strength 30)
  (setq orc-battle-player-class
        (nth (1+ (random (length orc-battle-player-classes)))
             orc-battle-player-classes)))

(defun player-dead-p ()
  (<= orc-battle-player-health 0))

(defun show-player ()
  (newline)
  (insert "You are a valiant ")
  (insert orc-battle-player-class)
  (insert " with a health of ")
  (insert (int-to-string orc-battle-player-health))
  (insert ", an agility of ")
  (insert (int-to-string orc-battle-player-agility))
  (insert ", and a strength of ")
  (insert (int-to-string orc-battle-player-strength))
  (insert ".")
  (newline))

(defun attack-stab ()
  (interactive)
  (monster-hit (pick-monster)
               (+ 2 (randval (ash orc-battle-player-strength -1))))
  (phase-one))

(defun attack-double-swing ()
  (interactive)
  (let ((x (randval (truncate (/ orc-battle-player-strength 6)))))
    (newline)
    (insert "Your double swing has a strength of ")
    (insert (number-to-string x))
    (newline)
    (monster-hit (pick-monster) x)
    (unless (monsters-dead-p)
      (monster-hit (pick-monster) x)))
  (phase-one))

(defun attack-roundhouse ()
  (interactive)
  (dotimes (x (1+ (randval (truncate (/ orc-battle-player-strength 3)))))
    (unless (monsters-dead-p)
      (monster-hit (random-monster) 1)))
  (phase-one))

(defun random-monster ()
  (let ((m (aref orc-battle-monsters (random (length orc-battle-monsters)))))
    (if (monster-dead-p m)
        (random-monster)
        m)))

(defun pick-monster ()
  (newline)
  (insert "Monster #:")
  (recenter -2)
  (let ((x (read-number "Number: ")))
    (if (not (and (integerp x) (>= x 1) (<= x orc-battle-monster-num)))
        (progn (newline)
               (insert "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref orc-battle-monsters (1- x))))
          (if (monster-dead-p m)
              (progn (newline)
                     (insert "That monster is already dead.")
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
             (insert "    ")
             (insert (int-to-string (incf x)))
             (insert ". ")
             (if (monster-dead-p m)
                 (insertc "**dead**" "purple" nil)
                 (progn (insert "(Health=")
                         (insert (int-to-string (monster-health m)))
                         (insert ") ")
                         (monster-show m))))
         orc-battle-monsters))
  (newline))

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
      (progn (insertc "You killed the " "green" nil)
             (insertc (symbol-name (object-class m)) "green" nil)
             (insertc "! " "green" nil)
             (newline))
      (progn (insert "You hit the ")
             (insert (symbol-name (object-class m)))
             (insert ", knocking off ")
             (insert (int-to-string x))
             (insert " health points! ")
             (newline))))

(defmethod monster-show (m)
  (insert "A fierce ")
  (insert (symbol-name (object-class m))))

(defmethod monster-attack (m))

;; orc
(defclass orc (monster) ((club-level :initform (randval 8)))
  "An orc")
(push 'orc orc-battle-monster-builders)

(defmethod monster-show ((m orc))
  (insert "A wicked orc with a level ")
  (insert (int-to-string (oref m club-level)))
  (insert " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (oref m club-level))))
       (insert "An orc swings his club at you and knocks off ")
       (insert (int-to-string x))
       (insert " of your health points. ")
       (newline)
       (decf orc-battle-player-health x)))

;; hydra
(defclass hydra (monster) nil
  "An hydra")
(push 'hydra orc-battle-monster-builders)

(defmethod monster-show ((m hydra))
  (insert "A malicious hydra with ")
  (insert (int-to-string (monster-health m)))
  (insert " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (oref m health) x)
  (if (monster-dead-p m)
      (progn (insert "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
             (newline))
    (progn (insert "You lop off ")
           (insert (int-to-string x))
           (insert " of the hydra's heads! ")
           (newline))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (insert "A hydra attacks you with ")
    (insert (int-to-string x))
    (insert " of its heads! It also grows back one more head! ")
    (newline)
    (incf (oref m health))
    (decf orc-battle-player-health x)))

;; slime mold
(defclass slime-mold (monster) ((sliminess :initform (randval 5)))
  "A slime mold")
(push 'slime-mold orc-battle-monster-builders)

(defmethod monster-show ((m slime-mold))
  (insert "A slime mold with a sliminess of ")
  (insert (int-to-string (oref m sliminess))))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (oref m sliminess))))
       (insert "A slime mold wraps around your legs and decreases your agility by ")
       (insert (int-to-string x))
       (insert "! ")
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
