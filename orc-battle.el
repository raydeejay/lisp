;; orc-battle.el -- fight!

;; make this a package
;; add mode, keybindings, etc
;; color!

(require 'eieio)

(defvar orc-battle-player-health nil)
(defvar orc-battle-player-agility nil)
(defvar orc-battle-player-strength nil)

(defvar orc-battle-monsters nil)
(defvar orc-battle-monster-builders nil)
(defvar orc-battle-monster-num 12)

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
  ;; ;; the game looks terrible with this on :/
  ;;  (electric-indent-mode -1)
  (orc-battle-init)
  (game-loop)
  (when (player-dead)
    (insert "You have been killed. Game Over."))
  (when (monsters-dead)
    (insert "Congratulations! You have vanquished all of your foes.")))

(defun orc-battle-init ()
  (init-monsters)
  (init-player))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 orc-battle-player-agility) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (newline)
    (map 'list
         (lambda(m)
           (or (monster-dead m) (monster-attack m)))
         orc-battle-monsters)
    (game-loop)))

(defun init-player ()
  (setq orc-battle-player-health 30)
  (setq orc-battle-player-agility 30)
  (setq orc-battle-player-strength 30))

(defun player-dead ()
  (<= orc-battle-player-health 0))

(defun show-player ()
  (newline)
  (insert "You are a valiant knight with a health of ")
  (insert (int-to-string orc-battle-player-health))
  (insert ", an agility of ")
  (insert (int-to-string orc-battle-player-agility))
  (insert ", and a strength of ")
  (insert (int-to-string orc-battle-player-strength)))

(defun player-attack ()
  (newline)
  (insert "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash orc-battle-player-strength -1)))))
    (d (let ((x (randval (truncate (/ orc-battle-player-strength 6)))))
         (insert "Your double swing has a strength of ")
         (insert x)
         (newline)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ orc-battle-player-strength 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun random-monster ()
  (let ((m (aref orc-battle-monsters (random (length orc-battle-monsters)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (newline)
  (insert "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x orc-battle-monster-num)))
        (progn (insert "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref orc-battle-monsters (1- x))))
          (if (monster-dead m)
              (progn (insert "That monster is already dead.")
                     (pick-monster))
              m)))))

(defun init-monsters ()
  (setq orc-battle-monsters
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length orc-battle-monster-builders))
                         orc-battle-monster-builders) "monster"))
             (make-vector orc-battle-monster-num nil))))

(defmethod monster-dead (m)
  (<= (monster-health m) 0))
(defun monsters-dead ()
  (every 'monster-dead orc-battle-monsters))

(defmethod monster-health (m)
  (oref m health))

(defun show-monsters ()
  (newline)
  (insert "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
             (newline)
             (insert "    ")
             (insert (int-to-string (incf x)))
             (insert ". ")
             (if (monster-dead m)
                 (insert "**dead**")
                 (progn (insert "(Health=")
                         (insert (int-to-string (monster-health m)))
                         (insert ") ")
                         (monster-show m))))
         orc-battle-monsters)))

(defclass monster nil ((health :initform (randval 10)))
  "A monster")

(defmethod monster-hit (m x)
  (decf (oref m health) x)
  (if (monster-dead m)
      (progn (insert "You killed the ")
             (insert (symbol-name (object-class m)))
             (insert "! "))
      (progn (insert "You hit the ")
             (insert (symbol-name (object-class m)))
             (insert ", knocking off ")
             (insert (int-to-string x))
             (insert " health points! "))))

(defmethod monster-show (m)
  (insert "A fierce ")
  (insert (symbol-name (object-class m))))

(defmethod monster-attack (m))

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
       (decf orc-battle-player-health x)))

(defclass hydra (monster) nil
  "An hydra")
(push 'hydra orc-battle-monster-builders)

(defmethod monster-show ((m hydra))
  (insert "A malicious hydra with ")
  (insert (int-to-string (monster-health m)))
  (insert " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (oref m health) x)
  (if (monster-dead m)
      (insert "The corpse of the fully decapitated and decapacitated hydra
falls to the floor!")
      (progn (insert "You lop off ")
              (insert (int-to-string x))
              (insert " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (insert "A hydra attacks you with ")
    (insert (int-to-string x))
    (insert " of its heads! It also grows back one more head! ")
    (incf (oref m health))
    (decf orc-battle-player-health x)))

(defclass slime-mold (monster) ((sliminess :initform (randval 5)))
  "A slime mold")
(push 'slime-mold orc-battle-monster-builders)

(defmethod monster-show ((m slime-mold))
  (insert "A slime mold with a sliminess of ")
  (insert (int-to-string (oref m sliminess))))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (oref m sliminess))))
       (insert "A slime mold wraps around your legs and decreases your agility
by ")
       (insert (int-to-string x))
       (insert "! ")
       (decf orc-battle-player-agility x)
       (when (zerop (random 2))
         (insert "It also squirts in your face, taking away a health point! ")
         (decf orc-battle-player-health))))

(defclass brigand (monster) nil
  "A brigand")
(push 'brigand orc-battle-monster-builders)

(defmethod monster-attack ((m brigand))
  (let ((x (max orc-battle-player-health orc-battle-player-agility orc-battle-player-strength)))
    (cond ((= x orc-battle-player-health)
           (insert "A brigand hits you with his slingshot, taking off 2 health
points! ")
           (decf orc-battle-player-health 2))
          ((= x orc-battle-player-agility)
           (insert "A brigand catches your leg with his whip, taking off 2
agility points! ")
           (decf orc-battle-player-agility 2))
          ((= x orc-battle-player-strength)
           (insert "A brigand cuts your arm with his whip, taking off 2
strength points! ")
           (decf orc-battle-player-strength 2)))))

