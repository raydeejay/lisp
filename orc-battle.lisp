;;; orc-battle.lisp --- an epic battle

(defpackage :net.raydeejay.orc-battle
  (:use :cl :ansi)
  (:import-from :ansi :princa)
  (:export :start))

(in-package :net.raydeejay.orc-battle)

;; selectively run code if inside/outside SLIME
;; #+:swank ()
;; #-:swank ()

(defvar *player-health* nil)
(defvar *player-agility* nil)
(defvar *player-strength* nil)

(defvar *monsters* nil)
(defvar *monster-builders* nil)
(defvar *monster-num* 12)

;; a little helper function
(defun randval (n)
  (1+ (random (max 1 n))))

;; player related functions
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princa :yellow
          "You are a valiant knight with a health of "
          *player-health*
          ", an agility of "
          *player-agility*
          ", and a strength of "
          *player-strength*
          "."
          :reset))

;; monster class
(defstruct monster (health (randval 10)))

(defgeneric monster-dead (m))

(defmethod monster-dead (m)
  (<= (monster-health m) 0))

(defgeneric monster-hit (m x))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princa :green
                     "You killed the "
                     (type-of m)
                     "! "
                     :reset))
      (progn (princa "You hit the "
                     (type-of m)
                     ", knocking off "
                     x
                     " health points! ")))
  (fresh-line))

(defgeneric monster-show (m))

(defmethod monster-show (m)
  (princa "A fierce " (type-of m)))

(defgeneric monster-attack (m))

(defmethod monster-attack (m))

;; functions that deal with monsters
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (fresh-line)
  (princa :cyan "Monster #: " :reset)
  (finish-output nil)
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princ "That monster is alread dead.")
                     (pick-monster))
              m)))))

(defun show-monsters ()
  (fresh-line)
  (princa :red "Your foes:" :reset)
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princa "    " :bold (format nil "~2d" (incf x)) ". " :reset)
           (if (monster-dead m)
               (princa :magenta "**dead**" :reset)
               (progn (princa "[" (format nil "~2d" (monster-health m)) "] ")
                      (monster-show m))))
         *monsters*)))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

;; orc

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princa "An orc swings his club at you and knocks off "
            x
            " of your health points. ")
    (fresh-line)
    (decf *player-health* x)))

;; hydra

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
             (fresh-line))
      (progn (princa "You lop off "
                     x
                     " of the hydra's heads! ")
             (fresh-line))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princa "A hydra attacks you with "
            x
            " of its heads! It also grows back one more head! ")
    (fresh-line)
    (incf (monster-health m))
    (decf *player-health* x)))

;; slime mold

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (fresh-line)
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (fresh-line)
      (decf *player-health*))))

;; brigand

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
           (fresh-line)
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (fresh-line)
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points!")
           (fresh-line)
           (decf *player-strength* 2)))))

;; turn function
(defun player-attack ()
  (fresh-line)
  (princa :cyan :bold "Attack style: [s]tab [d]ouble swing [r]oundhouse: " :reset)
  (finish-output nil)
  (case (read-char)
    (#\s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (#\d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (#\r (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
           (princa :magenta "Attack " x)
           (unless (monsters-dead)
             (monster-hit (random-monster) 1))))
    (otherwise (player-attack))))

;; monster initialization
(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (declare (ignore x))
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

;; main loop
(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

;; exported function to start the game
(defun start ()
  (init-monsters)
  (init-player)
  (game-loop)
  (fresh-line)
  (when (player-dead)
    (princa :red "You have been killed. Game Over." :reset))
  (when (monsters-dead)
    (princa :green :bold "Congratulations! You have vanquished all of your foes." :reset))
  (fresh-line))

