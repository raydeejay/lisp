;; evolution.el -- the evolution demo, ported to EmacsLisp

(defvar evolution-width 80)
(defvar evolution-height 30)
(defvar evolution-jungle-rectangle '(45 10 10 10))
(defvar evolution-plant-energy 80)

(defvar evolution-plants (make-hash-table :test #'equal))

;; create an initial animal, centered, with random genes
(defvar evolution-animals
  (list (make-animal :x      (ash evolution-width -1)
                     :y      (ash evolution-height -1)
                     :energy 1000
                     :dir    0
                     :genes  (loop repeat 8
                                   collecting (1+ (random 10))))))

(defvar evolution-reproduction-energy 200)

(defvar evolution-fence-color "yellow")

;; major mode things
(defun evolution ()
  (interactive)
  (switch-to-buffer "*evolution-game*")
  (evolution-mode)
  (font-lock-mode)
  (evolution-start))

(define-derived-mode evolution-mode
  special-mode "Evolution"
  "Major mode for doing evolution.
\\{evolution-mode-map}"
  :group "Evolution"
  (define-key evolution-mode-map (kbd "SPC") 'evolution-start))

;;;; insert colored and/or bright text
(defun insert-colored-text (str clr bright)
  "Inserts str at point, in color clr, bright or not."
  (interactive (list (read-string "String: ")
                     (read-string "Color: ")
                     (y-or-n-p    "Bright? ") ))
  (insert (propertize str 'font-lock-face
                      `(:weight ,(if bright 'bold 'normal) :foreground ,clr) )))

(defalias 'insertc 'insert-colored-text)

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos evolution-plants) t)))

(defun add-plants ()
  (apply 'random-plant evolution-jungle-rectangle)
  (random-plant 0 0 evolution-width evolution-height))

(defstruct animal x y energy dir genes)

(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal)
          (mod (+ x
                  (cond ((and (>= dir 2) (<= dir 4)) 1)
                        ((or (= dir 1) (= dir 5)) 0)
                        (t -1))
                  evolution-width)
               evolution-width))
    (setf (animal-y animal)
          (mod (+ y
                  (cond ((and (>= dir 1) (<= dir 2)) -1)
                        ((or (= dir 3) (= dir 7)) 0)
                        (t 1))
                  evolution-height)
               evolution-height))
    (decf (animal-energy animal))))

(defun turn (animal)
  (let ((x (random (apply '+ (animal-genes animal)))))
    (cl-labels ((angle (genes x)
                       (let ((xnu (- x (car genes))))
                         (if (< xnu 0)
                             0
                           (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                 8
                 )))))

(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos evolution-plants)
      (incf (animal-energy animal) evolution-plant-energy)
      (remhash pos evolution-plants))))

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e evolution-reproduction-energy)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-animal animal))
            (genes     (copy-list (animal-genes animal)))
            (mutation (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal) genes)
        (push animal-nu evolution-animals)))))

(defun update-world ()
  (setf evolution-animals (remove-if (lambda (animal)
                                       (<= (animal-energy animal) 0))
                                     evolution-animals))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        evolution-animals)
  (add-plants))

(defun draw-world ()
  (erase-buffer)
  (loop for y
        below evolution-height
        do (progn (newline)
                  (insertc "|" evolution-fence-color nil)
                  (loop for x
                        below evolution-width
                        do (cond ((some (lambda (animal)
                                                  (and (= (animal-x animal) x)
                                                       (= (animal-y animal) y)))
                                        evolution-animals)
                                  (insertc "m" "red" nil))
                                 ((gethash (cons x y) evolution-plants)
                                  (insertc "*" "green" nil))
                                 (t (insertc " " "darkgrey" nil))))
                  (insertc "|" evolution-fence-color nil)))
  (newline)
  (insert (format "Animals: %d -- Plants: %d"
                  (length evolution-animals)
                  (hash-table-count evolution-plants)
                  0))
  (redisplay t))

(defun evolution-start ()
  (interactive)
  (let ((inhibit-read-only t))
    (draw-world)
    (newline)
    (let ((str (read-string "Steps (or 'quit'): ")))
      (cond ((equal str "quit") ())
            (t (let ((x (string-to-int str)))
                 (if x
                     (loop for i
                           below x
                           do (update-world)
                           (draw-world))
                   (evolution))))))))

