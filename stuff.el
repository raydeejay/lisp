;; stuff.el -- some stuff with emacslisp

(defun stuff ()
  (interactive)
  (switch-to-buffer "*stuff*")
  (stuff-mode)
  (font-lock-mode))

(define-derived-mode stuff-mode
  special-mode "Stuff"
  "Major mode for doing stuff.
\\{stuff-mode-map}"
  :group "Stuff"
  (define-key stuff-mode-map (kbd "SPC") 'stuff-draw-patterns)
  (define-key stuff-mode-map [down-mouse-3] 'stuff-function)
  )

;;;; insert colored and/or bright text
(defun insert-colored-text (str clr bright)
  "Inserts str at point, in color clr, bright or not."
  (interactive (list (read-string " String: ")
                     (read-string " Color: ")
                     (y-or-n-p    " Bright? ") ))
  (insert (propertize str 'font-lock-face
                      `(:weight ,(if bright 'bold 'normal) :foreground ,clr) )))

(defalias 'insertc 'insert-colored-text)

;; silly example function
(defun stuff-function (name age)
  (interactive (list (read-string "Your name: ")
                     (read-number "Your age: ")))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insertc (format "Hello %s, of %d years of age!" name age)
                         "red" nil)))

(defun stuff-draw-patterns (width height count)
  (interactive (list (read-number "Width: ")
                     (read-number "Height: ")
                     (read-number "Count: ")))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (n count)
      (let ((length (1+ (random (/ width: 2)))))
        (dotimes (x width)
          (dotimes (y height)
            ))
        (insertc "hello" "green" nil)))

