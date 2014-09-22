;; tictactoe.el -- play tic tac toe on Emacs

(defun tictactoe ()
  "Start playing tic tac toe."
  (interactive)
  (switch-to-buffer "*tictactoe*")
  (tictactoe-mode)
  (tictactoe-init))

(define-derived-mode tictactoe-mode special-mode "tic-tac-toe"
  (define-key tictactoe-mode-map (kbd "SPC") 'tictactoe-mark))

(defvar *tictactoe-board* nil
  "The board.")

(defconst *board-size* 3
  "The size of (a side of) the board.")

(defun tictactoe-init ()
  "Start a new game of tic tac toe."
  (setq *tictactoe-board* (make-vector (* *board-size* *board-size*) ?\. ))
  (tictactoe-print-board)
  (setq *tictactoe-current-player* ?\X))

(defun tictactoe-print-board ()
  "Prints the board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *board-size*)
      (dotimes (column *board-size*)
        (insert (tictactoe-get-square row column)))
      (newline))))

(defun tictactoe-get-square (row column)
  "Get the value in the (row column) square."
  (elt *tictactoe-board*
       (+ column
          (* row
             *board-size*))))

(defun tictactoe-set-square (row column value)
  "Set the value in the (row column) square."
  (aset *tictactoe-board*
        (+ column
           (* row
              *board-size*))
        value))

(defun tictactoe-mark ()
  "Marks the current square."
  (interactive)
  (let ((row (1- (line-number-at-pos)))
        (column (current-column)))
    (tictactoe-set-square row column *tictactoe-current-player*))
  (tictactoe-print-board)
  (when (tictactoe-game-has-been-won-p)
    (message "Cons! Player %c won!" *tictactoe-current-player*))
  (tictactoe-swap-players))

(defvar *tictactoe-current-player* nil
  "The character representing the current player."
  )

(defun tictactoe-swap-players ()
  "Swaps the current player."
  (setq *tictactoe-current-player*
        (cond ((char-equal *tictactoe-current-player* ?\X) ?\O)
              (t ?\X))))

(defun tictactoe-game-has-been-won-p ()
  "Returns t if the game has been won, nil otherwise."
  (or (tictactoe-diagonal-win-p)
      (tictactoe-column-win-p)
      (tictactoe-row-win-p)))

(defun tictactoe-diagonal-win-p ()
  "Returns t if any diagonal is a winning row, nil otherwise."
  (or (tictactoe-all-same-player-p '(0 0 1 1 2 2))
      (tictactoe-all-same-player-p '(0 2 1 1 2 0))))

(defun tictactoe-row-win-p ()
  "Returns t if any row is a winning row, nil otherwise."
  (or (tictactoe-all-same-player-p '(0 0 1 0 2 0))
      (tictactoe-all-same-player-p '(0 1 1 1 2 1))
      (tictactoe-all-same-player-p '(0 2 1 2 2 2))))

(defun tictactoe-column-win-p ()
  "Returns t if any diagonal is a winning row, nil otherwise."
  (or (tictactoe-all-same-player-p '(0 0 0 1 0 2))
      (tictactoe-all-same-player-p '(1 0 1 1 1 2))
      (tictactoe-all-same-player-p '(2 0 2 1 2 2))))

(defun tictactoe-all-same-player-p (cells)
  "Returns t if all the cells contain the same character, nil otherwise."
  (let ((cell nil))
    (setq cell (tictactoe-get-square
                (first cells)
                (second cells)))
    (and (tictactoe-is-a-player cell)
         (char-equal cell
                     (tictactoe-get-square
                      (third cells) (fourth cells)))
         (char-equal cell
                     (tictactoe-get-square
                      (fifth cells) (sixth cells))))))

(defun tictactoe-is-a-player (square)
  "Returns t if a square contains a player character, nil otherwise."
  (or (char-equal square ?\X)
      (char-equal square ?\O)))

