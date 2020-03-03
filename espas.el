(require 'cl)
(require 'gamegrid)
(require 'seq)

(cl-defstruct (espas--enemy (:constructor espas-enemy--create)
                            (:conc-name   espas-enemy--get-))
  (entrance-path nil) (x nil) (y nil) (increment-count 0) (increment-funct '1+))

(defconst espas-score-file-name "espas-scores")
(defconst espas-buffer-name     "*Espas*")
(defconst espas-buffer-width    25)
(defconst espas-buffer-height   25)
(defconst espas-empty           0)
(defconst espas-empty-options   '(((t 32)) nil nil))
(defconst espas-floor           1)
(defconst espas-floor-options   '(((glyph colorize) (t 32))
                                  ((color-x color-x) (mono-x mono-x) (color-tty color-tty))
                                  (((glyph color-x) [0   0   0])   (color-tty "black"))))
(defconst espas-wall            2)
(defconst espas-wall-options    '(((glyph colorize) (t ?+))
                                  ((color-x color-x) (mono-x mono-x) (color-tty color-tty))
                                  (((glyph color-x) [0.5 0.5 0.5]) (color-tty "gray"))))
(defconst espas-player          3)
(defconst espas-player-options  '(((glyph colorize) (t ?P))
                                  ((color-x color-x) (mono-x mono-x) (color-tty color-tty))
                                  (((glyph color-x) [0.9 0.3 0.7]) (color-tty "yellow"))))
(defconst espas-fire            4)
(defconst espas-fire-options    '(((glyph colorize) (t ?|))
                                  ((color-x color-x) (mono-x mono-x) (color-tty color-tty))
                                  (((glyph color-x) [1   0   0])   (color-tty "red"))))
(defconst espas-enemy           5)
(defconst espas-enemy-options   '(((glyph colorize) (t ?X))
                                  ((color-x color-x) (mono-x mono-x) (color-tty color-tty))
                                  (((glyph color-x) [0   1   0])   (color-tty "green"))))
(defconst espas-tick            0.08
  "Time interval between each update.")
(defvar   espas-score           0)
(defvar   espas-null-map        (let ((map (make-sparse-keymap)))
                                  (define-key map (kbd "q") #'bury-buffer)
                                  (define-key map (kbd "n") #'espas-start-game)

                                  map)
  "Espas's menu keymap.")
(defvar   espas-mode-map        (let ((map (make-sparse-keymap)))
                                  (define-key map (kbd "q")       #'espas-end-game)
                                  (define-key map (kbd "n")       #'espas-start-game)
                                  (define-key map (kbd "p")       #'espas-pause-game)
                                  (define-key map (kbd "SPC")     #'espas-player-fire)
                                  (define-key map (kbd "a")       #'espas-move-left)
                                  (define-key map (kbd "<left>")  #'espas-move-left)
                                  ;; (define-key map (kbd "s")       #'espas-move-down)
                                  ;; (define-key map (kbd "<down>")  #'espas-move-down)
                                  (define-key map (kbd "d")       #'espas-move-right)
                                  (define-key map (kbd "<right>") #'espas-move-right)
                                  ;; (define-key map (kbd "w")       #'espas-move-up)
                                  ;; (define-key map (kbd "<up>")    #'espas-move-up)

                                  map)
  "The in-game keymap.")
(defvar   espas-enemies         nil)
(defvar   espas-player-updates  nil)
(defvar   espas-player-bullets  nil)
(defvar   espas-fired           nil)
(defvar   espas-enemy-move-p    nil)
(defvar   espas-moved           nil)
(defvar   espas-paused          nil)
(defvar   espas-player-x        nil)
(defvar   espas-player-y        nil)

(defun espas-init-game-values ()
  "Return a list of starting enemies."

  (setq espas-enemies        (list
                               ;; First Row
                               (espas-enemy--create :entrance-path '(( 3 .  1) ( 3 .  2) ( 4 .  3)
                                                                     ( 4 .  4) ( 5 .  5) ( 6 .  6)
                                                                     ( 6 .  7) ( 6 .  8) ( 7 .  9)
                                                                     ( 7 . 10) ( 7 . 11) ( 8 . 12)
                                                                     ( 9 . 12) (10 . 13) (11 . 13)
                                                                     (12 . 13) (13 . 13) (14 . 12)
                                                                     (15 . 12) (16 . 11) (16 . 10)
                                                                     (17 . 10) (17 .  9) (18 .  8)
                                                                     (18 .  7) (19 .  6) (19 .  5)
                                                                     (18 .  6) (17 .  6) (17 .  7)
                                                                     (16 .  8)))
                               (espas-enemy--create :entrance-path `(,nil      ,nil      ( 1 .  1)
                                                                     ( 1 .  2) ( 2 .  3) ( 2 .  4)
                                                                     ( 3 .  5) ( 4 .  6) ( 4 .  7)
                                                                     ( 4 .  8) ( 5 .  9) ( 5 . 10)
                                                                     ( 5 . 11) ( 6 . 12) ( 7 . 12)
                                                                     ( 8 . 13) ( 9 . 13) (10 . 13)
                                                                     (11 . 13) (12 . 12) (13 . 12)
                                                                     (14 . 11) (15 . 11) (16 . 10)
                                                                     (16 .  9) (17 .  9) (17 .  8)
                                                                     (16 .  7) (15 .  6) (14 .  7)
                                                                     (13 .  8)))
                               (espas-enemy--create :entrance-path `(,nil      ,nil      ,nil
                                                                     ,nil      ( 3 .  1) ( 3 .  2)
                                                                     ( 3 .  3) ( 4 .  4) ( 4 .  5)
                                                                     ( 4 .  6) ( 5 .  7) ( 5 .  8)
                                                                     ( 5 .  9) ( 6 . 10) ( 6 . 11)
                                                                     ( 7 . 12) ( 7 . 13) ( 8 . 14)
                                                                     ( 9 . 15) (10 . 15) (11 . 14)
                                                                     (12 . 13) (12 . 12) (13 . 11)
                                                                     (14 . 10) (14 .  9) (14 .  8)
                                                                     (13 .  7) (12 .  6) (11 .  7)
                                                                     (10 .  8)))
                               (espas-enemy--create :entrance-path `(,nil      ,nil      ,nil
                                                                     ,nil      ,nil      ,nil
                                                                     ( 1 .  1) ( 1 .  2) ( 1 .  3)
                                                                     ( 2 .  4) ( 2 .  5) ( 2 .  6)
                                                                     ( 3 .  7) ( 3 .  8) ( 3 .  9)
                                                                     ( 4 . 10) ( 4 . 11) ( 5 . 12)
                                                                     ( 5 . 13) ( 6 . 14) ( 7 . 15)
                                                                     ( 8 . 15) ( 9 . 14) (10 . 13)
                                                                     (10 . 12) (10 . 11) (10 . 10)
                                                                     (10 .  9) ( 9 .  8) ( 8 .  7)
                                                                     ( 7 .  8)))
                               (espas-enemy--create :entrance-path `(,nil      ,nil      ,nil
                                                                     ,nil      ,nil      ,nil
                                                                     ,nil      ,nil      ( 3 .  1)
                                                                     ( 4 .  2) ( 5 .  3) ( 5 .  4)
                                                                     ( 6 .  5) ( 6 .  6) ( 7 .  7)
                                                                     ( 8 .  8) ( 8 .  9) ( 9 . 10)
                                                                     ( 9 . 11) (10 . 12) (10 . 11)
                                                                     (11 . 10) (11 .  9) (11 .  8)
                                                                     (10 .  7) ( 9 .  6) ( 8 .  6)
                                                                     ( 7 .  7) ( 6 .  7) ( 5 .  8)
                                                                     ( 4 .  8)))
                               (espas-enemy--create :entrance-path `(,nil      ,nil      ,nil
                                                                     ,nil      ,nil      ,nil
                                                                     ,nil      ,nil      ,nil
                                                                     ,nil      ( 2 .  1) ( 2 .  2)
                                                                     ( 2 .  3) ( 3 .  4) ( 3 .  5)
                                                                     ( 3 .  6) ( 4 .  7) ( 4 .  8)
                                                                     ( 5 .  9) ( 5 . 10) ( 6 . 11)
                                                                     ( 6 . 12) ( 7 . 12) ( 8 . 11)
                                                                     ( 7 . 10) ( 6 .  9) ( 5 .  8)
                                                                     ( 4 .  8) ( 3 .  7) ( 2 .  7)
                                                                     ( 1 .  8)))))
  (setq espas-player-updates ())
  (setq espas-player-bullets ())
  (setq espas-fired          0)
  (setq espas-enemy-move-p   nil)
  (setq espas-moved          nil)
  (setq espas-paused         nil)
  (setq espas-player-x       (/ espas-buffer-width  2))
  (setq espas-player-y       (- espas-buffer-height 3)))

(defun espas-display-options ()
  "Return a vector with display informations."

  (let ((vec (make-vector 256 nil)))
    (dotimes (c 256)
      (aset vec c (cond
                   ((= c espas-empty)  espas-empty-options)
                   ((= c espas-floor)  espas-floor-options)
                   ((= c espas-wall)   espas-wall-options)
                   ((= c espas-player) espas-player-options)
                   ((= c espas-fire)   espas-fire-options)
                   ((= c espas-enemy)  espas-enemy-options)
                   (t                  '(nil nil nil)))))

    vec))

;;; Game Control Functions
(defun espas-pause-game ()
  "Pause the game."
  (interactive)

  (setq espas-paused (not espas-paused)))

(defun espas-update-whether-fired ()
  "Update the counter of whether the player can fire"

  (setq espas-fired (mod (1+ espas-fired) 9)))
(defun espas-player-fire ()
  "Have the player fire."
  (interactive)

  (when (= espas-fired 0)
    (push (cons espas-player-x espas-player-y) espas-player-bullets)

    (espas-update-whether-fired)))

(defun espas-move-left ()
  "Move the player left."
  (interactive)

  (unless espas-moved
    (push (cons -1 0) espas-player-updates)

    (setq espas-moved t)))
(defun espas-move-down ()
  "Move the player left."
  (interactive)

  (unless espas-moved
    (push (cons 0 1) espas-player-updates)

    (setq espas-moved t)))
(defun espas-move-right ()
  "Move the player left."
  (interactive)

  (unless espas-moved
    (push (cons 1 0) espas-player-updates)

    (setq espas-moved t)))
(defun espas-move-up ()
  "Move the player left."
  (interactive)

  (unless espas-moved
    (push (cons 0 -1) espas-player-updates)

    (setq espas-moved t)))

;;; Game Setup Functions
(defun espas-init-buffer ()
  "Initialize the Espas buffer."

  (gamegrid-init-buffer espas-buffer-width espas-buffer-height espas-empty)

  (let ((buffer-read-only nil))
    (dotimes (y espas-buffer-height)
      (dotimes (x espas-buffer-width)
        (gamegrid-set-cell x y espas-wall)))

    (let ((y    1)
          (wMax (1- espas-buffer-width))
          (hMax (1- espas-buffer-height)))
      (while (< y hMax)
        (let ((x 1))
          (while (< x wMax)
            (gamegrid-set-cell x y espas-floor)

            (setq x (1+ x))))

        (setq y (1+ y)))))

  (gamegrid-set-cell espas-player-x espas-player-y espas-player))

(defun espas-reset-game ()
  "Reset the current Espas game."

  (gamegrid-kill-timer)
  (espas-init-buffer))

(defun espas-end-game ()
  "End the current Espas game."
  (interactive)

  (gamegrid-kill-timer)
  (use-local-map espas-null-map)

  (gamegrid-add-score espas-score-file-name espas-score))

(defun espas-update-game (buffer)
  "Update the game.
BUFFER is the buffer in which this function has been called.
It should be `espas-buffer-name`."

  (unless (or
            espas-paused
            (not (string= (buffer-name buffer) espas-buffer-name))
            (minibuffer-window-active-p (selected-window)))
    (setq espas-player-bullets (seq-filter
                                 (lambda (bullet)
                                   (let ((newX (car bullet))
                                         (newY (1- (cdr bullet))))
                                     (unless (= (cdr bullet) espas-player-y)
                                       (gamegrid-set-cell (car bullet)
                                                          (cdr bullet) espas-floor))

                                     (setcar bullet newX)
                                     (setcdr bullet newY)

                                     (if (= (gamegrid-get-cell newX newY) espas-wall)
                                         nil
                                       (gamegrid-set-cell newX newY espas-fire)

                                       t)))
                                 espas-player-bullets))
    (when espas-enemy-move-p
      (setq espas-enemies (seq-filter
                            (lambda (enemy)
                              (if (espas-enemy--get-entrance-path enemy)
                                  (let ((enemyPos (pop
                                                    (espas-enemy--get-entrance-path
                                                      enemy))))
                                    (when (and
                                            (espas-enemy--get-x enemy)
                                            (espas-enemy--get-y enemy))
                                      (gamegrid-set-cell
                                        (espas-enemy--get-x enemy)
                                        (espas-enemy--get-y enemy)
                                        espas-floor))

                                    (gamegrid-set-cell
                                      (car enemyPos)
                                      (cdr enemyPos)
                                      espas-enemy)

                                    (setf (espas-enemy--get-x enemy) (car enemyPos))
                                    (setf (espas-enemy--get-y enemy) (cdr enemyPos)))
                                (gamegrid-set-cell
                                  (espas-enemy--get-x enemy)
                                  (espas-enemy--get-y enemy)
                                  espas-floor)

                                (setf (espas-enemy--get-x enemy) (funcall
                                                                   (espas-enemy--get-increment-funct enemy)
                                                                   (espas-enemy--get-x               enemy)))
                                (gamegrid-set-cell
                                  (espas-enemy--get-x enemy)
                                  (espas-enemy--get-y enemy)
                                  espas-enemy)

                                (setf (espas-enemy--get-increment-count enemy) (mod (1+ (espas-enemy--get-increment-count enemy)) 4))
                                (when (= (espas-enemy--get-increment-count enemy) 0)
                                  (setf (espas-enemy--get-increment-funct enemy) (if (eq (espas-enemy--get-increment-funct enemy) '1+)
                                                                                     '1-
                                                                                   '1+))))

                              t)
                            espas-enemies)))

    (unless (null espas-player-updates)
      (let ((action (pop espas-player-updates)))
        (let ((newX (+ espas-player-x (car action)))
              (newY (+ espas-player-y (cdr action))))
          (unless (= (gamegrid-get-cell newX newY) espas-wall)
            (gamegrid-set-cell espas-player-x espas-player-y espas-floor)
            (gamegrid-set-cell newX           newY           espas-player)

            (setq espas-player-x newX
                  espas-player-y newY)))))

    (when (> espas-fired 0)
      (espas-update-whether-fired))
    (setq espas-enemy-move-p (not espas-enemy-move-p))
    (setq espas-moved nil)))

(defun espas-start-game ()
  "Start a new Espas game."
  (interactive)

  (unless (string= (buffer-name (current-buffer)) espas-buffer-name)
    (error "To start a new game, switch to the `espas-buffer-name` buffer."))

  (espas-reset-game)
  (use-local-map espas-mode-map)

  (gamegrid-start-timer espas-tick #'espas-update-game))

(define-derived-mode espas-mode special-mode "Espas"
  "Mode for Espas game"

  (add-hook 'kill-buffer-hook #'gamegrid-kill-timer nil t)

  (use-local-map espas-null-map)

  (gamegrid-init (espas-display-options)))

(defun espas ()
  "How to play the game should be placed in the docstring."
  (interactive)

  (switch-to-buffer espas-buffer-name)
  (gamegrid-kill-timer)

  (espas-mode)
  (espas-start-game))
