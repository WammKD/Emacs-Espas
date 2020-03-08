(require 'cl)
(require 'gamegrid)
(require 'seq)
(require 'subr-x)

(cl-defstruct (espas--enemy (:constructor espas-enemy--create)
                            (:conc-name   espas-enemy--get-))
  (entrance-path nil) (to-position-path nil) (x nil) (y nil))

(defconst espas-score-file-name "espas-scores")
(defconst espas-buffer-name     "*Espas*")
(defconst espas-buffer-width    29)
(defconst espas-buffer-height   29)
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
(defconst intro1                '((16 .  1) (16 .  2) (15 .  3) (14 .  4)
                                  (13 .  5) (12 .  6) (11 .  7) (10 .  8)
                                  ( 9 .  9) ( 8 . 10) ( 7 . 11) ( 6 . 12)
                                  ( 6 . 13) ( 5 . 14) ( 5 . 15) ( 4 . 16)
                                  ( 4 . 17) ( 5 . 18) ( 6 . 19) ( 7 . 20)
                                  ( 8 . 21) ( 9 . 21) (10 . 20) (11 . 20)
                                  (12 . 19)))
(defconst intro3                '(( 1 . 20) ( 2 . 20) ( 3 . 19) ( 4 . 19)
                                  ( 5 . 19) ( 6 . 18) ( 7 . 18) ( 8 . 17)
                                  ( 9 . 16) ( 9 . 15) ( 8 . 13) ( 7 . 12)
                                  ( 6 . 11) ( 5 . 10) ( 4 . 11) ( 3 . 12)
                                  ( 3 . 13) ( 4 . 14) ( 5 . 15) ( 6 . 14)
                                  ( 7 . 13) ( 8 . 12) ( 9 . 11) (10 . 10)))
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
(defvar   espas-enemy-position  nil)
(defvar   espas-enemy-increment nil)
(defvar   espas-moved           nil)
(defvar   espas-paused          nil)
(defvar   espas-player-x        nil)
(defvar   espas-player-y        nil)

(defun espas-invert-x-of-path (path)
  "Convert path that leads in from the right to one that leads in from the left."

  (mapcar (lambda (coord)
            (cons (- (1- espas-buffer-width) (car coord)) (cdr coord))) path))

(defun espas-generate-line-path (start end)
  "Generate a series of coordinates when given the start and end coordinates."

  (let ((path (lexical-let* ((p1X (* 1.0 (car start)))
                             (p1Y (* 1.0 (cdr start)))
                             (p2X (* 1.0 (car end)))
                             (p2Y (* 1.0 (cdr end))))
                (lambda (y)
                  (+ (/ (- y p1Y) (/ (- p1Y p2Y) (- p1X p2X))) p1X))))
        (lst  '()))
    (mapcar
      (lambda (y)
        (cons (round (funcall path y)) y))
      (reverse (number-sequence (cdr end) (1- (cdr start)))))))

(defun espas-init-game-values ()
  "Return a list of starting enemies."

  (setq espas-enemies         (list
                                ;; First Row, left
                                (espas-enemy--create :entrance-path    intro1
                                                     :to-position-path (espas-generate-line-path
                                                                         '(12 . 19)
                                                                         '( 1 .  8)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 2 nil)
                                                                         intro1)
                                                     :to-position-path (espas-generate-line-path
                                                                         '(12 . 19)
                                                                         '( 6 .  8)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 4 nil)
                                                                         intro1)
                                                     :to-position-path (espas-generate-line-path
                                                                         '(12 . 19)
                                                                         '(11 .  8)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 6 nil)
                                                                         intro1)
                                                     :to-position-path (espas-generate-line-path
                                                                         '(12 . 19)
                                                                         '(12 .  8)))
                                ;; First Row, right
                                (espas-enemy--create :entrance-path    (espas-invert-x-of-path
                                                                         intro1)
                                                     :to-position-path (espas-generate-line-path
                                                                         '(16 . 19)
                                                                         '(22 .  8)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 2 nil)
                                                                         (espas-invert-x-of-path
                                                                           intro1))
                                                     :to-position-path (espas-generate-line-path
                                                                         '(16 . 19)
                                                                         '(21 .  8)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 4 nil)
                                                                         (espas-invert-x-of-path
                                                                           intro1))
                                                     :to-position-path (espas-generate-line-path
                                                                         '(16 . 19)
                                                                         '(20 .  8)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 6 nil)
                                                                         (espas-invert-x-of-path
                                                                           intro1))
                                                     :to-position-path (espas-generate-line-path
                                                                         '(16 . 19)
                                                                         '(15 .  8)))
                                ;; Second Row, left
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 42 nil)
                                                                         intro3)
                                                     :to-position-path (espas-generate-line-path
                                                                         '(10 . 10)
                                                                         '( 3 .  6)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 44 nil)
                                                                         intro3)
                                                     :to-position-path (espas-generate-line-path
                                                                         '(10 . 10)
                                                                         '( 8 .  6)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 46 nil)
                                                                         intro3)
                                                     :to-position-path (espas-generate-line-path
                                                                         '(10 . 10)
                                                                         '( 9 .  6)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 48 nil)
                                                                         intro3)
                                                     :to-position-path (espas-generate-line-path
                                                                         '(10 . 10)
                                                                         '(10 .  6)))
                                ;; Second Row, right
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 42 nil)
                                                                         (espas-invert-x-of-path
                                                                           intro3))
                                                     :to-position-path (espas-generate-line-path
                                                                         '(18 . 10)
                                                                         '(24 .  6)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 44 nil)
                                                                         (espas-invert-x-of-path
                                                                           intro3))
                                                     :to-position-path (espas-generate-line-path
                                                                         '(18 . 10)
                                                                         '(23 .  6)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 46 nil)
                                                                         (espas-invert-x-of-path
                                                                           intro3))
                                                     :to-position-path (espas-generate-line-path
                                                                         '(18 . 10)
                                                                         '(18 .  6)))
                                (espas-enemy--create :entrance-path    (append
                                                                         (make-list 48 nil)
                                                                         (espas-invert-x-of-path
                                                                           intro3))
                                                     :to-position-path (espas-generate-line-path
                                                                         '(18 . 10)
                                                                         '(13 .  6)))
                                ))
  (setq espas-player-updates  ())
  (setq espas-player-bullets  ())
  (setq espas-fired           0)
  (setq espas-enemy-move-p    nil)
  (setq espas-enemy-position  -1)
  (setq espas-enemy-increment '1+)
  (setq espas-moved           nil)
  (setq espas-paused          nil)
  (setq espas-player-x        (/ espas-buffer-width  2))
  (setq espas-player-y        (- espas-buffer-height 3)))

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

  (when (zerop espas-fired)
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

  (espas-init-game-values)

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

                                       (let ((result t))
                                         (setq espas-enemies (seq-filter
                                                               (lambda (enemy)
                                                                 (if-let ((enemyX (espas-enemy--get-x enemy))
                                                                          (enemyY (espas-enemy--get-y enemy)))
                                                                     (if (not (and
                                                                                (= enemyX newX)
                                                                                (= enemyY newY)))
                                                                         t
                                                                       (gamegrid-set-cell newX newY espas-floor)

                                                                       (setq result nil)

                                                                       nil)
                                                                   t))
                                                               espas-enemies))

                                         result))))
                                 espas-player-bullets))
    (when espas-enemy-move-p
      (dolist (enemy espas-enemies)
        (when-let ((newCord (cond
                              ((espas-enemy--get-entrance-path    enemy)
                                    (pop
                                      (espas-enemy--get-entrance-path     enemy)))
                              ((espas-enemy--get-to-position-path enemy)
                                    (pop
                                      (espas-enemy--get-to-position-path enemy)))
                              (t    (when (> 0 espas-enemy-position)
                                      (setq espas-enemy-position 0))

                                    (cons
                                      (funcall
                                        espas-enemy-increment
                                        (espas-enemy--get-x enemy))
                                      (espas-enemy--get-y enemy))))))
          (when (and (espas-enemy--get-x enemy) (espas-enemy--get-y enemy))
            (gamegrid-set-cell
              (espas-enemy--get-x enemy)
              (espas-enemy--get-y enemy)
              espas-floor))

          (gamegrid-set-cell (car newCord) (cdr newCord) espas-enemy)

          (setf (espas-enemy--get-x enemy) (car newCord))
          (setf (espas-enemy--get-y enemy) (cdr newCord))))

      (when (natnump espas-enemy-position)
        (setq espas-enemy-position (mod (1+ espas-enemy-position) 4))

        (when (zerop espas-enemy-position)
          (setq espas-enemy-increment (if (eq espas-enemy-increment '1+) '1- '1+)))))

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
