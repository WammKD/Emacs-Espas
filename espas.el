(require 'gamegrid)

(defconst espas-score-file-name "espas-scores")
(defconst espas-buffer-name     "*Espas*")
(defconst espas-buffer-width    17)
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
(defvar   espas-update-list     ())
(defvar   espas-moved           nil)
(defvar   espas-paused          nil)
(defvar   espas-player-x        (/ espas-buffer-width  2))
(defvar   espas-player-y        (- espas-buffer-height 3))

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
                   (t                  '(nil nil nil)))))

    vec))

(defun espas-pause-game ()
  "Pause the game."
  (interactive)

  (setq espas-paused (not espas-paused)))

(defun espas-move-left ()
  "Move the player left."
  (interactive)

  (unless espas-moved
    (push (cons -1 0) espas-update-list)

    (setq espas-moved t)))
(defun espas-move-down ()
  "Move the player left."
  (interactive)

  (unless espas-moved
    (push (cons 0 1) espas-update-list)

    (setq espas-moved t)))
(defun espas-move-right ()
  "Move the player left."
  (interactive)

  (unless espas-moved
    (push (cons 1 0) espas-update-list)

    (setq espas-moved t)))
(defun espas-move-up ()
  "Move the player left."
  (interactive)

  (unless espas-moved
    (push (cons 0 -1) espas-update-list)

    (setq espas-moved t)))

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
            (null espas-update-list))
    (let ((action (pop espas-update-list)))
      (let ((newX (+ espas-player-x (car action)))
            (newY (+ espas-player-y (cdr action))))
        (unless (= (gamegrid-get-cell newX newY) espas-wall)
          (gamegrid-set-cell espas-player-x espas-player-y espas-floor)
          (gamegrid-set-cell newX           newY           espas-player)

          (setq espas-player-x newX
                espas-player-y newY))))

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
