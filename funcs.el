(defun kdz/writing-fill-column ()
  (setq-local display-fill-column-indicator nil)
  (setq-local fill-column 100)
  (mixed-pitch-mode)
  (visual-fill-column-mode))

(defun switch-to-message-buffer ()
    (interactive)
    (pop-to-buffer "*Messages*"))

(defun kdz/set-zshrc-sh-shell ()
  (when (string-match "zshrc$" buffer-file-name)
    (sh-set-shell "zsh")))
