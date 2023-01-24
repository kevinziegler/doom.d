(defvar kdz--treemacs-shrink-frame-on-close nil)

(defun kdz/treemacs-pixel-width ()
  (* treemacs-width (frame-char-width)))

(defun kdz/treemacs-init-grow-frame (visibility)
  (unless (eq visibility 'visible)
      (setq kdz--treemacs-shrink-frame-on-close t)
      (set-frame-width (selected-frame) (+ (frame-width) treemacs-width))

      (when (> (car (frame-position)) (kdz/treemacs-pixel-width))
        (set-frame-position (selected-frame)
                            (- (car (frame-position)) (kdz/treemacs-pixel-width))
                            (cdr (frame-position))))))

(defun kdz/treemacs-quit-shrink-frame ()
  (when kdz--treemacs-shrink-frame-on-close
    (setq kdz--treemacs-shrink-frame-on-close nil)
    (set-frame-width (selected-frame) (- (frame-width) treemacs-width))
    (set-frame-position (selected-frame)
                        (+ (car (frame-position)) (kdz/treemacs-pixel-width))
                        (cdr (frame-position)))))

(defun kdz/treemacs-toggle-resize-advice (original-fn &rest args)
  (require 'treemacs)
  (let ((treemacs-visibility (treemacs-current-visibility)))
    (apply original-fn args)
    (when (eq treemacs-visibility 'visible) (kdz/treemacs-quit-shrink-frame))))
