;;;###autoload
(defun kdz/lsp-java-enable-lombok-support (lombok-version)
  (let* ((mvn-base "~/.m2")
         (lombok-package "org.projectlombok.lombok")
         (lombok-jar-fmtstr "lombok-%s.jar"))
    (when (and lombok-version (boundp 'lsp-java-vmargs))
      (let ((lombok-jar-path
             (string-join (list
                           mvn-base
                           "repository"
                           (string-replace "."
                                           (f-path-separator)
                                           lombok-package)
                           lombok-version
                           (format lombok-jar-fmtstr lombok-version))
                          (f-path-separator))))
        (add-to-list 'lsp-java-vmargs (concat "-javaagent:" lombok-jar-path))))))

;; https://www.reddit.com/r/emacs/comments/e7h3qw/how_to_make_open_repl_window_behavior_in_doom/
(defun kdz/popup-display-buffer-side-by-size (buffer &optional alist)
  "Dynamically display popup buffers to either the right side or bottom"
  (+popup-display-buffer-stacked-side-window-fn
   buffer
   (append `((side . ,(if (> (frame-pixel-height) (frame-pixel-width))
                          'bottom 'right)))
           alist)))

(defun kdz/writing-minor-modes ()
  "Adjust various minor modes for writing quality-of-life"
  (mixed-pitch-mode)
  (visual-line-mode t)
  (display-fill-column-indicator-mode -1)
  (display-line-numbers-mode -1))

(defun kdz/vertico--format-candiate-marker-advice
    (orig cand prefix suffix index start)
  (setq cand (funcall orig cand prefix suffix index start))
  (concat (if (= vertico--index index)
              (propertize "» " 'face 'vertico-current)
            "  ")
          cand))
