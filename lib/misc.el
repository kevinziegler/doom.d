;;;###autoload
(require 'string-inflection)

(defun kdz/set-zshrc-sh-shell ()
  (when buffer-file-name
    (when (string-match "zshrc$" buffer-file-name)
      (sh-set-shell "zsh"))))

(defun http-basic-auth-token-b64 (host)
  "Find the first result for the supplied HOST in auth sources (or nil if no
   matching host is found) and return an HTTP basic auth header with a base64
   encoded token value"
  (let* ((credentials (nth 0 (auth-source-search :host host)))
         (user (plist-get credentials :user))
         (password (funcall (plist-get credentials :secret))))
    (base64-encode-string (concat user ":" password))))

(defmacro kdz/treemacs-all-the-icons (extensions iconset icon &rest icon-params)
  `(treemacs-create-icon
       :icon (format " %s "
                     ,(append (list (intern (concat "all-the-icons-" iconset))
                                    icon
                                    :size 0.9)
                              icon-params))
       :extensions ,extensions
       :fallback 'same-as-icon))

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
