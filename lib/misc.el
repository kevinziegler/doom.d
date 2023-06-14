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

(defun string-as-tag (input-string)
  (string-inflection-underscore-function (string-replace " " "_" input-string)))

(defun titleize-tag (tag-string)
  (string-replace "_"
                  " "
                  (string-inflection-capital-underscore-function tag-string)))

(defun user-company-tag ()
  (when (boundp 'user-company) (string-as-tag user-company)))

(defun user-team-tag ()
  (when (boundp 'user-team) (string-as-tag user-team)))

(defun kdz/toggle-light-theme ()
  (interactive)
  (if-let ((doom-light-theme doom-light-theme)
           (doom-dark-theme doom-dark-theme)
           (toggle-to (if (eq doom-theme doom-dark-theme)
                          doom-light-theme doom-dark-theme)))
      (progn
        (setq doom-theme toggle-to)
        (load-theme doom-theme :no-confirm))
    (message "Must define values for 'doom-dark-theme and 'doom-light-theme")))

(defmacro kdz/treemacs-all-the-icons (extensions iconset icon &rest icon-params)
  `(treemacs-create-icon
       :icon (format " %s "
                     ,(append (list (intern (concat "all-the-icons-" iconset))
                                    icon
                                    :size 0.9)
                              icon-params))
       :extensions ,extensions
       :fallback 'same-as-icon))

(defun kdz/lsp-pyright-path-advice (origin-fn &rest args)
  "Advice to determine Python venv/executable values for lsp-pyright"
  (let* ((poetry-specified-venv
          (condition-case nil (poetry-venv-exist-p) (error nil)))
         (lsp-pyright-venv-path (or poetry-specified-venv
                                    lsp-pyright-venv-path))
         (asdf-specified-python (kdz/asdf-which "python")))
    ;; Our first preference is to use whatever poetry-specified environment
    ;; is detected, if one is detected at all.
    ;;
    ;; Failing that, we'll at least look for an ASDF-specified Python
    ;; executable.
    ;;
    ;; Lastly, we'll just fall back to allow lsp-pyright to find python and
    ;; venv values as it sees fit
    (if (and (not poetry-specified-venv) asdf-specified-python)
        asdf-specified-python
      (apply origin-fn args))))

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
  (mixed-pitch-mode t)
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
