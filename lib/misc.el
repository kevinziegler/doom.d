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

(defun kdz/org-goto-top-level ()
  (interactive)
  (let ((org-goto-interface 'outline-path-completion)
        (org-goto-max-level 1))
    (org-goto)
    (org-narrow-to-subtree)))

(defun kdz/org-open-one-on-one ()
  (interactive)
  (find-file "~/notes/one-on-one-discussions.org")
  (when (buffer-narrowed-p) (widen))
  (let ((org-goto-interface 'outline-path-completion)
        (org-goto-max-level 1))
    (org-goto))
  (org-narrow-to-subtree)
  (org-show-todo-tree))

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
