;;;###autoload
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively #'magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section-1))))))))

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
