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

(defun http-basic-auth-token-b64 (host)
  "Find the first result for the supplied HOST in auth sources (or nil if no
   matching host is found) and return an HTTP basic auth header with a base64
   encoded token value"
  (let* ((credentials (nth 0 (auth-source-search :host host)))
         (user (plist-get credentials :user))
         (password (funcall (plist-get credentials :secret))))
    (base64-encode-string (concat user ":" password))))
