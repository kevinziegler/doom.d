(defun kdz/writing-fill-column ()
  (setq-local display-fill-column-indicator nil)
  ;;(setq-local fill-column 100)
  ;;(visual-fill-column-mode)
  (mixed-pitch-mode))

(defun switch-to-message-buffer ()
  (interactive)
  (pop-to-buffer "*Messages*"))

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

(defun brew-prefix ()
  (if (eq (shell-command "which brew") 0)
      (string-trim (shell-command-to-string "brew --prefix"))
    (error "Homebrew is not available")))

(defun brew-bin (bin)
  "Given a BIN, generate the path for this bin assuming the homebrew prefix"
  (f-join (brew-prefix) "bin" bin))

;; (defun kdz/sops-extract--command (encrypted-file secret-path)
;;   (let*
;;       ((extract-path (concat "\\['" secret-path "'\\]"))
;;        (sops-args (list "sops" "-d" "--extract" extract-path encrypted-file)))
;;     (string-join sops-args " ")))

;; (defun kdz/sops-extract (encrypted-file secret-path)
;;   "Use SOPS to extract the value a SECRET-PATH from ENCRYPTED-FILE"
;;   (shell-command-to-string
;;    (kdz/sops-extract--command encrypted-file secret-path)))

;; (define-minor-mode kdz/colorized-ansi-output-mode
;;   :lighter " kdz/colorized-ansi-output-mode"
;;   (ansi-color-apply-on-region))
