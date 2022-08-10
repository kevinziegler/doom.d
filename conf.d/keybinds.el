(setq doom-leader-alt-key "M-SPC")
(setq doom-localleader-alt-key "M-SPC m")

(defun kdz/toggle-buffer-window (buffer-name activate-fn)
  (let ((window (get-buffer-window buffer-name)))
  (if window
      (delete-window window)
    (funcall activate-fn))))

(defun kdz/toggle-lsp-symbols ()
  (interactive)
  (kdz/toggle-buffer-window
   lsp-treemacs-symbols-buffer-name
   'lsp-treemacs-symbols))

(defun kdz/toggle-lsp-errors-list ()
  (interactive)
  (kdz/toggle-buffer-window
   lsp-treemacs-errors-buffer-name
   'lsp-treemacs-errors-list))

;; Enabling scrolling via the trackpad
(setq mouse-wheel-tilt-scroll t)
;; Make the direction sane on an apple trackpad
(setq mouse-wheel-flip-direction t)

(map! :leader
      :desc "Command" "SPC" #'execute-extended-command
      (:prefix ("e" . "Errors")
        :desc "Error List" "l" #'flycheck-list-errors
        :desc "Next Error" "n" #'next-error
        :desc "Previous Error" "p" #'previous-error)
      (:prefix "o"
        :desc "Project Errors" "e" #'kdz/toggle-lsp-errors-list
        :desc "Symbol Browser" "s" #'kdz/toggle-lsp-symbols
        :desc "Connect to Jupyter Notebook" "s" #'ein:notebooklist-login)
      (:prefix "b"
        :desc "Messages" "m" #'switch-to-message-buffer
        :desc "Copy Buffer" "y" #'doom/copy-buffer-contents)
      (:prefix "c"
       :desc "Show Documentation" "h" #'lsp-ui-doc-show)
      (:prefix "w"
       :desc "Swap Window" "a" #'ace-swap-window)
      (:prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase))

(evil-ex-define-cmd "W" 'evil-write)
(evil-ex-define-cmd "Wq" 'evil-save-and-close)
(evil-ex-define-cmd "WQ" 'evil-save-and-close)

(after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection))
