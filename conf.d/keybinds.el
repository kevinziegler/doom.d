(setq doom-leader-alt-key "M-SPC")
(setq doom-localleader-alt-key "M-SPC m")

(map! :leader
      :desc "Command" "SPC" #'execute-extended-command
      :desc "Change Major Mode" "M" #'counsel-major)

(map! :leader
      (:prefix ("e" . "Errors")
        :desc "Error List" "l" #'flycheck-list-errors
        :desc "Next Error" "n" #'next-error
        :desc "Previous Error" "p" #'previous-error))

(map! :leader
      (:prefix "o"
        :desc "Project Errors" "e" #'lsp-treemacs-errors-list
        :desc "Symbol Browser" "s" #'lsp-treemacs-symbols))

(map! :leader
      (:prefix "b"
        :desc "Messages" "m" #'switch-to-message-buffer
        :desc "Copy Buffer" "y" #'doom/copy-buffer-contents))

(map! :leader
      (:prefix "c"
       :desc "Show Documentation" "h" #'lsp-ui-doc-show))

(evil-ex-define-cmd "W" 'evil-write)
(evil-ex-define-cmd "Wq" 'evil-save-and-close)
(evil-ex-define-cmd "WQ" 'evil-save-and-close)

(map! :leader
      (:prefix "w"
       :desc "Swap Window" "a" #'ace-swap-window))

;; String inflection - extra keymaps
(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)

  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))
