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

(evil-ex-define-cmd "W" 'evil-write)
(evil-ex-define-cmd "Wq" 'evil-save-and-close)
(evil-ex-define-cmd "WQ" 'evil-save-and-close)
