(map! :leader
      :desc "Command" "SPC" #'counsel-M-x
      :desc "Change Major Mode" "M" #'counsel-major)

(map! :leader
      (:prefix-map ("a" . "Applications")
        (:prefix-map ("o" . "Org")
          :desc "Capture" "c" #'org-capture
          :desc "Store Link" "l" #'org-store-link)
        :desc "Deft" "n" #'deft))

(map! :leader
      (:prefix ("e" . "Errors")
        :desc "Error List" "l" #'flycheck-list-errors
        :desc "Next Error" "n" #'next-error
        :desc "Previous Error" "p" #'previous-error))

(map! :leader
      (:prefix "g"
        :desc "Worktrees" "w" #'magit-worktree
        (:prefix-map ("j" . "(Jump to) Links")
                :desc "Current file" "j" #'git-link
                :desc "Repository page" "r" #'git-link-homepage
                :desc "Current commit" "c" #'git-link-commit)))

(map! :leader
      (:prefix "o"
        :desc "Project Errors" "e" #'lsp-treemacs-errors-list))

(map! :leader
      (:prefix "b"
        :desc "Messages" "m" #'switch-to-message-buffer
        :desc "Copy Buffer" "y" #'copy-whole-buffer))

(evil-ex-define-cmd "W" 'evil-write)
(evil-ex-define-cmd "Wq" 'evil-save-and-close)
(evil-ex-define-cmd "WQ" 'evil-save-and-close)
