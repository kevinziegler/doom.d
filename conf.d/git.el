(require 'hydra)
(require 'f)

(defhydra hydra-git-timemachine ()
  "Git Time Machine"
  ("b" git-timemachine-blame "Show git blame")
  ("c" git-timemachine-show-commit "Show commit")
  ("p" git-timemachine-show-previous-revision "Previous revision")
  ("n" git-timemachine-show-next-revision "Next revision"))

(map! :leader
      (:prefix "g"
        :desc "Worktrees" "w" #'magit-worktree))

(add-hook! git-timemachine-mode #'hydra-git-timemachine/body)
(after! magit (magit-org-todos-autoinsert))

(setq magit-git-executable (brew-bin "git")
      magit-repository-directories '(("~/dev" . 2) ("~/.dotfiles" . 0)))

