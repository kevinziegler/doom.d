(require 'hydra)
(defhydra hydra-git-timemachine ()
  "Git Time Machine"
  ("b" git-timemachine-blame "Show git blame")
  ("c" git-timemachine-show-commit "Show commit")
  ("p" git-timemachine-show-previous-revision "Previous revision")
  ("n" git-timemachine-show-next-revision "Next revision"))

(add-hook! git-timemachine-mode #'hydra-git-timemachine/body)

(setq magit-git-executable "/usr/local/bin/git"
      magit-repository-directories
      '(("~/dev" . 2) ("~/.dotfiles" . 0)))
