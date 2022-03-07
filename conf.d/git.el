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
