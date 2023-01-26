(defun kdz/org-buffer-margins ()
  (setq left-margin-width 2)
  (setq right-margin-width 2))

(after! org
  (add-hook 'org-mode-hook #'org-appear-mode)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-transient-hook! #'org-babel-execute-src-block (require 'ob-async)))
