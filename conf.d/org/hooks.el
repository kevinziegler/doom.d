(defun kdz/org-buffer-margins ()
  (setq left-margin-width 2)
  (setq right-margin-width 2))

(after! org
  (add-hook 'org-mode-hook #'kdz/writing-fill-column)
  (add-hook 'org-mode-hook #'kdz/org-buffer-margins)
  (add-hook 'org-mode-hook #'+org-pretty-mode)
  (add-hook 'org-mode-hook #'org-appear-mode)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-mode-hook (display-fill-column-indicator-mode -1))
  (add-hook 'org-mode-hook #'valign-mode)
  (add-hook 'org-mode-hook (auto-save-mode t))
  (add-transient-hook! #'org-babel-execute-src-block (require 'ob-async)))
