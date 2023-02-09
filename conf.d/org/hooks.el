(after! org
  (add-hook 'org-mode-hook #'org-appear-mode)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-transient-hook! #'org-babel-execute-src-block (require 'ob-async)))
