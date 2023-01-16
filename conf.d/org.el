;;; $DOOMDIR/config-org.el -*- lexical-binding: t; -*-

(setq org-directory "~/notes/"
      org-hide-emphasis-markers t
      org-default-notes-file "~/notes/notes.org"
      org-roam-directory "~/notes/roam"
      org-insert-heading-respect-content t
      deft-recursive t
      deft-directory "~/notes")

(after! org
  (require 'ox-gfm nil t)
  (require 'org-expiry)

  (setq org-ellipsis (all-the-icons-material "unfold_more")
        valign-fancy-bar t
        org-hide-leading-stars t
        org-use-property-inheritance t
        ;; Not sure on this one - should check back later and see if it's useful
        org-fold-catch-invisible-edits t
        org-list-allow-alphabetical t
        org-modern-hide-stars t
        org-startup-indented nil
        org-fontify-quote-and-verse-blocks t
        org-appear-autolinks t
        org-appear-autokeywords t
        doom-themes-org-fontify-special-tags nil
        org-list-demote-modify-bullet '(("+" . "-")
                                        ("-" . "+")
                                        ("*" . "-")
                                        ("1." . "a."))
        org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w)
                        ("@personal" . ?p)
                        (:endgroup . nil))
        org-hidden-keywords '(title)
        org-babel-default-header-args '((:session . "none")
                                        (:results . "replace")
                                        (:exports . "code")
                                        (:cache . "no")
                                        (:noweb . "no")
                                        (:hlines . "no")
                                        (:tangle . "no")
                                        (:comments . "link")))

  (advice-add 'doom-modeline-buffer-file-name
              :around
              #'stolen/doom-modeline-buffer-file-name-roam-aware-a)

  (advice-add 'org-babel-get-src-block-info
              :around
              #'stolen/org-babel-get-src-block-info-eager-async-a)

  (global-org-modern-mode)

  ;; Adjust org-appear behavior so that elements only apepar in insert mode
  (setq org-appear-trigger 'manual)
  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'evil-insert-state-entry-hook
                                       #'org-appear-manual-start
                                       nil
                                       t)
                             (add-hook 'evil-insert-state-exit-hook
                                       #'org-appear-manual-stop
                                       nil
                                       t)))
  (after! ox
    (add-to-list 'org-export-filter-final-output-functions
                 #'+org-export-remove-zero-width-space t)))

;; (org-add-link-type "gh" #'kdz/org-github-link)
;; (org-add-link-type "gl" #'kdz/org-gitlab-link)
;; (org-add-link-type "jira" #'kdz/org-jira-link)

;; Smart Parens config for org-mode
(sp-local-pair '(org-mode) "<<" ">>" :actions '(insert))
