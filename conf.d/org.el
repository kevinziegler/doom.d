;;; $DOOMDIR/config-org.el -*- lexical-binding: t; -*-

(after! org
  (require 'ox-gfm nil t)
  (require 'org-expiry)

  (global-org-modern-mode)
  (add-hook 'org-mode-hook #'valign-mode)
  (setq doom-themes-org-fontify-special-tags nil
        org-appear-autokeywords t
        org-appear-autolinks t
        org-appear-trigger 'manual
        org-auto-align-tags nil
        org-default-notes-file "~/notes/notes.org"
        org-directory "~/notes/"
        org-ellipsis " ⋯"
        org-fold-catch-invisible-edits 'show-and-error
        org-fontify-quote-and-verse-blocks t
        org-hidden-keywords '(title)
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-insert-heading-respect-content t
        org-list-allow-alphabetical t
        org-modern-hide-stars t
        org-modern-table nil
        org-modern-table-horizontal 3
        org-pretty-entities t
        org-roam-directory "~/notes/roam"
        org-startup-indented nil
        org-use-property-inheritance t
        valign-fancy-bar t

        org-list-demote-modify-bullet '(("+" . "*")
                                        ("-" . "+")
                                        ("*" . "-")
                                        ("1." . "a."))

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

  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'evil-insert-state-entry-hook
                                       #'org-appear-manual-start
                                       nil
                                       t)
                             (add-hook 'evil-insert-state-exit-hook
                                       #'org-appear-manual-stop
                                       nil
                                       t))))

(org-link-set-parameters "gh" :follow (kdz/follow-suffix-link "https://github.com"))
(org-link-set-parameters "gl" :follow (kdz/follow-suffix-link "https://gitlab.com"))
(when hosted-gitlab-host
  (org-link-set-parameters "hgl"
                           :follow (kdz/follow-suffix-link hosted-gitlab-host)))
(when jira-host
  (org-link-set-parameters "jira"
                           :follow (kdz/follow-suffix-link (format "%s/browse"
                                                                   jira-host))))

;; Prevent automatic line wrapping when exporting via Pandoc
(after! 'ox-pandoc (add-to-list 'org-pandoc-options '(wrap . "none")))
