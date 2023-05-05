;;; $DOOMDIR/config-org.el -*- lexical-binding: t; -*-

(after! org
  (require 'org-expiry)

  (global-org-modern-mode)
  (setq doom-themes-org-fontify-special-tags nil
        org-appear-autokeywords t
        org-appear-autolinks t
        org-appear-trigger 'manual
        org-auto-align-tags nil
        org-babel-results-keyword "results"
        org-directory "~/notes/"
        org-ellipsis " ⋯"
        org-fold-catch-invisible-edits 'show-and-error
        org-fontify-quote-and-verse-blocks t
        org-hidden-keywords '(title)
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-html-html5-fancy t
        org-html-doctype "html5"
        org-insert-heading-respect-content t
        org-list-allow-alphabetical t
        org-modern-hide-stars t
        org-modern-table nil
        org-modern-table-horizontal 3
        org-plantuml-exec-mode 'executable
        org-pretty-entities t
        org-re-reveal-subtree-with-title-slide t
        org-re-reveal-transition "slide"
        org-re-reveal-title-slide "<h1>%t</h1><p>%a | %d</p>"
        org-re-reveal-plugins '(highlight markdown notes search zoom)
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

  (setf (alist-get 'height +org-capture-frame-parameters) 15)
  (setf (alist-get 'name +org-capture-frame-parameters) "❖ Capture")

  (setq +org-capture-fn #'kdz/org-capture-no-modeline)
  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (add-hook 'org-mode-hook #'kdz/writing-minor-modes)
  (add-hook 'org-mode-hook #'valign-mode)
  (add-hook 'org-mode-hook #'org-appear-mode)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-mode-hook #'org-autolist-mode)
  (add-hook 'org-mode-hook #'(kdz/org-appear-hook-evil-state "insert-state"))
  (add-hook 'org-mode-hook #'(kdz/org-appear-hook-evil-state "visual-state"))
  (add-hook 'org-mode-hook #'(kdz/org-appear-hook-evil-state "motion-state"))


  (advice-add 'org-babel-variable-assignments:plantuml
              :override #'kdz/org-babel-variable-assignments:plantuml)
  (advice-add 'org-babel-plantuml-make-body
              :override #'kdz/org-babel-plantuml-make-body)
  (advice-add 'org-mks
              :override #'org-mks-pretty)
  (advice-add 'org-capture-select-template
              :override #'org-capture-select-template-prettier)

  (org-link-set-parameters "gh" :follow (kdz/follow-suffix-link "https://github.com"))
  (org-link-set-parameters "gl" :follow (kdz/follow-suffix-link "https://gitlab.com"))

  (after! ox
    (add-to-list 'org-export-filter-link-functions
                 #'kdz/ox-filter-git-file-link))

  (after! ox-pandoc (add-to-list 'org-pandoc-options '(wrap . "none")))
  (when hosted-gitlab-host
    (org-link-set-parameters "hgl"
                             :follow (kdz/follow-suffix-link hosted-gitlab-host)))
  (when jira-host
    (org-link-set-parameters "jira"
                             :follow (kdz/follow-suffix-link (format "%s/browse"
                                                                     jira-host)))))
