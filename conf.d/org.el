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

  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'prepend)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (custom-set-faces!
    '(outline-1 :weight extra-bold :height 1.50 :underline t)
    '(outline-2 :weight bold :height 1.40)
    '(outline-3 :weight bold :height 1.30)
    '(outline-4 :weight semi-bold :height 1.20)
    '(outline-5 :weight semi-bold :height 1.15)
    '(outline-6 :weight semi-bold :height 1.15)
    '(outline-8 :weight semi-bold)
    '(outline-9 :weight semi-bold)
    '(org-document-title :height 1.9))

  (add-transient-hook! #'org-babel-execute-src-block (require 'ob-async))

  (add-hook 'org-mode-hook #'org-appear-mode)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (add-hook 'evil-insert-state-entry-hook
                                       #'org-appear-manual-start
                                       nil
                                       t)
                             (add-hook 'evil-insert-state-exit-hook
                                       #'org-appear-manual-stop
                                       nil
                                       t))))

  (appendq! +ligatures-extra-symbols
            `(:checkbox      ""
              :pending       ""
              :checkedbox    ""
              :created       ""
              :closed        ""
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :property      ""
              :header        "›"
              :properties    ""
              :end           ""
              :scheduled     ""
              :deadline      ""))

  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :created       ":created:"
    :closed        "CLOSED:"
    :closed        "closed:"
    :properties    ":PROPERTIES:"
    :properties    ":properties:"
    :end           ":END:"
    :end           ":end:"
    :filetags      "#+filetags:"
    :scheduled     "SCHEDULED:"
    :scheduled     "scheduled:"
    :deadline      "DEADLINE:"
    :deadline      "deadline:")
  (plist-put +ligatures-extra-symbols :name "⁍")

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

(advice-add 'org-capture-select-template
            :override #'org-capture-select-template-prettier)

(advice-add 'org-mks :override #'org-mks-pretty)

(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(setq doct-after-conversion-functions '(+doct-iconify-capture-templates))
