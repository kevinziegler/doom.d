;; Enabling scrolling via the trackpad
(setq mouse-wheel-tilt-scroll t)
;; Make the direction sane on an apple trackpad
(setq mouse-wheel-flip-direction t)

(map! :leader
      :desc "Command" "SPC" #'execute-extended-command

      (:prefix "b"
       :desc "Messages" "m" #'switch-to-message-buffer
       :desc "New empty ORG buffer" "o" #'evil-buffer-org-new
       :desc "Copy Buffer" "y" #'doom/copy-buffer-contents)

      (:prefix "g"
       :desc "Worktrees" "w" #'magit-worktree)
      (:prefix "i"
       :desc "Insert UUID" "U" #'uuidgen)

      (:prefix "o"
       :desc "Connect to Jupyter Notebook" "s" #'ein:notebooklist-login)

      (:prefix "t"
       :desc "Light/Dark Theme" "t" #'kdz/toggle-light-theme)

      (:prefix "w"
       :desc "Swap Window" "a" #'ace-swap-window)

      (:prefix-map ("y" . "Copy as Format")
       :desc "Copy for Slack" "s" #'copy-as-format-slack
       :desc "Copy for Jira"  "j" #'copy-as-format-jira
       :desc "Copy as HTML"   "h" #'copy-as-format-html)

      (:prefix ("c~" . "naming convention")
       :desc "cycle" "~" #'string-inflection-all-cycle
       :desc "toggle" "t" #'string-inflection-toggle
       :desc "CamelCase" "c" #'string-inflection-camelcase
       :desc "downCase" "d" #'string-inflection-lower-camelcase
       :desc "kebab-case" "k" #'string-inflection-kebab-case
       :desc "under_score" "_" #'string-inflection-underscore
       :desc "Upper_Score" "u" #'string-inflection-capital-underscore
       :desc "UP_CASE" "U" #'string-inflection-upcase))

(map! :prefix "g"
      :desc "Prev page break" :nv "[" #'backward-page
      :desc "Next page break" :nv "]" #'forward-page)

(after! org
  (map! :map org-mode-map
        :localleader
        :desc "Schedule" "S" #'org-schedule)

  (map! :map org-mode-map
        :localleader
        :desc "Outline" "O" #'org-ol-tree
        (:prefix ("s" . "Subtrees")
         :desc "Cut subtree" "d" #'org-cut-subtree
         :desc "Promote subtree" "h" #'org-promote-subtree
         :desc "Demote subtree" "l" #'org-demote-subtree
         :desc "Move Subtree Up" "k" #'org-move-subtree-up
         :desc "Move Subtree Down" "j" #'org-move-subtree-down))

  (map! :map org-mode-map
        :leader
        (:prefix "i"
         :desc "Heading" "h" #'evil-org-org-insert-heading-respect-content-below
         :desc "Parent Heading" "H" #'kdz/org-insert-heading-up
         :desc "Subheading" "s" #'kdz/org-insert-subheading
         :desc "Link" "l" #'org-insert-link)
        (:prefix ("ia" . "Application Links")
         :desc "Active Firefox Tab" "f" #'org-mac-link-firefox-insert-frontmost-url
         :desc "Finder Item" "F" #'org-mac-link-finder-insert-selected)))

(after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))

    (define-key evil-normal-state-map
      (kbd "g~") 'evil-operator-string-inflection)

    (evil-ex-define-cmd "W" 'evil-write)
    (evil-ex-define-cmd "Wq" 'evil-save-and-close)
    (evil-ex-define-cmd "WQ" 'evil-save-and-close)

    (map! :map evil-window-map
          "SPC" #'rotate-layout
          "<left>"     #'evil-window-left
          "<down>"     #'evil-window-down
          "<up>"       #'evil-window-up
          "<right>"    #'evil-window-right
          "C-<left>"   #'+evil/window-move-left
          "C-<down>"   #'+evil/window-move-down
          "C-<up>"     #'+evil/window-move-up
          "C-<right>"  #'+evil/window-move-right))

(after! evil-args
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(after! lsp
  (map! :map lsp-mode-map
        :leader
        (:prefix "c"
         :desc "Show Documentation" "h" #'lsp-ui-doc-show
         :desc "Glance Documentation" "g" #'lsp-ui-doc-glance)

        (:prefix "o"
         :desc "Project Errors" "e" #'kdz/toggle-lsp-errors-list
         :desc "Symbol Browser" "s" #'kdz/toggle-lsp-symbols)

        (:prefix t
         :desc "Toggle LSP Sideline Symbols" "S" #'lsp-ui-sideline-toggle-symbols-info)))

(after! wordel
  (evil-make-intercept-map wordel-mode-map)
  (evil-make-intercept-map wordel-select-mode-map)
  (evil-set-initial-state  'wordel-mode 'insert)
  (evil-set-initial-state  'wordel-select-mode 'insert))
