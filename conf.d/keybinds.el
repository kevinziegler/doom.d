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

      (:prefix "c"
       :desc "Show Documentation" "h" #'lsp-ui-doc-show
       :desc "Glance Documentation" "g" #'lsp-ui-doc-glance)

      (:prefix "i"
       :desc "Insert UUID" "U" #'uuidgen)

      (:prefix "o"
       :desc "Project Errors" "e" #'kdz/toggle-lsp-errors-list
       :desc "Symbol Browser" "s" #'kdz/toggle-lsp-symbols
       :desc "Connect to Jupyter Notebook" "s" #'ein:notebooklist-login)

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
