;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kevin Ziegler"
      projectile-project-search-path '("~/dev")
      delete-by-moving-to-trash t
      window-combination-resize t
      x-stretch-cursor t
      scroll-margin 2
      auto-save-default t
      truncate-string-ellipsis "…"
      password-cache-expiry nil
      global-subword-mode 1
      undo-limit 80000000
      enable-local-variables t
      company-show-quick-access  t ;; Show numbers by completions; accessible via M-<number>
      which-key-idle-delay 0.1 ;; Reduce time to show which-key popup
      evil-want-fine-undo t
      evil-kill-on-visual-paste nil
      evil-vsplit-window-right t
      evil-split-window-below t)

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(load! "funcs")
(load! "conf.d/lsp")
;; (load! "conf.d/lsp-java")
(load! "conf.d/git")
(load! "conf.d/heb")
(load! "conf.d/keybinds")
(load! "conf.d/org")
(load! "conf.d/ui")
(load! "conf.d/treemacs")
(load! "conf.d/smerge")
(load! "conf.d/local" nil t)

;; Open TSX files in Typescript TSX Mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

;; Open `.zshrc` in shell-script-mode with the ZSH dialect
(add-hook 'sh-mode-hook #'kdz/set-zshrc-sh-shell)

;; Use system-installed plantuml executable
(after! plantuml-mode (setq plantuml-default-exec-mode 'executable))

;; Set ordering styles for vertico
(after! vertico
  (setq orderless-matching-styles '(orderless-prefixes
                                    orderless-flex
                                    orderless-regexp)))


;; Use Ispell for completion in text/markdown modes
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

;; Show numbers by completions; accessible via M-<number>
(setq company-show-quick-access  t)

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))

(setq ispell-dictionary "en-custom")
(setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))

(use-package! etrace
  :after elp)

;; String inflection - extra keymaps
(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)))

;; Smart Parens config for org-mode
(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(setq emojify-emoji-set "twemoji-v2")

(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀")
  "Characters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))

(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))

;; Show ANSI color codes in text-mode
;; TODO See how this plays with magit process buffers?
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max) t))))

(after! org
  ;; TODO Check these against existing org-config
  (use-package! org-ol-tree :commands org-ol-tree)
  (map! :map org-mode-map
        :after org
        :localleader
        :desc "Outline" "O" #'org-ol-tree)

  (use-package! ob-http :commands org-babel-execute:http)
  (setq org-use-property-inheritance t)
  (setq org-list-allow-alphabetical t)

  ;; Not sure on this one - should check back later and see if it's useful
  (setq org-catch-invisible-edits t)

  (setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))

  (evil-define-command evil-buffer-org-new (count file)
    "Creates a new ORG buffer replacing the current window, optionally
     editing a certain FILE"
    :repeat nil
    (interactive "P<f>")
    (if file
        (evil-edit file)
      (let ((buffer (generate-new-buffer "*new org*")))
        (set-window-buffer nil buffer)
        (with-current-buffer buffer
          (org-mode)))))
  (map! :leader
        (:prefix "b"
         :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))


  (map! :map org-mode-map
        :nie "M-SPC M-SPC" (cmd! (insert "\u200B")))



  (defun +org-export-remove-zero-width-space (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string "\u200B" "" text)))

  (after! ox
    (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-zero-width-space t))

  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

  (defun unpackaged/org-element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
  TYPE should be an element type, like `item' or `paragraph'.
  ELEMENT should be a list like that returned by `org-element-context'."
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (unpackaged/org-element-descendant-of type parent))))

  ;;;###autoload
  (defun unpackaged/org-return-dwim (&optional default)
    "A helpful replacement for `org-return-indent'.  With prefix, call `org-return-indent'.

  On headings, move point to position after entry content.  In
  lists, insert a new item or end the list, with checkbox if
  appropriate.  In tables, insert a new row or end the table."
    ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
    (interactive "P")
    (if default
        (org-return t)
      (cond
       ;; Act depending on context around point.

       ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
       ;; followed.

       ;; ((eq 'link (car (org-element-context)))
       ;;  ;; Link: Open it.
       ;;  (org-open-at-point-global))

       ((org-at-heading-p)
        ;; Heading: Move to position after entry content.
        ;; NOTE: This is probably the most interesting feature of this function.
        (let ((heading-start (org-entry-beginning-position)))
          (goto-char (org-entry-end-position))
          (cond ((and (org-at-heading-p)
                      (= heading-start (org-entry-beginning-position)))
                 ;; Entry ends on its heading; add newline after
                 (end-of-line)
                 (insert "\n\n"))
                (t
                 ;; Entry ends after its heading; back up
                 (forward-line -1)
                 (end-of-line)
                 (when (org-at-heading-p)
                   ;; At the same heading
                   (forward-line)
                   (insert "\n")
                   (forward-line -1))
                 (while (not (looking-back "\\(?:[[:blank:]]?\n\\)\\{3\\}" nil))
                   (insert "\n"))
                 (forward-line -1)))))

       ((org-at-item-checkbox-p)
        ;; Checkbox: Insert new item with checkbox.
        (org-insert-todo-heading nil))

       ((org-in-item-p)
        ;; Plain list.  Yes, this gets a little complicated...
        (let ((context (org-element-context)))
          (if (or (eq 'plain-list (car context))  ; First item in list
                  (and (eq 'item (car context))
                       (not (eq (org-element-property :contents-begin context)
                                (org-element-property :contents-end context))))
                  (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
              ;; Non-empty item: Add new item.
              (org-insert-item)
            ;; Empty item: Close the list.
            ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
            (delete-region (line-beginning-position) (line-end-position))
            (insert "\n"))))

       ((when (fboundp 'org-inlinetask-in-task-p)
          (org-inlinetask-in-task-p))
        ;; Inline task: Don't insert a new heading.
        (org-return t))

       ((org-at-table-p)
        (cond ((save-excursion
                 (beginning-of-line)
                 ;; See `org-table-next-field'.
                 (cl-loop with end = (line-end-position)
                          for cell = (org-element-table-cell-parser)
                          always (equal (org-element-property :contents-begin cell)
                                        (org-element-property :contents-end cell))
                          while (re-search-forward "|" end t)))
               ;; Empty row: end the table.
               (delete-region (line-beginning-position) (line-end-position))
               (org-return t))
              (t
               ;; Non-empty row: call `org-return-indent'.
               (org-return t))))
       (t
        ;; All other cases: call `org-return-indent'.
        (org-return t)))))

  (map!
   :after evil-org
   :map evil-org-mode-map
   :i [return] #'unpackaged/org-return-dwim)

  (defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
    :around #'org-fancy-priorities-mode
    :around #'org-superstar-mode
    (ignore-errors (apply orig-fn args)))

  (add-hook 'org-mode-hook #'+org-pretty-mode)

  (custom-set-faces!
    '(outline-1 :weight extra-bold :height 1.25)
    '(outline-2 :weight bold :height 1.15)
    '(outline-3 :weight bold :height 1.12)
    '(outline-4 :weight semi-bold :height 1.09)
    '(outline-5 :weight semi-bold :height 1.06)
    '(outline-6 :weight semi-bold :height 1.03)
    '(outline-8 :weight semi-bold)
    '(outline-9 :weight semi-bold))

  (custom-set-faces!
    '(org-document-title :height 1.2))

  (setq org-fontify-quote-and-verse-blocks t)

  (defun locally-defer-font-lock ()
    "Set jit-lock defer and stealth, when buffer is over a certain size."
    (when (> (buffer-size) 50000)
      (setq-local jit-lock-defer-time 0.05
                  jit-lock-stealth-time 1)))

    (defvar org-prettify-inline-results t
    "Whether to use (ab)use prettify-symbols-mode on {{{results(...)}}}.
  Either t or a cons cell of strings which are used as substitutions
  for the start and end of inline results, respectively.")

  (defvar org-fontify-inline-src-blocks-max-length 200
    "Maximum content length of an inline src block that will be fontified.")

  (defun org-fontify-inline-src-blocks (limit)
    "Try to apply `org-fontify-inline-src-blocks-1'."
    (condition-case nil
        (org-fontify-inline-src-blocks-1 limit)
      (error (message "Org mode fontification error in %S at %d"
                      (current-buffer)
                      (line-number-at-pos)))))

  (defun org-fontify-inline-src-blocks-1 (limit)
    "Fontify inline src_LANG blocks, from `point' up to LIMIT."
    (let ((case-fold-search t)
          (initial-point (point)))
      (while (re-search-forward "\\_<src_\\([^ \t\n[{]+\\)[{[]?" limit t) ; stolen from `org-element-inline-src-block-parser'
        (let ((beg (match-beginning 0))
              pt
              (lang-beg (match-beginning 1))
              (lang-end (match-end 1)))
          (remove-text-properties beg lang-end '(face nil))
          (font-lock-append-text-property lang-beg lang-end 'face 'org-meta-line)
          (font-lock-append-text-property beg lang-beg 'face 'shadow)
          (font-lock-append-text-property beg lang-end 'face 'org-block)
          (setq pt (goto-char lang-end))
          ;; `org-element--parse-paired-brackets' doesn't take a limit, so to
          ;; prevent it searching the entire rest of the buffer we temporarily
          ;; narrow the active region.
          (save-restriction
            (narrow-to-region beg (min (point-max) limit (+ lang-end org-fontify-inline-src-blocks-max-length)))
            (when (ignore-errors (org-element--parse-paired-brackets ?\[))
              (remove-text-properties pt (point) '(face nil))
              (font-lock-append-text-property pt (point) 'face 'org-block)
              (setq pt (point)))
            (when (ignore-errors (org-element--parse-paired-brackets ?\{))
              (remove-text-properties pt (point) '(face nil))
              (font-lock-append-text-property pt (1+ pt) 'face '(org-block shadow))
              (unless (= (1+ pt) (1- (point)))
                (if org-src-fontify-natively
                    (org-src-font-lock-fontify-block (buffer-substring-no-properties lang-beg lang-end) (1+ pt) (1- (point)))
                  (font-lock-append-text-property (1+ pt) (1- (point)) 'face 'org-block)))
              (font-lock-append-text-property (1- (point)) (point) 'face '(org-block shadow))
              (setq pt (point))))
          (when (and org-prettify-inline-results (re-search-forward "\\= {{{results(" limit t))
            (font-lock-append-text-property pt (1+ pt) 'face 'org-block)
            (goto-char pt))))
      (when org-prettify-inline-results
        (goto-char initial-point)
        (org-fontify-inline-src-results limit))))

  (defun org-fontify-inline-src-results (limit)
    (while (re-search-forward "{{{results(\\(.+?\\))}}}" limit t)
      (remove-list-of-text-properties (match-beginning 0) (point)
                                      '(composition
                                        prettify-symbols-start
                                        prettify-symbols-end))
      (font-lock-append-text-property (match-beginning 0) (match-end 0) 'face 'org-block)
      (let ((start (match-beginning 0)) (end (match-beginning 1)))
        (with-silent-modifications
          (compose-region start end (if (eq org-prettify-inline-results t) "⟨" (car org-prettify-inline-results)))
          (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))
      (let ((start (match-end 1)) (end (point)))
        (with-silent-modifications
          (compose-region start end (if (eq org-prettify-inline-results t) "⟩" (cdr org-prettify-inline-results)))
          (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))))

  (defun org-fontify-inline-src-blocks-enable ()
    "Add inline src fontification to font-lock in Org.
  Must be run as part of `org-font-lock-set-keywords-hook'."
    (setq org-font-lock-extra-keywords
          (append org-font-lock-extra-keywords '((org-fontify-inline-src-blocks)))))

  (add-hook 'org-font-lock-set-keywords-hook #'org-fontify-inline-src-blocks-enable)
  (setq doom-themes-org-fontify-special-tags nil)
  (after! org-superstar
    (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-superstar-prettify-item-bullets t ))

    (setq org-ellipsis " ▾ "
        org-hide-leading-stars t
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue)))
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :title         "𝙏"
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :property      "☸"
              :options       "⌥"
              :startup       "⏻"
              :macro         "𝓜"
              :html_head     "🅷"
              :html          "🅗"
              :latex_class   "🄻"
              :latex_header  "🅻"
              :beamer_header "🅑"
              :latex         "🅛"
              :attr_latex    "🄛"
              :attr_html     "🄗"
              :attr_org      "⒪"
              :begin_quote   "❝"
              :end_quote     "❞"
              :caption       "☰"
              :header        "›"
              :results       "🠶"
              :begin_export  "⏩"
              :end_export    "⏪"
              :properties    "⚙"
              :end           "∎"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
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
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :startup       "#+startup:"
    :macro         "#+macro:"
    :html_head     "#+html_head:"
    :html          "#+html:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :latex         "#+latex:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_html:"
    :attr_org      "#+attr_org:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]")
  (plist-put +ligatures-extra-symbols :name "⁍")

  (use-package! ox-gfm :after ox)
  (add-transient-hook! #'org-babel-execute-src-block
    (require 'ob-async))

  ;; Improve org-roam buffer names in modelike
  (defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
    :around #'doom-modeline-buffer-file-name ; takes no args
    (if (s-contains-p org-roam-directory (or buffer-file-name ""))
        (replace-regexp-in-string
         "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
         "🢔(\\1-\\2-\\3) "
         (subst-char-in-string ?_ ?  buffer-file-name))
      (funcall orig-fun)))


  (use-package! websocket
    :after org-roam)

  (use-package! org-roam-ui
    :after org-roam
    :commands org-roam-ui-open
    :hook (org-roam . org-roam-ui-mode)
    :config
    (require 'org-roam) ; in case autoloaded
    (defun org-roam-ui-open ()
      "Ensure the server is active, then open the roam graph."
      (interactive)
      (unless org-roam-ui-mode (org-roam-ui-mode 1))
      (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))


  (defvar org-babel-auto-async-languages '()
    "Babel languages which should be executed asyncronously by default.")

  (defadvice! org-babel-get-src-block-info-eager-async-a (orig-fn &optional light datum)
    "Eagarly add an :async parameter to the src information, unless it seems problematic.
  This only acts o languages in `org-babel-auto-async-languages'.
  Not added when either:
  + session is not \"none\"
  + :sync is set"
    :around #'org-babel-get-src-block-info
    (let ((result (funcall orig-fn light datum)))
      (when (and (string= "none" (cdr (assoc :session (caddr result))))
                 (member (car result) org-babel-auto-async-languages)
                 (not (assoc :async (caddr result))) ; don't duplicate
                 (not (assoc :sync (caddr result))))
        (push '(:async) (caddr result)))
      result))) ;; NOTE End of hijacked ORG config blob

(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)

(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))

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

(require 'hydra)

(use-package smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))
