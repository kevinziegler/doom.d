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
;;(load! "conf.d/bookmarks")
(load! "conf.d/emoji")
(load! "conf.d/lsp")
;; (load! "conf.d/lsp-java")
(load! "conf.d/keybinds")
(load! "conf.d/git")
(load! "conf.d/markdown")
(load! "conf.d/org")
(load! "conf.d/org/capture-templates")
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

;; Don't use special ligatures in python mode
(after! python (set-ligatures! 'python-mode nil))

;; Set ordering styles for vertico
(after! vertico
  (setq orderless-matching-styles '(orderless-prefixes
                                    orderless-regexp)))

;; Use Ispell for completion in text/markdown modes
;; (set-company-backend!
;;   '(text-mode
;;     markdown-mode
;;     gfm-mode)
;;   '(:seperate
;;     company-ispell
;;     company-files
;;     company-yasnippet))

(setq-default history-length 1000
              prescient-history-length 1000)

(setq bookmark-version-control t)

(setq projectile-ignored-projects
      '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))

(setq ispell-dictionary "en"
      ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))

(use-package! etrace :after elp)

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

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

;; (bookmark-load (concat doom-private-dir "bookmarks"))

(after! org
  ;; TODO Check these against existing org-config

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
    '(org-document-title :height 1.5))

  (appendq! +ligatures-extra-symbols
            `(:checkbox      ""
              :pending       ""
              :checkedbox    ""
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :title         ""
              :subtitle      ""
              :author        ""
              :date          ""
              :property      ""
              :options       "⌥"
              :startup       "⏻"
              :macro         ""
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
              :results       ""
              :begin_export  ""
              :end_export    ""
              :properties    ""
              :end           ""
              :begin_src     ""
              :end_src       ""
              :priority_a   ,(propertize "  " 'face 'all-the-icons-red)
              :priority_b   ,(propertize "" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize " ■ " 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "  " 'face 'all-the-icons-green)
              :priority_e   ,(propertize "  " 'face 'all-the-icons-blue)))
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
    :begin_src     "#+begin_src"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]")
  (plist-put +ligatures-extra-symbols :name "⁍"))
  ;; NOTE End of hijacked ORG config blob
