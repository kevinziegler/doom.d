;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! page-break-lines)
(use-package! org-modern)
(use-package! org-autolist)
(use-package! chatgpt-shell)

(use-package jinx :hook (emacs-startup . global-jinx-mode))

(mapc (lambda (lib-file) (load! (concat "lib/" lib-file)))
      (directory-files (expand-file-name "lib" doom-user-dir) nil "\\.el$"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 13)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13)
;;       doom-big-font (font-spec :family "Fira Sans" :size 16))
;;
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-big-font (kdz/find-and-use-font kdz--mono-sans-font-list 14)
      doom-font (kdz/find-and-use-font kdz--mono-sans-font-list 13)
      doom-serif-font (kdz/find-and-use-font kdz--mono-serif-font-list 13)
      doom-unicode-font (kdz/find-and-use-font kdz--unicode-font-list)
      doom-variable-pitch-font (kdz/find-and-use-font kdz--variable-pitch-font-list 13))

;; Set variables native to emacs
(setq user-full-name "Kevin Ziegler"
      auto-save-default t
      bookmark-version-control t
      company-show-quick-access  t
      delete-by-moving-to-trash t
      display-line-numbers-type t
      doom-themes-treemacs-theme 'kaolin
      emojify-emoji-set "twemoji-v2"
      enable-local-variables t
      fancy-splash-image (kdz/config-resource "modern-sexy-v2_128.png")
      global-subword-mode 1
      ispell-personal-dictionary (kdz/config-resource "ispell_personal")
      mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t
      native-comp-async-report-warnings-errors nil
      page-break-lines-max-width fill-column
      password-cache-expiry nil
      scroll-margin 2
      truncate-string-ellipsis "…"
      undo-limit 80000000
      uniquify-buffer-name-style 'post-forward
      window-combination-resize t
      x-stretch-cursor t)

(setq-default history-length 1000
              prescient-history-length 1000)

;; Set default frame properties
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(internal-border-width . 8))

;; Customize popup rules
(set-popup-rules!
  '(("^\\*format-all-errors\\*" :height 5 :select nil :modeline nil)
    ("^\\*Capture\\*$\\|CAPTURE-.*$" :height 0.5 :select t :quit nil)
    ("\\*Messages\\*" :height 0.3 :quit nil)
    ("\\*Compile-Log\\*" :ttl 0.01 :quit t)))

(set-popup-rule! "^\\*helpful .+: .+\\*"
  :actions '(kdz/popup-display-buffer-side-by-size)
  :select t
  :height 0.5
  :width 0.5
  :quit nil
  :modeline t)

(ace-window-posframe-mode)

;; Configurations to run after various packages load

;; (after! consult
;;   (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))

(after! ace-window
  (set-face-attribute 'aw-leading-char-face nil :height 3.0))

(after! doom-modeline
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (setq doom-modeline-percent-position nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-display-default-persp-name t
        doom-modeline-hud t
        doom-modeline-persp-name t))

(after! display-fill-column-indicator
  (add-to-list 'global-display-fill-column-indicator-modes '(not org-mode))
  (add-to-list 'global-display-fill-column-indicator-modes '(not markdown-mode))
  (global-display-fill-column-indicator-mode))

(after! evil
  (setq evil-kill-on-visual-paste nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-fine-undo t))

(after! evil-goggles
  (setq evil-goggles-duration 1.0
        evil-goggles-pulse t))

(after! git-link
  (advice-add #'git-link--branch :after-until #'kdz/git-link--tag))

(after! hydra
  (defhydra hydra-git-timemachine ()
    "Git Time Machine"
    ("b" git-timemachine-blame "Show git blame")
    ("c" git-timemachine-show-commit "Show commit")
    ("p" git-timemachine-show-previous-revision "Previous revision")
    ("n" git-timemachine-show-next-revision "Next revision"))

  (add-hook! git-timemachine-mode #'hydra-git-timemachine/body))

(after! info-colors
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(after! kaolin-themes
  (setq kaolin-themes-bold t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline-wave nil))

(after! magit
  (magit-org-todos-autoinsert)
  (magit-delta-mode +1)

  (setq magit-git-executable (brew-bin "git")
        magit-repository-directories '(("~/dev" . 2) ("~/.dotfiles" . 0))))

(after! marginalia (setq marginalia-align 'right))

(after! markdown-mode
  (setq markdown-header-scaling t
        markdown-fontify-code-blocks-natively t)
  (after! mixed-pitch (add-hook 'markdown-mode-hook #'kdz/writing-minor-modes))
  (add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill))

(after! markdown-xwidget
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default"))

(after! modern-fringes (modern-fringes-mode t))

(after! olivetti
  (setq olivetti-body-width 122))

(after! persp-mode
  ;; Set up dedicated workspaces for Org-mode notes and Doom documentation
  (kdz/doom-run-in-workspace "*Notes*" #'+default/find-in-notes)
  (kdz/doom-run-in-workspace "*Doom Documentation*" #'doom/help-modules))

(after! pixel-scroll (pixel-scroll-precision-mode))

(after! plantuml-mode (setq plantuml-default-exec-mode 'executable))

(after! projectile
  (setq projectile-ignored-projects '("~/"
                                      "/tmp"
                                      "~/.emacs.d/.local/straight/repos/")
        projectile-project-search-path '(("~/.doom.d" . 0)
                                         ("~/.dotfiles" . 0)
                                         ("~/dev" . 5))))

(after! python (set-ligatures! 'python-mode nil))

(after! treemacs
  (kaolin-treemacs-theme)

  (setq +treemacs-git-mode 'deferred
        treemacs-collapse-dirs 7
        treemacs-width 45
        treemacs-follow-mode t
        treemacs-project-follow-mode t
        treemacs-recenter-after-file-follow t
        treemacs-project-follow-cleanup t)

  (treemacs-modify-theme "kaolin"
    :config
    (progn
      (kdz/treemacs-all-the-icons (package) "material" "archive")
      (kdz/treemacs-all-the-icons (namespace) "material" "widgets" :valign 0.9)
      (kdz/treemacs-all-the-icons (class) "material" "class" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons (function) "material" "code" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons (method) "faicon" "code" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons (enum) "faicon" "tags" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons (enumerator) "faicon" "tags" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons (enum-item) "faicon" "tag" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons (enum-member) "faicon" "tag" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons (property) "material" "info_outline" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons (constant) "material" "info_outline" :face 'font-lock-keyword-face)
      (kdz/treemacs-all-the-icons ("json") "fileicon" "jsonld")
      (kdz/treemacs-all-the-icons ("csv") "faicon" "table")
      (kdz/treemacs-all-the-icons ("editorconfig" "envrc" "envrc.local") "faicon" "table"))))

(after! vertico
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp)
        vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))
        vertico-posframe-poshandler 'kdz/posframe-offset-top)

  (advice-add #'vertico--format-candidate
              :around #'kdz/vertico--format-candiate-marker-advice))

(after! vterm (setq vterm-shell (brew-bin "zsh")))

(after! which-key
  (setq which-key-ellipsis "»"
        which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05
        which-key-prefix-prefix "⬮ "
        which-key-sort-order #'which-key-prefix-then-key-order))

(after! which-key-posframe
  (setq which-key-posframe-poshandler 'kdz/posframe-offset-bottom)
  (advice-add #'which-key-posframe--show-buffer
              :override
              #'kdz/fixup--which-key-posframe--show-buffer)
  (which-key-posframe-mode 1))


;; Explicitly specify modes for certain file types
(add-to-list 'auto-mode-alist '("\\.puml$" . plantuml-mode))
(add-to-list 'auto-mode-alist '("/Tiltfile.*\\'" . bazel-starlark-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

(load! "conf.d/local" nil t)
(load! "conf.d/keybinds")
(load! "conf.d/keybinds-removed")
(load! "conf.d/org")
(load! "conf.d/org/capture")
(load! "conf.d/org/ob-async-advice")

;; (defun my-minibuffer-setup-hook ()
;;   (buffer-face-set 'variable-pitch)
;;   ; NOTE The leading space character is correct in the buffer name below
;;   (with-current-buffer (get-buffer " *Echo Area 0*")
;;     (setq-local face-remapping-alist '((default variable-pitch)))))

;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
