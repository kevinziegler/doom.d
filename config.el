;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! page-break-lines)
(use-package! which-key-posframe)
(use-package! org-modern)
(use-package! org-autolist)

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
(setq doom-big-font (font-spec :family "Iosevka Comfy" :size 14)
      doom-font (font-spec :family "Iosevka Comfy" :size 13)
      doom-serif-font (font-spec :family "Iosevka Comfy Motion" :size 13)
      doom-unicode-font (font-spec :family "Iosevka Term")
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 13))

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
      ispell-dictionary "en"
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
(kdz/add-all-to-list 'default-frame-alist
                     '(height . 60)
                     '(width . 235)
                     '(undecorated-round . t)
                     '(internal-border-width . 10))

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

;; Activate pixel-precision scrolling
(pixel-scroll-precision-mode)

;; Configurations to run after various packages load

;; (after! consult
;;   (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))

(after! doom-modeline
  (setq doom-modeline-percent-position nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-display-default-persp-name t
        doom-modeline-hud t
        doom-modeline-persp-name t))

(after! evil
  (setq evil-kill-on-visual-paste nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-fine-undo t))

(after! evil-goggles
  (setq evil-goggles-duration 1.0
        evil-goggles-pulse t))

(after! hydra
  (defhydra hydra-git-timemachine ()
    "Git Time Machine"
    ("b" git-timemachine-blame "Show git blame")
    ("c" git-timemachine-show-commit "Show commit")
    ("p" git-timemachine-show-previous-revision "Previous revision")
    ("n" git-timemachine-show-next-revision "Next revision"))

  (add-hook! git-timemachine-mode #'hydra-git-timemachine/body))

(after! kaolin-themes
  (setq kaolin-themes-bold t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline-wave nil))

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-pyright-multi-root nil
        lsp-idle-delay 0.8)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\__pycache__\\'"))

(after! lsp-java
  (setq lsp-java-maven-download-sources t
        lsp-java-import-maven-enabled t)

  ;; NOTE Supply a lombok version to add the appropriate JVM args
  (kdz/lsp-java-enable-lombok-support nil))

(after! lsp-pyright
  (advice-add 'lsp-pyright-locate-python :around #'kdz/lsp-pyright-path-advice))

(after! magit
  (magit-org-todos-autoinsert)
  (magit-delta-mode +1)

  (setq magit-git-executable (brew-bin "git")
        magit-repository-directories '(("~/dev" . 2) ("~/.dotfiles" . 0))))

(after! marginalia (setq marginalia-align 'right))

(after! markdown-mode
  (setq markdown-header-scaling t
        markdown-fontify-code-blocks-natively t))

(after! markdown-xwidget
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default"))

(after! modern-fringes (modern-fringes-mode t))

(after! persp-mode
  ;; Set up dedicated workspaces for Org-mode notes and Doom documentation
  (kdz/doom-run-in-workspace "*Notes*" #'+default/find-in-notes)
  (kdz/doom-run-in-workspace "*Doom Documentation*" #'+doom/help-modules)

  ;; Pin workspaces to the front of our workspace list
  (advice-add 'persp-add-to-menu :after #'kdz/pin-workspaces))


(after! plantuml-mode (setq plantuml-default-exec-mode 'executable))

(after! projectile
  (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/")
        projectile-project-search-path '("~/dev")))

(after! python (set-ligatures! 'python-mode nil))

(after! treemacs
  (kaolin-treemacs-theme)

  (setq +treemacs-git-mode 'deferred
        lsp-treemacs-theme 'kaolin
        treemacs-collapse-dirs 7
        treemacs-width 45
        treemacs-follow-mode t
        treemacs-project-follow-mode t
        treemacs-recenter-after-file-follow t
        treemacs-project-follow-cleanup t)

  ;; Doom's +treemacs/toggle function uses delete-window to remove the treemacs
  ;; window, which doesn't call the quit hooks.  To resize the frame when using
  ;; the 'SPC o p' toggle we have to advise this function to trigger the resize
  ;; explicitly.
  (advice-add '+treemacs/toggle :around #'kdz/treemacs-toggle-resize-advice)

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
      (kdz/treemacs-all-the-icons ("editorconfig" "envrc" "envrc.local") "faicon" "table")))

  (add-hook 'treemacs-select-functions #'kdz/treemacs-init-grow-frame)
  (add-hook 'treemacs-quit-hook #'kdz/treemacs-quit-shrink-frame))

(after! vertico
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp)
        vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))
        vertico-posframe-poshandler (kdz/posframe-interior-bottom 100))
  (advice-add #'vertico--format-candidate
              :around
              #'kdz/vertico--format-candiate-marker-advice))

(after! vterm (setq vterm-shell (brew-bin "zsh")))

(after! which-key
  (setq which-key-ellipsis "»"
        which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05))

;; Explicitly specify modes for certain file types
(kdz/add-all-to-list 'auto-mode-alist
                     '("/Tiltfile.*\\'" . bazel-starlark-mode)
                     '("\\.tsx\\'" . typescript-tsx-mode)
                     '("\\.jq$" . jq-mode))

;; Apply hooks for various modes
(add-hook 'markdown-mode-hook #'kdz/writing-minor-modes)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook! 'size-indication-mode (setq size-indication-mode nil))
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)

(load! "conf.d/keybinds")
(load! "conf.d/org")
(load! "conf.d/org/capture")
(load! "conf.d/smerge")
(load! "conf.d/local" nil t)
