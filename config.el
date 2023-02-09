;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! page-break-lines)
(use-package! which-key-posframe)
(use-package! org-modern)

(mapc (lambda (lib-file) (load! (concat "lib/" lib-file)))
      (directory-files (expand-file-name "lib" doom-user-dir) nil "\\.el$"))

(setq user-full-name "Kevin Ziegler"
      auto-save-default t
      bookmark-version-control t
      company-show-quick-access  t
      delete-by-moving-to-trash t
      emojify-emoji-set "twemoji-v2"
      enable-local-variables t
      evil-kill-on-visual-paste nil
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-fine-undo t
      global-subword-mode 1
      ispell-dictionary "en"
      ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-user-dir)
      magit-git-executable (brew-bin "git")
      magit-repository-directories '(("~/dev" . 2) ("~/.dotfiles" . 0))
      markdown-header-scaling t
      markdown-fontify-code-blocks-natively t
      native-comp-async-report-warnings-errors nil
      page-break-lines-max-width fill-column
      password-cache-expiry nil
      projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/")
      projectile-project-search-path '("~/dev")
      scroll-margin 2
      truncate-string-ellipsis "…"
      undo-limit 80000000
      which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0.05
      window-combination-resize t
      x-stretch-cursor t)

(setq-default history-length 1000
              prescient-history-length 1000)

(after! plantuml-mode (setq plantuml-default-exec-mode 'executable))
(after! python (set-ligatures! 'python-mode nil))
(after! vertico (setq orderless-matching-styles '(orderless-prefixes
                                                  orderless-regexp)))
(after! hydra
  (defhydra hydra-git-timemachine ()
    "Git Time Machine"
    ("b" git-timemachine-blame "Show git blame")
    ("c" git-timemachine-show-commit "Show commit")
    ("p" git-timemachine-show-previous-revision "Previous revision")
    ("n" git-timemachine-show-next-revision "Next revision"))
  (add-hook! git-timemachine-mode #'hydra-git-timemachine/body))

(after! magit
  (magit-org-todos-autoinsert)
  (magit-delta-mode +1))

(after! markdown-xwidget
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default"))

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

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-pyright-multi-root nil
        lsp-idle-delay 0.8)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\__pycache__\\'"))

(after! lsp-java
  ;; (require 'lsp-java-boot)
  ;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  ;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (setq lsp-java-maven-download-sources t
        lsp-java-import-maven-enabled t)
  (setq lombok-jar-path
        (expand-file-name
         "~/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"))
  (setq lsp-java-lombok-args
    (list (concat "-javaagent:" lombok-jar-path)))
  (when (boundp 'lsp-java-vmargs)
    (setq lsp-java-vmargs (append lsp-java-vmargs lsp-java-lombok-args))))

(advice-add 'lsp-pyright-locate-python :around #'kdz/lsp-pyright-path-advice)

(add-to-list 'auto-mode-alist '("/Tiltfile.*\\'" . bazel-starlark-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

(load! "conf.d/keybinds")
(load! "conf.d/org")
;; (load! "conf.d/org/capture-templates")
(load! "conf.d/org/capture-inbox")
(load! "conf.d/org/faces")
(load! "conf.d/org/hooks")
(load! "conf.d/org/keybinds")
(load! "conf.d/org/ligatures")
(load! "conf.d/org/pretty-capture")
(load! "conf.d/ui")
(load! "conf.d/smerge")
(load! "conf.d/local" nil t)
