;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
      company-show-quick-access  t
      which-key-idle-delay 0.1
      which-key-idle-secondary-delay 0.05
      evil-want-fine-undo t
      evil-kill-on-visual-paste nil
      evil-vsplit-window-right t
      evil-split-window-below t
      native-comp-async-report-warnings-errors nil
      page-break-lines-max-width fill-column
      bookmark-version-control t
      projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/")
      ispell-dictionary "en"
      ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-user-dir))

(setq-default history-length 1000
              prescient-history-length 1000)

(after! plantuml-mode (setq plantuml-default-exec-mode 'executable))
(after! python (set-ligatures! 'python-mode nil))
(after! vertico (setq orderless-matching-styles '(orderless-prefixes
                                                  orderless-regexp)))

(add-to-list 'auto-mode-alist '("/Tiltfile.*\\'" . bazel-starlark-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(mapc (lambda (lib-file) (load! (concat "lib/" lib-file)))
      (directory-files (expand-file-name "lib" doom-user-dir) nil "\\.el$"))

(load! "conf.d/emoji")
(load! "conf.d/git")
(load! "conf.d/lsp")
(load! "conf.d/keybinds")
(load! "conf.d/markdown")
(load! "conf.d/org")
;; (load! "conf.d/org/capture-templates")
(load! "conf.d/org/capture-inbox")
(load! "conf.d/org/faces")
(load! "conf.d/org/hooks")
(load! "conf.d/org/keybinds")
(load! "conf.d/org/ligatures")
(load! "conf.d/org/pretty-capture")
(load! "conf.d/ui")
(load! "conf.d/treemacs")
(load! "conf.d/smerge")
(load! "conf.d/local" nil t)
