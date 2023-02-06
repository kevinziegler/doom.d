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
      enable-local-variables t
      evil-kill-on-visual-paste nil
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-fine-undo t
      global-subword-mode 1
      ispell-dictionary "en"
      ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-user-dir)
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

(add-to-list 'auto-mode-alist '("/Tiltfile.*\\'" . bazel-starlark-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

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
