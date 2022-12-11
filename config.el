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
      ;; Show numbers by completions; accessible via M-<number>
      company-show-quick-access  t
      ;; Reduce time to show which-key popup
      which-key-idle-delay 0.1
      which-key-idle-secondary-delay 0.05
      evil-want-fine-undo t
      evil-kill-on-visual-paste nil
      evil-vsplit-window-right t
      evil-split-window-below t
      ;; Silence compiler warnings as they can be pretty disruptive
      comp-async-report-warnings-errors nil
      page-break-lines-max-width fill-column
      bookmark-version-control t
      projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/")
      ispell-dictionary "en"
      ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-user-dir))

(mapc (lambda (lib-file) (load! (concat "lib/" lib-file)))
      (directory-files (expand-file-name "lib" doom-user-dir) nil "\\.el$"))

;;(load! "conf.d/bookmarks")
(load! "conf.d/emoji")
(load! "conf.d/git")
(load! "conf.d/lsp")
;; (load! "conf.d/lsp-java")
(load! "conf.d/keybinds")
(load! "conf.d/markdown")
(load! "conf.d/modes")
(load! "conf.d/org")
(load! "conf.d/org/capture-templates")
(load! "conf.d/org/faces")
(load! "conf.d/org/hooks")
(load! "conf.d/org/keybinds")
(load! "conf.d/org/ligatures")
(load! "conf.d/ui")
(load! "conf.d/treemacs")
(load! "conf.d/smerge")

(load! "conf.d/local" nil t)

;; Use system-installed plantuml executable
(after! plantuml-mode (setq plantuml-default-exec-mode 'executable))

;; Don't use special ligatures in python mode
(after! python (set-ligatures! 'python-mode nil))

;; Set ordering styles for vertico
(after! vertico
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp)))

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

;; (use-package! etrace :after elp)

;; Show ANSI color codes in text-mode
;; TODO See how this plays with magit process buffers?
;; (after! text-mode
;;   (add-hook! 'text-mode-hook
;;              ;; Apply ANSI color codes
;;              (with-silent-modifications
;;                (ansi-color-apply-on-region (point-min) (point-max) t))))

;; (bookmark-load (concat doom-private-dir "bookmarks"))
;; TODO Set popup rules for Markdown Xwidget to use right/50% split for the
;;      preview buffer
;; TODO Figure out "format-all-errors" when running preview mode
(after! markdown-xwidget
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default"))
