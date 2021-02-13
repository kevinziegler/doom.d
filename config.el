;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kevin Ziegler"
      user-mail-address "ziegler.kevin@heb.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 13)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13)
      doom-big-font (font-spec :family "Fira Sans" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;;(setq doom-theme 'doom-oceanic-next)
(setq doom-theme 'kaolin-galaxy)
(setq doom-themes-treemacs-theme 'kaolin)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;; Set default window size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 180))

(load! "funcs")
(load! "conf.d/lsp")
(load! "conf.d/lsp-java")
(load! "conf.d/heb")
(load! "conf.d/keybinds")
(load! "conf.d/org")

(after! kaolin-themes
  (setq kaolin-themes-italic-comments t)
  (setq kaolin-themes-underline-wave nil))
(after! treemacs
  (setq treemacs-collapse-dirs 7)
  (kaolin-treemacs-theme))
(after! plantuml-mode (setq plantuml-default-exec-mode 'executable))
(after! vterm (setq vterm-shell "/usr/local/bin/zsh"))
(after! magit (magit-org-todos-autoinsert))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

;; TODO: HEB, add projectile default directory
;; TODO: Add wq command for org-src-mode
;; TODO: 'add' menu for org under SPC m a
;; TODO: Minor mode for plantuml live-reload previews?
;; TODO: Set up dedicated notes workspace

(add-hook 'sh-mode-hook #'kdz/set-zshrc-sh-shell)
;;(add-hook 'markdown-mode-hook #'kdz/writing-fill-column)
