;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kevin Ziegler"
      user-mail-address "ziegler.kevin@heb.com")

(load! "funcs")
(load! "conf.d/lsp")
(load! "conf.d/lsp-java")
(load! "conf.d/heb")
(load! "conf.d/keybinds")
(load! "conf.d/org")
(load! "conf.d/ui")

;; Open TSX files in Typescript TSX Mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

;; Use system-installed plantuml executable
(after! plantuml-mode (setq plantuml-default-exec-mode 'executable))

;; Open `.zshrc` in shell-script-mode with the ZSH dialect
(add-hook 'sh-mode-hook #'kdz/set-zshrc-sh-shell)
