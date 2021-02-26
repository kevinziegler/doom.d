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
;;(setq doom-theme 'kaolin-galaxy)
(setq doom-theme 'kaolin-aurora)
(setq doom-themes-treemacs-theme 'kaolin)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)
(setq-default frame-title-format nil)

;; Set default window size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 180))

(after! kaolin-themes
  (setq kaolin-themes-italic-comments t)
  (setq kaolin-themes-underline-wave nil))
(after! treemacs
  (setq treemacs-collapse-dirs 7)
  (kaolin-treemacs-theme))

(after! vterm (setq vterm-shell "/usr/local/bin/zsh"))
