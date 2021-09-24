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
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 13)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 13)
      doom-big-font (font-spec :family "Iosevka" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;;(setq doom-theme 'doom-oceanic-next)
;;(setq doom-theme 'doom-monokai-machine)
(setq doom-theme 'kaolin-dark)
;;(setq doom-theme 'kaolin-galaxy)
;;(setq doom-theme 'kaolin-bubblegum)
;;(setq doom-theme 'doom-old-hope)
;; (setq doom-theme 'doom-1337)
;;(setq doom-theme 'doom-henna)
(setq doom-themes-treemacs-theme 'kaolin)

(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-display-default-persp-name t)
(setq doom-modeline-hud t)
(setq doom-modeline-persp-name t)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)
(setq-default frame-title-format nil)

;; Set default window size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 180))

(after! kaolin-themes
  (setq kaolin-themes-bold t)
  (setq kaolin-themes-distinct-company-scrollbar t)
  (setq kaolin-themes-italic t)
  (setq kaolin-themes-treemacs-hl-line t)
  (setq kaolin-themes-italic-comments t)
  (setq kaolin-themes-underline-wave nil))

(after! treemacs
  (setq treemacs-collapse-dirs 7)
  (setq treemacs-width 45)
  (setq treemacs-project-follow-mode t)
  (setq lsp-treemacs-theme 'kaolin)
  (kaolin-treemacs-theme)
  (treemacs-modify-theme "kaolin"
    :config
    (progn
      (treemacs-create-icon
       :icon (format " %s " (all-the-icons-fileicon "jsonld" :size 0.9))
       :extensions ("json")
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format " %s " (all-the-icons-faicon "table" :size 0.9))
       :extensions ("csv")
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format " %s " (all-the-icons-faicon "cogs" :size 0.9))
       :extensions ("editorconfig" "envrc" "envrc.local")
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-material "class"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (class)
       :fallback 'same-as-icon)

      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-material "code"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (def)
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-material "code"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (function)
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-material "code"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (method)
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-faicon "tags"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (enum)
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-faicon "tags"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (enumerator)
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-faicon "tag"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (enum-item)
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-faicon "tag"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (enum-member)
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-material "info_outline"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (property)
       :fallback 'same-as-icon)
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-material "info_outline"
                                                   :size 0.9
                                                   :face 'font-lock-keyword-face))
       :extensions (constant)
       :fallback 'same-as-icon))))


(after! vterm (setq vterm-shell (brew-bin "zsh")))
