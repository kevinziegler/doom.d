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
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 12)
;;       doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 12)
;;       doom-big-font (font-spec :family "JetBrains Mono" :size 12)
;;       ;; doom-theme 'doom-one ;; NOTE Set by local.el
;;       )

(setq doom-font (font-spec :family "Fira Code" :size 12)
      doom-big-font (font-spec :family "Fira Code" :size 14)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 13)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "Overpass" :weight 'light :size 13)
      doom-modeline-buffer-encoding nil
      doom-modeline-buffer-file-name-style 'relative-to-project
      doom-modeline-display-default-persp-name t
      doom-modeline-hud t
      doom-modeline-persp-name t
      doom-modeline-major-mode-icon t
      doom-themes-treemacs-theme 'kaolin)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

(setq frame-title-format
      '((:eval (kdz/frame-title-segment (kdz/frame-title-save-state) t))
        (:eval (kdz/frame-title-segment (kdz/frame-title-buffer-name)))
        (:eval (kdz/frame-title-segment (kdz/frame-title-project-name)))))

;; Set default window size
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 235))

(after! kaolin-themes
  (setq kaolin-themes-bold t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline-wave nil))

(after! vterm (setq vterm-shell (brew-bin "zsh")))

(global-display-fill-column-indicator-mode)

;; (after! consult
;;   (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
;;   (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) "; "))

(after! magit (magit-delta-mode +1))

(after! org
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'prepend)
  (set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$"
    :side 'bottom
    :height 0.5
    :select t
    :quit nil))

(setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))

;; NOTE Replace this with `pixel-scroll-precision-mode' in Emacs 29
;; (good-scroll-mode 1)

(defun make-capture-frame ()
     "Create a new frame and run org-capture."
     (interactive)
     (make-frame '((name . "capture")))
     (select-frame-by-name "capture")
     (delete-other-windows)
     (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
       (org-capture)))
