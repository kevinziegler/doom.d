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
      doom-themes-treemacs-theme 'kaolin
      fancy-splash-image (expand-file-name "modern-sexy-v2_128.png" doom-user-dir)
      display-line-numbers-type t
      frame-title-format
      '((:eval (kdz/frame-title-segment (kdz/frame-title-save-state) t))
        (:eval (kdz/frame-title-segment (kdz/frame-title-buffer-name)))
        (:eval (kdz/frame-title-segment (kdz/frame-title-project-name)))))

;; Set default window size
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 235))

(when (boundp 'EMACS29+)
      (add-to-list 'default-frame-alist '(undecorated-round . t))
      (add-to-list 'default-frame-alist '(internal-border-width . 10)))

(after! kaolin-themes
  (setq kaolin-themes-bold t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline-wave nil))

(after! vterm (setq vterm-shell (brew-bin "zsh")))

(global-display-fill-column-indicator-mode)
(modern-fringes-mode t)

;; (after! consult
;;   (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
;;   (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) "; "))

(defvar kdz--notes-persp-name "Notes")

(defun kdz/notes-to-first-persp ()
  "Move the notes perspective to the first position"
  (setq persp-names-cache
        (cons kdz--notes-persp-name
              (delete kdz--notes-persp-name persp-names-cache))))

(after! persp-mode
  (persp-def-auto-persp kdz--notes-persp-name
                        :mode 'org-mode
                        :file-name (file-truename org-directory)
                        :hooks '(kdz/notes-to-first-persp)
                        :switch 'frame)
  (persp-def-auto-persp "Doom Documentation"
                        :mode 'org-mode
                        :file-name (expand-file-name doom-emacs-dir)
                        :switch 'frame))

(defvar kdz-posframe-top-offset 50)
(defvar kdz-posframe-bottom-offset 50)

(defun kdz/posframe-poshandler-frame-top-center-offset (info)
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        kdz-posframe-top-offset))

(defun kdz/posframe-poshandler-frame-bottom-center-offset (info)
   (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (- (plist-get info :parent-frame-height)
           (plist-get info :posframe-height)
           (plist-get info :mode-line-height)
           (plist-get info :minibuffer-height)
           kdz-posframe-bottom-offset)))

(setq vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))
      vertico-posframe-poshandler #'kdz/posframe-poshandler-frame-bottom-center-offset)
