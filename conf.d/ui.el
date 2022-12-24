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

(setq doom-font (font-spec :family "Iosevka Fixed" :size 12.5)
      doom-big-font (font-spec :family "Iosevka Fixed" :size 14)
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

(when (> emacs-major-version 28)
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

(after! consult
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) "# "))

(set-face-attribute 'minibuffer-prompt nil :height 1.3 :family doom-variable-pitch-font)

(after! doom-modeline (size-indication-mode nil))

(defvar kdz--notes-persp-name "Notes")

(defun kdz/notes-to-first-persp ()
  "Move the notes perspective to the first position"
  (setq persp-names-cache
        (cons kdz--notes-persp-name
              (delete kdz--notes-persp-name persp-names-cache))))

(after! persp-mode
  (persp-def-auto-persp kdz--notes-persp-name
                        :mode 'org-mode
                        :file-name (regexp-quote (file-truename org-directory))
                        :switch 'frame)
  (persp-def-auto-persp "Doom Documentation"
                        :mode 'org-mode
                        :file-name (expand-file-name doom-emacs-dir)
                        :switch 'frame))

(setq vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))
      vertico-posframe-poshandler #'kdz/posframe-poshandler-frame-bottom-center-offset)

(set-popup-rule! "^\\*format-all-errors\\*"
  :side 'bottom
  :height 5
  :select nil
  :quit t
  :modeline nil)

(set-popup-rule! "^\\*helpful .+: .+\\*"
  :side 'right
  :select t
  :width 0.5
  :quit nil
  :modeline t)

(set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$"
  :side 'bottom
  :height 0.5
  :select t
  :quit nil)

(set-popup-rule! "\\*Messages\\*" :height 0.3 :quit nil)
(set-popup-rule! "\\*Compile-Log\\*" :ttl 0 :quit t)

(after! which-key-posframe
  (which-key-posframe-mode 1)
  (setq which-key-posframe-poshandler
        #'kdz/posframe-poshandler-frame-bottom-center-offset))

(add-hook 'text-mode-hook (lambda ()
                            (setq-local fill-column 120)
                            (visual-fill-column-mode t)
                            (mixed-pitch-mode t)
                            (display-fill-column-indicator-mode -1)
                            (display-line-numbers-mode -1)))

(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand)))

(add-hook! 'treemacs-select-functions #'kdz/treemacs-init-grow-frame)
(add-hook! 'treemacs-quit-hook #'kdz/treemacs-quit-shrink-frame)

;; Doom's +treemacs/toggle function uses delete-window to remove the treemacs
;; window, which doesn't call the quit hooks.  To resize the frame when using
;; the 'SPC o p' toggle we have to advise this function to trigger the resize
;; explicitly.
(advice-add '+treemacs/toggle :around #'kdz/treemacs-toggle-resize-advice)
