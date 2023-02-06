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

(setq doom-big-font (font-spec :family "Iosevka Comfy" :size 14)
      doom-font (font-spec :family "Iosevka Comfy" :size 13)
      doom-serif-font (font-spec :family "Overpass" :weight 'light :size 13)
      doom-unicode-font (font-spec :family "Iosevka Term")
      doom-variable-pitch-font (font-spec :family "Overpass" :size 13))

(setq doom-modeline-buffer-encoding nil
      doom-modeline-buffer-file-name-style 'relative-to-project
      doom-modeline-display-default-persp-name t
      doom-modeline-hud t
      doom-modeline-persp-name t)

(setq display-line-numbers-type t
      doom-themes-treemacs-theme 'kaolin
      evil-goggles-duration 1.0
      evil-goggles-pulse t
      fancy-splash-image (expand-file-name "modern-sexy-v2_128.png" doom-user-dir)
      vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))
      vertico-posframe-poshandler #'kdz/posframe-poshandler-frame-bottom-center-offset)

;; Set default window size
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 235))
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(internal-border-width . 10))

(after! kaolin-themes
  (setq kaolin-themes-bold t
        kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline-wave nil))

(after! vterm (setq vterm-shell (brew-bin "zsh")))

(modern-fringes-mode t)

(after! consult
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) "# "))

(set-face-attribute 'minibuffer-prompt
                    nil
                    :height 1.3
                    :family doom-variable-pitch-font)

(after! doom-modeline
  (size-indication-mode nil)
  (setq doom-modeline-percent-position nil))

(after! persp-mode
  (advice-add 'persp-add-to-menu :after #'kdz/pin-workspaces)
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

;; https://www.reddit.com/r/emacs/comments/e7h3qw/how_to_make_open_repl_window_behavior_in_doom/

(defun kdz/popup-display-buffer-side-by-size (buffer &optional alist)
  "Dynamically display popup buffers to either the right side or bottom"
  (+popup-display-buffer-stacked-side-window-fn
   buffer
   (append `((side . ,(if (> (frame-pixel-height) (frame-pixel-width))
                          'bottom 'right)))
           alist)))

(set-popup-rules!
  '(("^\\*format-all-errors\\*" :height 5 :select nil :modeline nil)
    ("^\\*Capture\\*$\\|CAPTURE-.*$" :height 0.5 :select t :quit nil)
    ("\\*Messages\\*" :height 0.3 :quit nil)
    ("\\*Compile-Log\\*" :ttl 0.01 :quit t)))

(set-popup-rule! "^\\*helpful .+: .+\\*"
  :actions '(kdz/popup-display-buffer-side-by-size)
  :select t
  :height 0.5
  :width 0.5
  :quit nil
  :modeline t)

(defun kdz/writing-mode-enhancements ()
  (mixed-pitch-mode t)
  (visual-line-mode t)
  (display-fill-column-indicator-mode -1)
  (display-line-numbers-mode -1))

(add-hook 'org-mode-hook #'kdz/writing-mode-enhancements)
(add-hook 'markdown-mode-hook #'kdz/writing-mode-enhancements)

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
