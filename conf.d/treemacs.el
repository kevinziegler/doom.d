
(setq +treemacs-git-mode 'deferred)

(after! treemacs
  (setq treemacs-collapse-dirs 7)
  (setq treemacs-width 45)
  (setq treemacs-follow-mode t)
  (setq treemacs-project-follow-mode t)
  (setq treemacs-recenter-after-file-follow t)
  (setq treemacs-project-follow-cleanup t)
  (kaolin-treemacs-theme)
  (setq lsp-treemacs-theme 'kaolin)
  (treemacs-modify-theme "kaolin"
    :config
    (progn
      (treemacs-create-icon
       :icon (format " %s " (all-the-icons-faicon "archive" :size 0.9))
       :extensions (package)
       :fallback 'same-as-icon)
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
       :icon (format "%s " (all-the-icons-faicon "archive"
                                                 :size 0.9
                                                 :v-adjust -0.1))
       :extensions (namespace)
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
