(after! org
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'prepend)

  (custom-set-faces!
    '(outline-1 :weight extra-bold :height 1.50 :underline t)
    '(outline-2 :weight bold :height 1.40)
    '(outline-3 :weight bold :height 1.30)
    '(outline-4 :weight semi-bold :height 1.20)
    '(outline-5 :weight semi-bold :height 1.15)
    '(outline-6 :weight semi-bold :height 1.15)
    '(outline-8 :weight semi-bold)
    '(outline-9 :weight semi-bold)
    '(org-document-title :height 1.9)))
