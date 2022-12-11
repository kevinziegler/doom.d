(after! org
  (custom-set-faces!
    '(outline-1 :weight extra-bold :height 1.25)
    '(outline-2 :weight bold :height 1.15)
    '(outline-3 :weight bold :height 1.12)
    '(outline-4 :weight semi-bold :height 1.09)
    '(outline-5 :weight semi-bold :height 1.06)
    '(outline-6 :weight semi-bold :height 1.03)
    '(outline-8 :weight semi-bold)
    '(outline-9 :weight semi-bold)
    '(org-document-title :height 1.5))

  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'prepend))
