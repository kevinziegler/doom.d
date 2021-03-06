;;; config-lsp.el -*- lexical-binding: t; -*-

(map! :after lsp-mode :map lsp-mode-map
  :leader (:prefix "c"
    (:prefix ("b" . "Browse")
      :desc "Dependencies" "d" #'lsp-treemacs-dep-list
      :desc "Errors (LSP)" "e" #'lsp-treemacs-errors-list
      :desc "Implementations" "i" #'lsp-treemacs-implementations
      :desc "References" "r" #'lsp-treemacs-references
      :desc "Symbols" "s" #'lsp-treemacs-symbols
      :desc "Type Hierarchy" "t" #'lsp-treemacs-type-hierarchy)))

(add-hook! lsp-mode #'lsp-enable-which-key-integration)

(after! lsp-mode
  (setq lsp-idle-delay 0.8))
