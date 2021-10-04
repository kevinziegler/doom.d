;;; config-lsp.el -*- lexical-binding: t; -*-

(add-hook! lsp-mode #'lsp-enable-which-key-integration)

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (setq lsp-idle-delay 0.8))
