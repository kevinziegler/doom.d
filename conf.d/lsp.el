;;; config-lsp.el -*- lexical-binding: t; -*-

(add-hook! lsp-mode #'lsp-enable-which-key-integration)

(after! lsp-mode
  (setq lsp-idle-delay 0.8))
