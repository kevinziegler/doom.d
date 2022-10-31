;;; config-lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (setq lsp-idle-delay 0.8)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\__pycache__\\'"))

(setq lsp-pyright-multi-root nil)

(defun kdz/pyright-prefer-asdf-python (origin-fn &rest args)
  (or (kdz/asdf-which "python") (apply origin-fn args)))

;; When using lsp-pyright, check if we have an asdf-managed python and prefer
;; that when specifying a python binary for the LSP server.  This addresses
;; issues where the pyright server process picks up the wrong python version
;; because it runs the ASDF shim in a directory that isn't the project root
(advice-add 'lsp-pyright-locate-python :around #'kdz/pyright-prefer-asdf-python)
