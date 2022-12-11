;;; config-lsp.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-pyright-multi-root nil
        lsp-idle-delay 0.8)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\__pycache__\\'"))

(after! lsp-java
  ;; (require 'lsp-java-boot)
  ;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  ;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (setq lsp-java-maven-download-sources t
        lsp-java-import-maven-enabled t)
  (setq lombok-jar-path
        (expand-file-name
         "~/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"))
  (setq lsp-java-lombok-args
    (list (concat "-javaagent:" lombok-jar-path)))
  (when (boundp 'lsp-java-vmargs)
    (setq lsp-java-vmargs (append lsp-java-vmargs lsp-java-lombok-args))))

(defun kdz/pyright-prefer-asdf-python (origin-fn &rest args)
  (or (kdz/asdf-which "python") (apply origin-fn args)))

;; When using lsp-pyright, check if we have an asdf-managed python and prefer
;; that when specifying a python binary for the LSP server.  This addresses
;; issues where the pyright server process picks up the wrong python version
;; because it runs the ASDF shim in a directory that isn't the project root
(advice-add 'lsp-pyright-locate-python :around #'kdz/pyright-prefer-asdf-python)
