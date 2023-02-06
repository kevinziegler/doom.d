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

(defun kdz/lsp-pyright-path-advice (origin-fn &rest args)
  "Advice to determine Python venv/executable values for lsp-pyright"
  (let* ((poetry-specified-venv
          (condition-case nil (poetry-venv-exist-p) (error nil)))
         (lsp-pyright-venv-path (or poetry-specified-venv
                                    lsp-pyright-venv-path))
         (asdf-specified-python (kdz/asdf-which "python")))
    ;; Our first preference is to use whatever poetry-specified environment
    ;; is detected, if one is detected at all.
    ;;
    ;; Failing that, we'll at least look for an ASDF-specified Python
    ;; executable.
    ;;
    ;; Lastly, we'll just fall back to allow lsp-pyright to find python and
    ;; venv values as it sees fit
    (if (and (not poetry-specified-venv) asdf-specified-python)
        asdf-specified-python
      (apply origin-fn args))))

(advice-add 'lsp-pyright-locate-python :around #'kdz/lsp-pyright-path-advice)
