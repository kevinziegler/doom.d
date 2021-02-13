(after! lsp-java
  ;; (require 'lsp-java-boot)
  ;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  ;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (setq lsp-java-maven-download-sources t)
  (setq lsp-java-import-maven-enabled t)
  (setq lombok-jar-path
        (expand-file-name
         "~/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"))
  (setq lsp-java-lombok-args
    (list (concat "-javaagent:" lombok-jar-path)))
  (when (boundp 'lsp-java-vmargs)
    (setq lsp-java-vmargs (append lsp-java-vmargs lsp-java-lombok-args))))

(after! dap-java
  (setq dap-java-doom-junit-runner
    "eclipse.jdt.ls/server/test-runner/junit-platform-console-standalone.jar")
  (setq dap-java-test-runner (concat doom-etc-dir dap-java-doom-junit-runner)))
