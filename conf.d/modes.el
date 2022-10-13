
(add-to-list 'auto-mode-alist '("/Tiltfile.*\\'" . bazel-starlark-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

;; Open `.zshrc` in shell-script-mode with the ZSH dialect
(add-hook 'sh-mode-hook #'kdz/set-zshrc-sh-shell)
