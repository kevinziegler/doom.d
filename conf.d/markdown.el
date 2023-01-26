(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
(setq markdown-header-scaling t)

;; TODO Set popup rules for Markdown Xwidget to use right/50% split for the
;;      preview buffer
;; TODO Figure out "format-all-errors" when running preview mode
(after! markdown-xwidget
  (setq markdown-xwidget-command "pandoc"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default"))
