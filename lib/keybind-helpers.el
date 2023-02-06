
(defun kdz/toggle-buffer-window (buffer-name activate-fn)
  (let ((window (get-buffer-window buffer-name)))
    (if window (delete-window window) (funcall activate-fn))))

(defun kdz/toggle-lsp-symbols ()
  (interactive)
  (kdz/toggle-buffer-window
   lsp-treemacs-symbols-buffer-name
   'lsp-treemacs-symbols))

(defun kdz/toggle-lsp-errors-list ()
  (interactive)
  (kdz/toggle-buffer-window
   lsp-treemacs-errors-buffer-name
   'lsp-treemacs-errors-list))

(defun switch-to-message-buffer ()
  (interactive)
  (pop-to-buffer "*Messages*"))

(defun kdz/org-insert-heading-up (arg)
  (interactive "p")
  (save-excursion
    (outline-up-heading arg)
    (org-insert-heading-after-current)
    (evil-insert nil)))

(defun kdz/org-insert-subheading ()
  (interactive)
  (end-of-visible-line)
  (call-interactively #'org-insert-subheading)
  (evil-insert nil))

(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
     editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
