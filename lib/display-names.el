(defvar kdz-org-title-pattern
  "^[[:space:]]*#\\+TITLE:[[:space:]]*\\(.*?\\)[[:space:]]*$"
  "Pattern for matching #+title: in org-mode documents")

(defvar kdz-special-buffer-pattern
  "^[[:space:]]*\\*.+\\*$"
  "Pattern for matching 'special' buffers that shouldn't display as files")

(defmacro kdz/with-buffer-or-current (&optional buffer &rest body)
  `(let ((buffer-actual (or ,buffer (current-buffer))))
     (with-current-buffer buffer-actual
       (unwind-protect (progn ,@body)))))

(defun kdz/is-org-buffer (&optional buffer)
  (kdz/with-buffer-or-current buffer
                              (eq 'org-mode major-mode)))

(defun kdz/is-org-roam-buffer (&optional buffer)
  (kdz/with-buffer-or-current buffer
                              (s-contains-p org-roam-directory
                                            (or buffer-file-name ""))))

(defun kdz/is-special-buffer (&optional buffer)
  (kdz/with-buffer-or-current buffer
                              (string-match-p kdz-special-buffer-pattern
                                              (buffer-name))))

(defun kdz/org-buffer-title (&optional buffer)
  (kdz/with-buffer-or-current buffer
                              (save-excursion (goto-char (point-min))
                                              (re-search-forward
                                               kdz-org-title-pattern nil t)
                                              (match-string 1))))

(defun kdz/org-buffer-display-name (&optional buffer)
  (kdz/with-buffer-or-current buffer
                              (when (kdz/is-org-buffer)
                                (or (kdz/org-buffer-title)
                                    (format "Untitled (%s)" (buffer-name))))))

(defun kdz/frame-title-buffer-name ()
  (cond ((kdz/is-special-buffer))
        ((kdz/is-org-buffer) (format "☰ %s" (kdz/org-buffer-display-name)))
        (t "%b")))

(defun kdz/frame-title-save-state ()
  (unless (kdz/is-special-buffer) (if (buffer-modified-p) "◉" " ● ")))


(defun kdz/frame-title-project-name ()
  (let* ((project-name (projectile-project-name))
         (display-name (if (string= "-" project-name) "No Project"
                         project-name)))
    (format "(%s)" display-name)))

(defun kdz/frame-title-segment (maybe-string &optional is-start)
  (let ((leading (unless is-start " ")))
    (when maybe-string (concat leading maybe-string))))
