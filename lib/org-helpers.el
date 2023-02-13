(defun stolen/doom-modeline-buffer-file-name-roam-aware-a (orig-fun)
  "Improve org-roam buffer names in modeline"
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

(defun stolen/get-keyword-key-value (kwd)
     (let ((data (cadr kwd)))
       (list (plist-get data :key)
             (plist-get data :value))))

(defun stolen/org-current-buffer-get-title ()
  "Get the title of the current org-mode buffer"
  (nth 1
       (assoc "TITLE"
              (org-element-map (org-element-parse-buffer 'greater-element)
                  '(keyword)
                #'stolen/get-keyword-key-value))))

(defun kdz/string-to-filename (name-string word-sep extension)
  "Sanitize a string for use as a filename"
  (let* ((name-alphanumeric
          (replace-regexp-in-string "[^A-Za-z0-9 ]" "" name-string))
         (basename (string-replace " " word-sep (downcase name-alphanumeric))))
    (file-name-with-extension basename extension)))

(defun kdz/org-sync-title-to-filename ()
  "Sync the title of the current org document with the filename"
  (let* ((title (stolen/org-current-buffer-get-title))
         (basename (kdz/string-to-filename
                    title "-" (file-name-extension (buffer-file-name)))))
    (doom/move-this-file (expand-file-name
                          basename (file-name-directory (buffer-file-name))))))

(defun kdz/org-update-title (new-title)
  "Update the title of an org-mode document, or add one if none exists"
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (progn (search-forward "#+title: ")
               (kill-line)
               (insert new-title))
      ('error (insert (format "#+title: %s\n" new-title))))))

(defun kdz/org-rename-title ()
  "Update the title of an org-mode document interactively, and rename the file"
  (interactive)
  (let ((new-title (read-string (format "Re-Title \"%s\" to: "
                       (stolen/org-current-buffer-get-title)))))
    (kdz/org-update-title new-title)
    (kdz/org-sync-title-to-filename)))
