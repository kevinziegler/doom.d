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

(defun kdz/org-goto-top-level ()
  (interactive)
  (let ((org-goto-interface 'outline-path-completion)
        (org-goto-max-level 1))
    (org-goto)
    (org-narrow-to-subtree)))

(defun kdz/org-open-one-on-one ()
  (interactive)
  (find-file "~/notes/one-on-one-discussions.org")
  (when (buffer-narrowed-p) (widen))
  (let ((org-goto-interface 'outline-path-completion)
        (org-goto-max-level 1))
    (org-goto))
  (org-narrow-to-subtree)
  (org-show-todo-tree nil))

(defmacro kdz/org-appear-hook-evil-state (evil-state)
  `(lambda ()
     (add-hook ',(intern (concat "evil-" (symbol-name evil-state) "-entry-hook"))
               #'org-appear-manual-start nil t)
     (add-hook ',(intern (concat "evil-" (symbol-name evil-state) "-exit-hook"))
               #'org-appear-manual-stop nil t)))

(defun kdz/org-output-dir ()
  "Helper to set the default path for org babel outputs (via the :dir header)"
  (concat (file-name-base (buffer-file-name)) ".outputs"))

;; This works to insert a new line in cases where it's missing, sort of:
;; - It still doesn't address cases where the section element has _multiple_ newlines
;; - Need to see how it works when a section contains only newline(s)
;;
;; Rules for end-of-section blanks:
;; - If a section has content, then that section should end with a blank
;;   - "properties" an other metadata are not counted as content
;; - If the next section in the document is at a higher level (i.e. is not a
;;   sibling or a child), then the section should close with two blank lines
(defun kdz/section-add-newline-maybe (section-element edit-count)
  (when (eq 0 (org-element-property :post-blank section-element))
    (save-excursion
      (goto-char (+ edit-count (org-element-property :end section-element)))
      (cl-incf edit-count)
      (insert hard-newline))))

(defun kdz/org-inject-section-blanks ()
  (let ((edited-section-count 0))
    (org-element-map (org-element-parse-buffer) 'section
      (lambda (section-element)
        (kdz/section-add-newline-maybe section-element edited-section-count)))))

(defun kdz/org-export-path (file-name &optional subpath)
  (interactive "f")
  (concat (file-name-base file-name)
          ".exports"
          (when subpath "/")
          subpath))

(defun kdz/org-mac-link-advise-evil (org-mac-link-fn &rest orig-args)
  "Advice to org-mac-link functions to handle insertion with evil-mode"
  (interactive)
  (let ((char-at-insert (thing-at-point 'char)))
    (evil-save-state
      (evil-append 1)
      (when (not (string-match-p "[[:space:]]" char-at-insert))
          (insert " "))
      (apply org-mac-link-fn orig-args))))
