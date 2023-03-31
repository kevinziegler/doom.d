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

(defun kdz/org-babel-plantuml-format-var (var-value)
  (cond ((numberp var-value) (number-to-string var-value))
        ((booleanp var-value) (if var-value "true" "false"))
        (t (format "\"%s\"" var-value))))

(defun kdz/org-babel-variable-assignments:plantuml (params)
  "Return a list of PlantUML statements assigning the block's variables.
PARAMS is a property list of source block parameters, which may
contain multiple entries for the key `:var'.  `:var' entries in PARAMS
are expected to be scalar variables."
  (mapcar
   (lambda (pair)
     (format "!$%s=%s"
	     (car pair)
	     (kdz/org-babel-plantuml-format-var (cdr pair))))
   (org-babel--get-vars params)))

(defun kdz/ob-plantuml-directive-theme (params)
  (let ((theme (cdr (assq :theme params)))
        (theme-from (cdr (assq :theme-from params))))
    (cond ((and theme theme-from) (format "!theme %s from %s"
                                          theme
                                          (expand-file-name theme-from)))
          (theme (format "!theme %s" theme))
          (t ""))))

(defun kdz/org-babel-plantuml-make-body (body params)
  (let ((theme-directive (kdz/ob-plantuml-directive-theme params))
        (type (or (cdr (assq :type params)) "uml"))
        (full-body
         (org-babel-expand-body:generic
	  body params (org-babel-variable-assignments:plantuml params))))
    (if (string-prefix-p "@start" body t) full-body
      (string-join (list (concat "@start" type)
                         theme-directive
                         full-body
                         (concat "@end" type))
                   "\n"))))
