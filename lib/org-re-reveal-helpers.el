;; List the contents of the 'themes' directory for org-re-reveal
(defun kdz/org-re-reveal-list-themes ()
  (mapcar #'file-name-base
          (directory-files
           (expand-file-name "css/theme/source" org-re-reveal-root)
           nil
           "\.scss$")))

(defun kdz/org-re-reveal--theme-source-path (theme)
  (expand-file-name (concat theme ".scss")
                    (expand-file-name "css/theme/source" org-re-reveal-root) ))
(defun kdz/org-re-reveal--theme-dist-path (theme)
  (expand-file-name (concat theme ".css")
                    (expand-file-name "dist/theme" org-re-reveal-root)))

(defvar kdz-re-reveal-preview-root-properties
  '("--r-background-color"
    "--r-main-color"
    "--r-heading-color"
    "--r-link-color"
    "--r-link-color-dark"
    "--r-link-color-hover"
    "--r-selection-color"
    "--r-selection-background-color"))

(defvar kdz-re-reveal-color-preview-symbol "█")
(defvar kdz-re-reveal-color-regexp "#[A-Fa-f0-9]\\{3,6\\}")
(defvar kdz--org-re-reveal-compile-buffer "*org-rereveal: Compile RevealJS*")

(defun kdz/org-re-reveal--find-color-in-buffer (buffer selector)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (goto-char (search-forward selector))
      (re-search-forward kdz-re-reveal-color-regexp (line-end-position))
      (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

(defun kdz/org-re-reveal--preview-theme (theme)
  (let ((theme-dist-buffer
         (get-buffer-create (kdz/org-re-reveal--theme-dist-path theme))))
    (concat
     (mapcar
      (lambda (selector)
        (propertize
         kdz-re-reveal-color-preview-symbol
         'face
         `(:foreground
           ,(kdz/org-re-reveal--find-color-in-buffer theme-dist-buffer
                                                     selector))))
      kdz-re-reveal-preview-root-properties))))

;; Get the revealjs theme in the current org-mode document
(defun kdz/org-re-reveal-current-theme ())

;; Set the revealjs theme in the current org-mode document
(defun kdz/org-re-reveal-set-theme (theme))

(defun kdz/org-re-reveal-edit-theme (theme)
  "Edit a revealjs THEME, and then re-compile revealjs"
  (with-current-buffer
      (find-file-other-window (kdz/org-re-reveal--theme-source-path theme))
    (add-hook 'kill-buffer-hook 'kdz/org-re-reveal--compile)))

(defun kdz/org-re-reveal--compile ()
  (with-current-buffer kdz--org-re-reveal-compile-buffer
    (cd org-re-reveal-root)
    (call-process "npm" nil (current-buffer) nil "run" "build")))
