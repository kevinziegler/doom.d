(defun kdz/line-number-from-fragment (fragment)
  "Find line number of FRAGMENT in current buffer"
  (when fragment
    (save-excursion (goto-char (point-min))
                    (search-forward fragment)
                    (line-number-at-pos))))

(defun kdz/git-link--tag ()
  "Get the latest tag for constructing a git-link URL."
  (car (git-link--exec "describe" "--tags" "HEAD")))

(defun kdz/file-link-as-git-link (git-file option)
  "Convert a file path link to a git-controlled file to a git-link URL"
  (let* ((existing-buffer (get-file-buffer git-file))
         (file-buffer (or existing-buffer (find-file-noselect git-file)))
         (option-as-number (when (and (stringp option)
                                  (string-match-p "\\`[0-9]+\\'" option))
                             (string-to-number option)))
         (url
          (with-current-buffer file-buffer
            ;; `git-link'  returns nil on success or a string on error
            (unless (git-link (git-link--remote)
                              (or option-as-number
                                  (kdz/line-number-from-fragment option))
                              nil)
              (car kill-ring)))))
    (unless existing-buffer (kill-buffer file-buffer))
    url))

(defun kdz/ox-filter-git-file-link (data backend channel)
  "Transform file links in DATA into git-link URLs when appropriate."
  (let* ((beg (next-property-change 0 data))
         (link (if beg (get-text-property beg :parent data)))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (option (org-element-property :search-option link)))
    (when (and (equal (org-element-property :type link) "file")
               (eq (vc-backend path) 'Git))
      (format "[[%s][%s]]"
              (kdz/file-link-as-git-link path option)
              (org-element-contents link)))))
