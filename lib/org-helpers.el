(defvar org-babel-auto-async-languages '()
  "Babel languages which should be executed asyncronously by default.")

(defun stolen/org-babel-get-src-block-info-eager-async-a (orig-fn
                                                          &optional light datum)
    "Eagarly add an :async param to the src info, unless it seems problematic.
  This only acts o languages in `org-babel-auto-async-languages'.
  Not added when either:
  + session is not \"none\"
  + :sync is set"
  (let ((result (funcall orig-fn light datum)))
      (when (and (string= "none" (cdr (assoc :session (caddr result))))
                 (member (car result) org-babel-auto-async-languages)
                 (not (assoc :async (caddr result))) ; don't duplicate
                 (not (assoc :sync (caddr result))))
        (push '(:async) (caddr result)))
      result))

(defun stolen/doom-modeline-buffer-file-name-roam-aware-a (orig-fun)
  "Improve org-roam buffer names in modeline"
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

(defun +org-export-remove-zero-width-space (text _backend _info)
  "Remove zero width spaces from TEXT."
  (unless (org-export-derived-backend-p 'org)
    (replace-regexp-in-string "\u200B" "" text)))
