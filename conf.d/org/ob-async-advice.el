;;; conf.d/org/ob-async-advice.el -*- lexical-binding: t; -*-
;;;
;;; This file address changes in the org-babel API that have broken ob-async:
;;; https://github.com/astahlman/ob-async/issues/92
;;;
;;; First, we define a replacement to ob-async's execute function to
;;; effectively apply this patch:
;;;
;;; https://github.com/astahlman/ob-async/pull/93/files
;;;
;;; This new version of the function is implemented as advice to override the
;;; intitial function definition from ob-async.
;;;
;;; Then, we make the corresponding change to Doom's own helper advice and
;;; replace the advice from the Doom org module with an additional
;;; org-load-hook.
;;;
;;; Once the ob-async issue is merged and updated in Doom's module dependencies
;;; the "override" advice can be removed.  Finally, once doom's own module code
;;; is updated as well we should be able to remove this adivce entirely.

(defun kdz/ob-async-org-babel-execute-src-block
    (&optional orig-fun arg info params &rest other-args)
  "Like org-babel-execute-src-block, but run asynchronously.

Original docstring for org-babel-execute-src-block:

Execute the current source code block.  Insert the results of
execution into the buffer.  Source code execution and the
collection and formatting of results can be controlled through a
variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive "P")
  (cond
   ;; If this function is not called as advice, do nothing
   ((not orig-fun)
    (warn "ob-async-org-babel-execute-src-block is no longer needed in org-ctrl-c-ctrl-c-hook")
    nil)
   ;; If there is no :async parameter, call the original function
   ((not (assoc :async (nth 2 (or info (org-babel-get-src-block-info)))))
    (apply orig-fun arg info params other-args))
   ;; If the src block language is in the list of languages async is not to be
   ;; used for, call the original function
   ((member (nth 0 (or info (org-babel-get-src-block-info)))
            ob-async-no-async-languages-alist)
    (apply orig-fun arg info params other-args))
   ;; Otherwise, perform asynchronous execution
   (t
    (let ((placeholder (ob-async--generate-uuid)))
      ;; Here begins the original source of org-babel-execute-src-block
      (let* ((org-babel-current-src-block-location
              (or org-babel-current-src-block-location
                  (nth 5 info)
                  (org-babel-where-is-src-block-head)))
             (src-block-marker (save-excursion
                                 (goto-char org-babel-current-src-block-location)
                                 (point-marker)))
             (info (if info (copy-tree info) (org-babel-get-src-block-info))))
        ;; Merge PARAMS with INFO before considering source block
        ;; evaluation since both could disagree.
        (cl-callf org-babel-merge-params (nth 2 info) params)
        (when (org-babel-check-evaluate info)
          (cl-callf org-babel-process-params (nth 2 info))
          (let* ((params (nth 2 info))
                 (cache (let ((c (cdr (assq :cache params))))
                          (and (not arg) c (string= "yes" c))))
                 (new-hash (and cache (org-babel-sha1-hash info)))
                 (old-hash (and cache (org-babel-current-result-hash)))
                 (current-cache (and new-hash (equal new-hash old-hash)))
                 (result-params (cdr (assq :result-params params))))
            (cond
             (current-cache
              (save-excursion		;Return cached result.
                (goto-char (org-babel-where-is-src-block-result nil info))
                (forward-line)
                (skip-chars-forward " \t")
                (let ((result (org-babel-read-result)))
                  (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
                  result)))
             ((org-babel-confirm-evaluate info)
              ;; Insert a GUID as a placeholder in our RESULTS block
              (when (not (or (member "none" result-params)
                             (member "silent" result-params)))
                (org-babel-insert-result placeholder '("replace")))
              (let* ((lang (nth 0 info))
                     ;; Expand noweb references in BODY and remove any
                     ;; coderef.
                     (body
                      (let ((coderef (nth 6 info))
                            (expand
                             (if (org-babel-noweb-p params :eval)
                                 (org-babel-expand-noweb-references info)
                               (nth 1 info))))
                        (if (not coderef) expand
                          (replace-regexp-in-string
                           (org-src-coderef-regexp coderef) "" expand nil nil 1))))
                     (dir (cdr (assq :dir params)))
                     (default-directory
                       (or (and dir (file-name-as-directory (expand-file-name dir)))
                           default-directory))
                     (cmd (intern (concat "org-babel-execute:" lang)))
                     (org-babel-async-content
                      (buffer-substring-no-properties (point-min) (point-max)))
                     result)
                (unless (fboundp cmd)
                  (error "No org-babel-execute function for %s!" lang))
                (message "executing %s code block%s..."
                         (capitalize lang)
                         (let ((name (nth 4 info)))
                           (if name (format " (%s)" name) "")))
                (progn
                  (async-start
                   `(lambda ()
                      ;; TODO: Put this in a function so it can be overidden
                      ;; Initialize the new Emacs process with org-babel functions
                      (setq exec-path ',exec-path)
                      (setq load-path ',load-path)
                      ,(async-inject-variables ob-async-inject-variables)
                      (package-initialize)
                      (setq ob-async-pre-execute-src-block-hook ',ob-async-pre-execute-src-block-hook)
                      (run-hooks 'ob-async-pre-execute-src-block-hook)
                      (org-babel-do-load-languages 'org-babel-load-languages ',org-babel-load-languages)
                      (let ((default-directory ,default-directory))
                        (with-temp-buffer
                          (insert org-babel-async-content)
                          (,cmd ,body ',params))))
                   `(lambda (result)
                      (with-current-buffer ,(current-buffer)
                        (let ((default-directory ,default-directory))
                          (save-excursion
                            (cond
                              ((member "none" ',result-params)
                               (message "result silenced"))
                              ((member "silent" ',result-params)
                               (message (replace-regexp-in-string "%" "%%" (format "%S" result))))
                              (t
                               (goto-char ,src-block-marker)
                               (let ((file (cdr (assq :file ',params))))
                                 (when file
                                   ;; when result type is link, don't write result content to file.
                                   (unless (member "link" ',result-params)
                                     ;; If non-empty result and :file then write to :file.
                                     (when result
                                       (with-temp-file file
                                         (insert (org-babel-format-result
                                                  result (cdr (assq :sep ',params)))))))
                                   (setq result file))
                                 ;; Possibly perform post process provided its
                                 ;; appropriate.  Dynamically bind "*this*" to the
                                 ;; actual results of the block.
                                 (let ((post (cdr (assq :post ',params))))
                                   (when post
                                     (let ((*this* (if (not file) result
                                                     (org-babel-result-to-file
                                                      file
                                                      (let ((desc (assq :file-desc ',params)))
                                                        (and desc (or (cdr desc) result)))))))
                                       (setq result (org-babel-ref-resolve post))
                                       (when file
                                         (setq result-params (remove "file" ',result-params))))))
                                 (org-babel-insert-result result ',result-params ',info ',new-hash ',lang))))
                            (run-hooks 'org-babel-after-execute-hook)))))))))))))))))

(defun kdz/+org-babel-disable-async-maybe-a
    (fn &optional orig-fn arg info params &rest other-args)
  "Use ob-comint where supported, disable async altogether where it isn't.

We have access to two async backends: ob-comint or ob-async, which have
different requirements. This advice tries to pick the best option between them,
falling back to synchronous execution otherwise. Without this advice, they die
with an error; terrible UX!

Note: ob-comint support will only kick in for languages listed in
`+org-babel-native-async-langs'.

Also adds support for a `:sync' parameter to override `:async'."
  (if (null orig-fn)
      (apply fn orig-fn arg info params other-args)
    (let* ((info (or info (org-babel-get-src-block-info)))
           (params (org-babel-merge-params (nth 2 info) params)))
      (if (or (assq :sync params)
              (not (assq :async params))
              (member (car info) ob-async-no-async-languages-alist)
              ;; ob-comint requires a :session, ob-async does not, so fall
              ;; back to ob-async if no :session is provided.
              (unless (member (alist-get :session params) '("none" nil))
                (unless (memq (let* ((lang (nth 0 info))
                                     (lang (cond ((symbolp lang) lang)
                                                 ((stringp lang) (intern lang)))))
                                (or (alist-get lang +org-babel-mode-alist)
                                    lang))
                              +org-babel-native-async-langs)
                  (message "Org babel: %s :session is incompatible with :async. Executing synchronously!"
                           (car info))
                  (sleep-for 0.2)) t))
          (apply orig-fn arg info params other-args)
        (apply fn orig-fn arg info params other-args)))))

(after! org
  (advice-remove 'ob-async-org-babel-execute-src-block
                 #'+org-babel-disable-async-maybe-a)
  (advice-add 'ob-async-org-babel-execute-src-block
              :override
              #'kdz/ob-async-org-babel-execute-src-block)
  (advice-add 'ob-async-org-babel-execute-src-block
              :around
              #'kdz/+org-babel-disable-async-maybe-a))
