(defmacro kdz/doom-run-in-workspace (workspace function)
  `(advice-add ,function
    :before (lambda (&rest _)
              (when `(modulep! :ui workspaces)
                (+workspace-switch ,workspace t)))))

(defun kdz/switch-to-notes ()
  (interactive)
  (if-let ((index (cl-position "*Notes*" (+workspace-list-names) :test 'equal)))
      (+workspace/switch-to index)
    (+default/find-in-notes)))

(defmacro kdz/pin-workspaces! (&rest workspace-names)
  `(advice-add
   'persp-add-to-menu
   :after
   (lambda (persp)
     (mapc (lambda (persp-to-move)
             (when (member persp-to-move persp-names-cache)
               (setq persp-names-cache
                     (cons persp-to-move
                           (delete persp-to-move persp-names-cache)))))
           ',(reverse workspace-names)))))
