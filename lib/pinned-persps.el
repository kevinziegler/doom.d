(defvar kdz--notes-persp-name "Notes")
(defvar kdz--docs-persp-name "Doom Documentation")
(defvar kdz--pinned-workspaces (list kdz--notes-persp-name
                                     kdz--docs-persp-name))

(defun kdz/persp-to-front (persp-to-move)
  "Move PERSP-TO-MOVE perspective to the first position in the persp list"
  (when (member persp-to-move persp-names-cache)
    (setq persp-names-cache
          (cons persp-to-move
                (delete persp-to-move persp-names-cache)))))

(defun kdz/pin-workspaces (persp)
  (mapc #'kdz/persp-to-front (reverse kdz--pinned-workspaces)))

(defun kdz/notes-to-first-persp ()
  (when (member kdz--notes-persp-name persp-names-cache)
    (setq persp-names-cache
          (cons kdz--notes-persp-name
                (delete kdz--notes-persp-name persp-names-cache)))))
