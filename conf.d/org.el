;;; $DOOMDIR/config-org.el -*- lexical-binding: t; -*-

(setq org-directory "~/notes/"
      org-hide-emphasis-markers t
      org-default-notes-file "~/notes/notes.org"
      org-roam-directory "~/notes/roam"
      org-insert-heading-respect-content t
      deft-recursive t
      deft-directory "~/notes")

(after! org
  (require 'ox-gfm nil t)
  (require 'org-expiry)

  (defvar org-babel-auto-async-languages '()
    "Babel languages which should be executed asyncronously by default.")

  (defun +org-export-remove-zero-width-space (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string "\u200B" "" text)))

  (setq org-ellipsis (all-the-icons-material "unfold_more")
        valign-fancy-bar t
        org-use-property-inheritance t
        ;; Not sure on this one - should check back later and see if it's useful
        org-fold-catch-invisible-edits t
        org-list-allow-alphabetical t
        org-fontify-quote-and-verse-blocks t
        doom-themes-org-fontify-special-tags nil
        org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

  ;; (map! :map org-mode-map
  ;;       :nie "M-SPC M-SPC" (cmd! (insert "\u200B")))

  ;; Improve org-roam buffer names in modelike
  (defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
    :around #'doom-modeline-buffer-file-name ; takes no args
    (if (s-contains-p org-roam-directory (or buffer-file-name ""))
        (replace-regexp-in-string
         "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
         "🢔(\\1-\\2-\\3) "
         (subst-char-in-string ?_ ?  buffer-file-name))
      (funcall orig-fun)))

  (defadvice! org-babel-get-src-block-info-eager-async-a (orig-fn &optional light datum)
    "Eagarly add an :async param to the src info, unless it seems problematic.
  This only acts o languages in `org-babel-auto-async-languages'.
  Not added when either:
  + session is not \"none\"
  + :sync is set"
    :around #'org-babel-get-src-block-info
    (let ((result (funcall orig-fn light datum)))
      (when (and (string= "none" (cdr (assoc :session (caddr result))))
                 (member (car result) org-babel-auto-async-languages)
                 (not (assoc :async (caddr result))) ; don't duplicate
                 (not (assoc :sync (caddr result))))
        (push '(:async) (caddr result)))
      result))

  (defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
    :around #'org-fancy-priorities-mode
    :around #'org-superstar-mode
    (ignore-errors (apply orig-fn args)))

  (after! org-superstar
    (setq org-superstar-headline-bullets-list '("◉" "○")
          org-superstar-prettify-item-bullets t ))
  (after! ox
    (add-to-list
     'org-export-filter-final-output-functions
     #'+org-export-remove-zero-width-space t)))

    (setq org-ellipsis "  "
        org-hide-leading-stars t
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue)))

  (setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link"))))

;; (org-add-link-type "gh" #'kdz/org-github-link)
;; (org-add-link-type "gl" #'kdz/org-gitlab-link)
;; (org-add-link-type "jira" #'kdz/org-jira-link)

;; Smart Parens config for org-mode
(sp-local-pair '(org-mode) "<<" ">>" :actions '(insert))
(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w)
                      ("@personal" . ?p)
                      (:endgroup . nil)))
