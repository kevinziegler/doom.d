;;; $DOOMDIR/config-org.el -*- lexical-binding: t; -*-

(setq org-directory "~/notes/"
      org-hide-emphasis-markers t
      org-default-notes-file "~/notes/notes.org"
      org-roam-directory "~/notes/roam"
      org-insert-heading-respect-content t
      deft-recursive t
      deft-directory "~/notes")

(defun kdz/org-capture-template (fname)
  "Generate a path to the capture template named <FNAME>.org.tpl"
  (concat doom-private-dir "capture-templates/" fname ".org.tpl"))

(defun kdz/iconify-org-token-2 (icon-fn icon-family icon token)
  (push (cons (upcase token)
              (propertize
               (funcall icon-fn icon)
               'face `(:family ,(funcall icon-family) :height 1.2)))
              prettify-symbols-alist)
  (push (cons (downcase token)
              (propertize
               (funcall icon-fn icon)
               'face `(:family ,(funcall icon-family) :height 1.2)))
        prettify-symbols-alist))

(defun kdz/iconify-org-token (icon-fn icon token)
  (push (cons (upcase token) (funcall icon-fn icon :v-adjust -0.5 :height 1.2)) prettify-symbols-alist)
  (push (cons (downcase token) (funcall icon-fn icon :v-adjust -0.5 :height 1.2)) prettify-symbols-alist))

(defun kdz/org-buffer-margins ()
  (setq left-margin-width 2)
  (setq right-margin-width 2))

(defun kdz/org-insert-heading-up (arg)
  (interactive "p")
  (save-excursion
    (outline-up-heading arg)
    (org-insert-heading-after-current)))

(after! org
  (require 'ox-gfm nil t)
  (require 'ob-restclient)
  (require 'ob-http)
  (require 'org-pretty-table)

  (add-hook 'org-mode-hook #'kdz/writing-fill-column)
  (add-hook 'org-mode-hook #'kdz/org-buffer-margins)
  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'org-mode-hook 'org-pretty-table-mode)

  (setq org-ellipsis (all-the-icons-material "unfold_more"))

  ;;(setq org-superstar-item-bullet-alist '("●" "‣" "‒"))

  (map! :map org-mode-map
        :localleader
        :desc "Schedule" "S" #'org-schedule)

  (map! :map org-mode-map
        :localleader
        (:prefix ("s" . "Subtrees")
        :desc "Cut subtree" "d" #'org-cut-subtree
        :desc "Promote subtree" "h" #'org-promote-subtree
        :desc "Demote subtree" "l" #'org-demote-subtree
        :desc "Move Subtree Up" "k" #'org-move-subtree-up
        :desc "Move Subtree Down" "j" #'org-move-subtree-down))

  (map! :map org-mode-map
        :leader
        (:prefix "i"
         :desc "Heading" "h" #'org-insert-heading
         :desc "Parent Heading" "H" #'kdz/org-insert-heading-up
         :desc "Subheading" "s" #'org-insert-subheading
         :desc "Link" "l" #'org-insert-link))


  (add-to-list 'org-capture-templates
               '("f"
                "Jira Feature Ticket Note"
                entry
                (file "~/notes/tickets.org")
                (file (kdz/org-capture-template "feature-ticket"))))

  (add-to-list 'org-capture-templates
               '("b"
                "Jira Bug Ticket Note"
                entry
                (file "~/notes/tickets.org")
                (file (kdz/org-capture-template "bug-ticket"))))

  (add-to-list 'org-capture-templates
               '("d"
                "Technical debt Note"
                entry (file "~/notes/general.org")
                "* TODO Technical debt work: %?\nFound in: [[file:%F][%f]]"))

  (add-to-list 'org-capture-templates
               '("r"
                "Retro thought"
                entry
                (file "~/notes/general.org")
                "* TODO Discuss in next retro: %?")))
