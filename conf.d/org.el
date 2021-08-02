;;; $DOOMDIR/config-org.el -*- lexical-binding: t; -*-

(setq org-directory "~/notes/"
      org-hide-emphasis-markers t
      org-journal-dir "~/notes/journal/"
      org-default-notes-file "~/notes/notes.org"
      org-insert-heading-respect-content t
      deft-recursive t
      deft-directory "~/notes")

(defun kdz/org-capture-template (fname)
  "Generate a path to the capture template named <FNAME>.org.tpl"
  (concat doom-private-dir "capture-templates/" fname ".org.tpl"))

(defun kdz/prettify-material-icon-for (token icon)
  "Use an all-the-icons-material icon for the given token in org-mode"
  (push (cons (upcase token) (all-the-icons-material icon)) prettify-symbols-alist)
  (push (cons (downcase token) (all-the-icons-material icon)) prettify-symbols-alist))

(defun kdz/prettify-org-material ()
  "Set up prettify-org with all-the-icons symbols"
  (kdz/prettify-material-icon-for "#+BEGIN_SRC" "code")
  (kdz/prettify-material-icon-for "#+END_SRC" "expand_less")
  (kdz/prettify-material-icon-for "#+RESULTS:" "receipt")

  (kdz/prettify-material-icon-for "#+BEGIN_EXAMPLE" "description")
  (kdz/prettify-material-icon-for "#+END_EXAMPLE" "expand_less")

  (kdz/prettify-material-icon-for "#+BEGIN_QUOTE" "format_quote")
  (kdz/prettify-material-icon-for "#+END_QUOTE" "expand_less")

  (kdz/prettify-material-icon-for "#+TITLE:" "title")
  (kdz/prettify-material-icon-for "#+AUTHOR:" "person")
  (kdz/prettify-material-icon-for "#+OPTIONS:" "settings")
  (kdz/prettify-material-icon-for "#+NAME:" "info")
  (kdz/prettify-material-icon-for "#+TODO:" "check_circle")

  (kdz/prettify-material-icon-for "[ ]" "check_box_outline_blank")
  (kdz/prettify-material-icon-for "[-]" "indeterminate_check_box")
  (kdz/prettify-material-icon-for "[X]" "check_box")

  (prettify-symbols-mode))

(defun kdz/org-buffer-margins ()
  (setq left-margin-width 2)
  (setq right-margin-width 2))

(after! org
  (require 'ox-gfm nil t)
  (require 'ob-restclient)
  (require 'ob-http)
  (require 'org-pretty-table)

  (add-hook 'org-mode-hook #'kdz/prettify-org-material)
  (add-hook 'org-mode-hook #'kdz/writing-fill-column)
  (add-hook 'org-mode-hook #'kdz/org-buffer-margins)
  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'org-mode-hook 'org-pretty-table-mode)

  (setq org-ellipsis (all-the-icons-material "unfold_more"))

  (map! :map org-mode-map :localleader "i" nil)
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
        :localleader
        (:prefix ("i" . "Insert")
        :desc "Insert Heading" "h" #'org-insert-heading
        :desc "Insert Subheading" "s" #'org-insert-subheading))

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
