;;; $DOOMDIR/config-org.el -*- lexical-binding: t; -*-

(setq org-directory "~/notes/"
      org-hide-emphasis-markers t
      org-journal-dir "~/notes/journal/"
      org-default-notes-file "~/notes/notes.org"
      org-insert-heading-respect-content t
      deft-directory "~/notes")

(after! org
  (require 'ox-gfm nil t)
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

  (map! :map org-mode-map :localleader "i" nil)
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
                (file "~/.dotfiles/capture-templates/feature-ticket.org.tpl")))

  (add-to-list 'org-capture-templates
               '("b"
                "Jira Bug Ticket Note"
                entry
                (file "~/notes/tickets.org")
                (file "~/.dotfiles/capture-templates/bug-ticket.org.tpl")))

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

(add-hook 'org-mode-hook #'kdz/writing-fill-column)
