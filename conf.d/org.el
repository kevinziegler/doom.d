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

(defun kdz/iconify-org-token (icon-fn icon token)
  (push (cons (upcase token) (funcall icon-fn icon :face 'font-lock-keyword-face)) prettify-symbols-alist)
  (push (cons (downcase token) (funcall icon-fn icon)) prettify-symbols-alist))

(defun kdz/prettify-org-material ()
  "Set up prettify-org with all-the-icons symbols"
  (kdz/iconify-org-token #'all-the-icons-material "code" "#+BEGIN_SRC")
  (kdz/iconify-org-token #'all-the-icons-material "expand_less" "#+END_SRC")
  (kdz/iconify-org-token #'all-the-icons-material "receipt" "#+RESULTS:")

  (kdz/iconify-org-token #'all-the-icons-material "description" "#+BEGIN_EXAMPLE")
  (kdz/iconify-org-token #'all-the-icons-material "expand_less" "#+END_EXAMPLE")

  (kdz/iconify-org-token #'all-the-icons-material "format_quote" "#+BEGIN_QUOTE")
  (kdz/iconify-org-token #'all-the-icons-material "expand_less" "#+END_QUOTE")

  (kdz/iconify-org-token #'all-the-icons-material "title" "#+TITLE:")
  (kdz/iconify-org-token #'all-the-icons-material "person" "#+AUTHOR:")
  (kdz/iconify-org-token #'all-the-icons-fileicon "config" "#+PROPERTY:")
  (kdz/iconify-org-token #'all-the-icons-material "settings" "#+OPTIONS:")
  (kdz/iconify-org-token #'all-the-icons-material "info" "#+NAME:")
  (kdz/iconify-org-token #'all-the-icons-material "check_circle" "#+TODO:")

  (kdz/iconify-org-token #'all-the-icons-material "person" ":dbuser")
  (kdz/iconify-org-token #'all-the-icons-material "security" ":dbpassword")
  (kdz/iconify-org-token #'all-the-icons-octicon "database" ":database")
  (kdz/iconify-org-token #'all-the-icons-octicon "server" ":dbhost")

  (kdz/iconify-org-token #'all-the-icons-material "check_box_outline_blank" "[ ]")
  (kdz/iconify-org-token #'all-the-icons-material "indeterminate_check_box" "[-]")
  (kdz/iconify-org-token #'all-the-icons-material "check_box" "[X]")

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
  (add-hook 'org-mode-hook (lambda ()
                             (display-fill-column-indicator-mode nil)
                             (setq-local fill-column 120)))

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
