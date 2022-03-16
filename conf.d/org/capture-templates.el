(defun kdz/org-capture-template (fname)
  "Generate a path to the capture template named <FNAME>.org.tpl"
  (concat doom-private-dir "capture-templates/" fname ".org.tpl"))

(after! org
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
                (file "~/notes/retro.org")
                "* TODO Discuss in next retro: %?")))
