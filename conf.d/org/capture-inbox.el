(after! org-capture
  (setq org-capture-templates
        (doct '(("Work Note"
                 :keys "w"
                 :icon ("work" :set "material" :color "yellow")
                 :file "~/notes/inbox-work.org"
                 :children (("To-Do Item"
                             :keys "t"
                             :prepend t
                             :icon ("checklist" :set "octicon" :color "green")
                             :headline "Action Items"
                             :template "* TODO %?")
                            ("To-Do Item (Deadline)"
                             :keys "T"
                             :prepend t
                             :icon ("checklist" :set "octicon" :color "red")
                             :headline "Action Items"
                             :template "* TODO %?\n%t\nDEADLINE: %^{Due on}t")
                            ("Idea/Question"
                             :keys "i"
                             :icon ("question" :set "octicon" :color "blue")
                             :headline "Ideas/Questions"
                             ;; TODO Add date created to template
                             :template "* %?")
                            ("Retro Discussions"
                             :keys "r"
                             :icon ("history" :set "material" :color "blue")
                             :file "~/notes/retros.org"
                             :template ("* %{retro-category}: %?"
                                        ":properties:"
                                        ":created: %t"
                                        ":end:")
                             :children (("Start Doing" :keys "s" :retro-category "START")
                                        ("Stop Doing"  :keys "S" :retro-category "STOP")
                                        ("Keep Doing"  :keys "k" :retro-category "KEEP")
                                        ("Observation" :keys "o" :retro-category "OBSERVATION")
                                        ("Shoutout/Accolade"
                                         :keys "a"
                                         :template "* SHOUTOUT %^{Person(s)}: %^{Shoutout For}\n%t")))
                            ("Standup Topic"
                             :keys "s"
                             :icon ("question" :set "octicon" :color "blue")
                             :headline "Standup Topics"
                             ;; TODO Add date created to template
                             :template ("* %?"
                                        "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))"))))
                ("Personal Note"
                 :keys "p"
                 :icon ("work" :set "material" :color "yellow")
                 :file "~/notes/inbox-work.org"
                 :children (("To-Do Item"
                             :keys "t"
                             :prepend t
                             :icon ("checklist" :set "octicon" :color "green")
                             :headline "Action Items"
                             :template "* TODO %?")
                            ("To-Do Item (Deadline)"
                             :keys "T"
                             :prepend t
                             :icon ("checklist" :set "octicon" :color "red")
                             :headline "Action Items"
                             :template "* TODO %?\n%t\nDEADLINE: %^{Due on}t")
                            ("Idea/Question"
                             :keys "i"
                             :icon ("question" :set "octicon" :color "blue")
                             :headline "Ideas/Questions"
                             ;; TODO Add date created to template
                             :template "* %?")
                            ("Shopping List Item"
                             :keys "s"
                             :icon ("shopping_cart" :set "material" :color "blue")
                             :headline "Shopping List"
                             :type checkitem)))
                ("One-on-One"
                 :keys "o"
                 :file "~/notes/one-on-one-discussions.org"
                 :prepend t
                 :icon  ("person" :set "material" :color "blue")
                 :function (lambda ()
                             (let ((org-goto-interface 'outline-path-completion)
                                   (org-goto-max-level 1))
                               (org-goto)))
                 :children (("Discussion Topic"
                             :keys "t"
                             :icon ("comment" :set "material" :color "blue")
                             :template ("* DISCUSS %?"
                                        ":properties:"
                                        ":created: %t"
                                        ":end:"))))))))
