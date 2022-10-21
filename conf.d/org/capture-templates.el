(defun kdz/org-capture-template (fname)
  "Generate a path to the capture template named <FNAME>.org.tpl"
  (concat doom-user-dir "capture-templates/" fname ".org.tpl"))

(defvar kdz-db-engines (list "MySQL" "PostgreSQL"))

(defun kdz/select-db-engine ()
  (downcase (completing-read "Select Database Type:" kdz-db-engines)))

;; Roam templates to create
;; - One-off Meeting Notes
;;   - What 'types' of meetings can I define?
;; - Recurring meetings:
;;   - Team lead meetings
;; - Ticket Notes
;; - 'Dossiers'      :: per person; place to consolidate 1-1s, feedback, etc
;; - "Thought" entry :: Not necessary actionable, but worth further review in
;;                      the future
;; - "Idea" entry    :: An idea that could be actionable, but needs to be
;;                      further developed first
;; - "Reading List"  :: Articles that I think would be useful instructional
;;                      material to give to someone in the future

(after! org-capture
  (setq org-capture-templates
        (doct '(("One-on-One"
                 :keys "o"
                 :file "~/notes/one-on-one-discussions.org"
                 :prepend t
                 :function (lambda ()
                             (let ((org-goto-interface 'outline-path-completion)
                                   (org-goto-max-level 1))
                               (org-goto)))
                 :children (("Discussion Topic"
                             :keys "t"
                             :template ("* DISCUSS %?"
                                        ":properties:"
                                        ":created: %t"
                                        ":end:"))))
                ("Retro Discussions"
                 :keys "r"
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
                ("To-do Items"
                 :keys "t"
                 :children (("Personal To-do"
                             :keys "p"
                             :file "~/notes/personal/todos.org"
                             :template "* TODO %?\n%t\nDEADLINE: %^{Due on}t")
                            ("Personal To-do (Deadline)"
                             :keys "P"
                             :file "~/notes/personal/todos.org"
                             :template "* TODO %?")
                            ("Work To-do"
                             :keys "w"
                             :file "~/notes/work-todos.org"
                             :template "* TODO %?")
                            ("Work To-do (Deadline)"
                             :keys "W"
                             :file "~/notes/work-todos.org"
                             :template "* TODO %?\n%t\nDEADLINE %^{Due on}t")
                            ("Create Ticket"
                             :keys "t"
                             :file "~/notes/work-todos.org"
                             :template "* TODO Create Ticket: %?\n%t")
                            ("Team lead To-do"
                             :keys "l"
                             :file "~/notes/work-todos.org"
                             :template "* TODO %? :lead_work:\n")
                            ("Team lead To-do (Deadline)"
                             :keys "l"
                             :file "~/notes/work-todos.org"
                             :template "* TODO %? :lead_work:\n%t\nDEADLINE %^{Due on}t")))

                ("HTTP Notebooks"
                 :keys "h"
                 :file "~/notes/http-notebooks.org"
                 :children (("Create notebook"
                             :keys "h"
                             :template ("* %^{Notebook-Name|New Notebook}"
                                         ":properties:"
                                         ":header-args:http: :host %^{Host/Port|localhost:8080}"
                                         ":end:"))
                            ("Create collection"
                             :keys "c"
                             :function (lambda ()
                                         (let ((org-goto-interface 'outline-path-completion)
                                               (org-goto-max-level 1))
                                           (org-goto)))
                             :prepend t
                             :template ("* %^{Collection Name|New Collection}"
                                         "#+begin_src"
                                         "%?"
                                         "#+end_src"))
                            ("Create request"
                             :keys "r"
                             :type plain
                             :function (lambda ()
                                         (let ((org-goto-interface 'outline-path-completion)
                                               (org-goto-max-level 2))
                                           (org-goto)))
                             :template ("#+begin_src"
                                        "%?"
                                        "#+end_src"))))))))

;; (after! org
;;   (add-to-list 'org-capture-templates
;;                '("f"
;;                 "Jira Feature Ticket Note"
;;                 entry
;;                 (file "~/notes/tickets.org")
;;                 (file (kdz/org-capture-template "feature-ticket"))))

;;   (add-to-list 'org-capture-templates
;;                '("b"
;;                 "Jira Bug Ticket Note"
;;                 entry
;;                 (file "~/notes/tickets.org")
;;                 (file (kdz/org-capture-template "bug-ticket"))))

;;   (add-to-list 'org-capture-templates
;;                '("e"
;;                  "One-on-one Topic"
;;                  entry
;;                  (file+function "~/notes/one-on-one-discussions.org"
;;                                 kdz/capture-one-on-one-topic)
;;                  "* DISCUSS %?\n%t\n")))
