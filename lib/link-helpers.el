(defvar hosted-gitlab-host nil)
(defvar jira-host nil)

(defmacro kdz/follow-suffix-link (base-url)
  `(lambda (suffix) (browse-url (string-join (list ,base-url suffix) "/") )))
