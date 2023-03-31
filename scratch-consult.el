;;; scratch.el -*- lexical-binding: t; -*-

(defun kdz/dummy-list()
  (list "foo" "bar" "baz" "qux"))

(defun kdz/dummy-action (item)
  (message "KDZ Selected %s" item))

(defvar consult--kdz--source-things
  `(:name "thigns to get"
    :category "stuff"
    :action ,#'kdz/dummy-action
    :items ,#'kdz/dummy-list))

(defun kdz/consult-things ()
  (interactive)
  (consult--multi '(consult--kdz--source-things)
                  :sort nil))
