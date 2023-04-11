(defun kdz/org-babel-plantuml-format-var (var-value)
  (cond ((numberp var-value) (number-to-string var-value))
        ((booleanp var-value) (if var-value "true" "false"))
        (t (format "\"%s\"" var-value))))

(defun kdz/org-babel-variable-assignments:plantuml (params)
  "Return a list of PlantUML statements assigning the block's variables.

PARAMS is a property list of source block parameters, which may
contain multiple entries for the key `:var'.  `:var' entries in PARAMS
are expected to be scalar variables."
  (mapcar
   (lambda (pair)
     (format "!$%s=%s"
             (car pair)
             (kdz/org-babel-plantuml-format-var (cdr pair))))
   (org-babel--get-vars params)))

(defun kdz/ob-plantuml-directive-theme (params)
  "Process :theme and :theme-from header args into an ob-plantuml block"
  (let ((theme (cdr (assq :theme params)))
        (theme-from (cdr (assq :theme-from params))))
    (cond ((and theme theme-from) (format "!theme %s from %s"
                                          theme
                                          (expand-file-name theme-from)))
          (theme (format "!theme %s" theme))
          (t ""))))

(defun kdz/ob-plantuml-wrap-body (type &rest body-parts)
  "Wrap BODY-PARTS with plantuml @start/@end anchors for TYPE"
  (string-join
   (append (list (concat "@start" type))
           body-parts
           (list (concat "@end" type)))
   "\n"))

(defun kdz/org-babel-plantuml-make-body (body params)
  "Pre-process ob-plantuml BODY with PARAMS

This extends the default ob-plantuml behavior to support new header args:
 - :type :: The type of diagram to apply @start/@end anchors
 - :theme :: The plantuml theme to use for the diagram
 - :theme-from :: The path to find the theme specified by :theme

Additionally, :var VAR=VAL header args are processed at $!VAR=VAL instead of
using the !define VAR VAL syntax"
  (let ((theme-directive (kdz/ob-plantuml-directive-theme params))
        (type (or (cdr (assq :type params)) "uml"))
        (full-body
         (org-babel-expand-body:generic
          body params (org-babel-variable-assignments:plantuml params))))
    (if (string-prefix-p "@start" body t) full-body
      (kdz/ob-plantuml-wrap-body type theme-directive full-body))))
