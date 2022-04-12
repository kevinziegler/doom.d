;;; sprints.el -*- lexical-binding: t; -*-
;;;
(defvar sprints/cadence-weeks 2)
(defvar sprints/start-day 2) ;; Day of the week the sprint starts on
(defvar sprints/start-on-week 1) ;; Week number of the first sprint of the year

(defun sprints/sprint-calendar-days ()
  (* sprints/cadence-weeks 7))

(defun sprints/current-sprint-number ()
  (/ (calendar-day-number (calendar-current-date))
     (sprints/sprint-calendar-days)))

(defun sprints/current-sprint-start ()
  (* (sprints/current-sprint-number) (sprints/sprint-days)))

(defun sprints/current-sprint-end ()
  (+ (sprints/current-sprint-start) (sprints/sprint-calendar-days)))

(defun sprints/org-date-string-to-calendar (date-as-string)
  (let* ((org-date-parsed (org-parse-time-string
                           (org-read-date nil nil date-as-string)))
         (day (nth 3 org-date-parsed))
         (month (nth 4 org-date-parsed))
         (year (nth 5 org-date-parsed)))
        (list month day year)))

(defun sprints/sprint-file (sprint-number))

(defun sprints/days-between (start-date end-date)
  (let ((parsed-start (calendar-absolute-from-gregorian start-date))
        (parsed-end (calendar-absolute-from-gregorian end-date)))
    (- parsed-end parsed-start)))

