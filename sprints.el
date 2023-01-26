;;; sprints.el -*- lexical-binding: t; -*-
;;;
(defvar sprints/initial-start-date nil)
(defvar sprints/duration-weeks 2)
(defvar sprints/planning-offset "mon")
(defvar sprints/refinement-offset "++mon")
(defvar sprints/retro-offset "++2mon")

(setq sprints/initial-start-date "2022-07-18")

(defun sprints/get-offset-date (from-string date-string)
  (org-read-date nil nil from-string nil (org-time-string-to-time date-string)))

(defun sprints//current-date ()
    (org-time-string-to-absolute (org-read-date nil nil "+0")))

(defun sprints/sprint-start (&optional sprint-offset)
  "Compute the start date of a sprint."
  (let* ((start-date (org-time-string-to-absolute sprints/initial-start-date))
         (current-date
          (org-time-string-to-absolute (org-read-date nil nil "+0")))
         (elapsed-weeks-since-initial (/ (- current-date start-date) 7))
         (sprint-number (/ elapsed-weeks-since-initial sprints/duration-weeks))
         (sprint-extra-offset (or sprint-offset 0))
         (sprint-offset-weeks
          (* (+ sprint-number sprint-extra-offset) sprints/duration-weeks)))
    (sprints/get-offset-date (format "++%dw" sprint-offset-weeks)
                              sprints/initial-start-date)))

(defun sprints/sprint-end-date (&optional sprint-offset)
  (sprints/get-offset-date (format "++%dw" sprints/duration-weeks)
                            (sprints/sprint-start sprint-offset)))

(defun sprints/refinement-date (&optional sprint-offset)
  (sprints/get-offset-date sprints/refinement-offset
                            (sprints/sprint-start sprint-offset)))

(defun sprints/planning-date (&optional sprint-offset)
  (sprints/get-offset-date sprints/planning-offset
                            (sprints/sprint-start sprint-offset)))

(defun sprints/retro-date (&optional sprint-offset)
  (sprints/get-offset-date sprints/retro-offset
                            (sprints/sprint-start sprint-offset)))

(defun sprints/upcoming-refinement-date ()
  "Get the next refinement meeting date in the future")

(defun sprints/last-refinement-date ()
  "Get the last refinement meeting date")
