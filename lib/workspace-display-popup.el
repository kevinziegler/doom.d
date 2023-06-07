;;; lib/workspace-display-popup.el -*- lexical-binding: t; -*-

(defface workspace-popup-base
  '((t . (:height 1.1)))
  "Base face for the workspace popup list")

(defface workspace-popup-number
  '((t . (:inherit workspace-popup-base :foreground "linkColor")))
  "Face for displaying numbers on the workspace list popup")

(defface workspace-popup-current
  '((t . (:inherit workspace-popup-base
          :weight bold
          :foreground "selectedTextColor"
          :background "selectedTextBackgroundColor")))
  "Face for select workspace in workspace popup list")

(defface workspace-popup-padding
  '((t . (:height 0.5)))
  "Face used to emulate vertical padding in the popup window")


(defun workspace-popup-vertical-padding (&optional fill-string)
  "Empty line to provide vertical padding for workspace display popup"
  (propertize (or fill-string " \n" )'face 'workspace-popup-padding))

(defun kdz/workspace-list (&optional names)
  (let ((names (or names (+workspace-list-names)))
        (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (kdz/workspace-list-line name i current-name))
     "\n")))

(defun kdz/workspace-list-line (name index current-workspace-name)
  (let ((fmt-spec (propertize "[%s] %s" 'face 'workspace-popup-base))
        (number (propertize (int-to-string (+ 1 index)) 'face 'workspace-popup-number))
        (workspace (propertize name 'face (if (equal current-workspace-name name)
                                    'workspace-popup-current
                                  'workspace-popup-base))))
    (format fmt-spec number workspace)))

(defun kdz/workspaces-display-posframe ()
  (interactive)
  ;; TODO How to hide the posframe on `ESC' rather than waiting for timeout?
  (posframe-show " *workspaces-list*"
                 :string (concat (workspace-popup-vertical-padding " \n")
                                 (kdz/workspace-list)
                                 (workspace-popup-vertical-padding "\n "))

                 :font "Berkeley Mono 20"
                 :poshandler #'posframe-poshandler-frame-center
                 :border-color "black"
                 :left-fringe 10
                 :right-fringe 10
                 :border-width 2
                 :timeout 2))
