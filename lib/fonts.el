(defvar kdz--mono-sans-font-list
  (list (font-spec :name "Berkeley Mono")
        (font-spec :name "Iosevka Comfy")
        (font-spec :name "Menlo")))

(defvar kdz--mono-serif-font-list
  (list (font-spec :name "Iosevka Comfy Motion")
        (font-spec :name "Courier New")
        (font-spec :name "Menlo")))

(defvar kdz--variable-pitch-font-list
  (list (font-spec :name "Iosevka Aile")))

(defvar kdz--unicode-font-list
  (list (font-spec :name "Iosevka Term")
        (font-spec :name "Symbol")))

(defun kdz/find-and-use-font (candidates &optional size)
  (let ((found-font (seq-find #'find-font candidates)))
    (when size (font-put found-font :size size))
    found-font))
