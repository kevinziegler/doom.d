(defun brew-prefix ()
  "Get the Homebrew installation prefix."
  (with-temp-buffer
    (call-process "brew" nil (current-buffer) nil "--prefix")
    (s-trim (buffer-string))))

(defun brew-bin-path ()
  "Get the Homebrew bin base path"
  (expand-file-name "bin" (brew-prefix)))

(defun brew-bin (executable)
  "Get the path of a homebrew-installed binary"
  (expand-file-name executable (brew-bin-path)))
