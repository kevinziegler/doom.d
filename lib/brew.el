;; FIXME Use (call-process) as in kdz/asdf-which to avoid creating a throwaway
;; buffer that holds the shell output
(defun brew-prefix ()
  (if (eq (shell-command "which brew") 0)
      (string-trim (shell-command-to-string "brew --prefix"))
    (error "Homebrew is not available")))

(defun brew-bin (bin)
  "Given a BIN, generate the path for this bin assuming the homebrew prefix"
  (f-join (brew-prefix) "bin" bin))
