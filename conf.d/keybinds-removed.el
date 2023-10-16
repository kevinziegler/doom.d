;;; conf.d/keybinds-removed.el -*- lexical-binding: t; -*-

(map! :leader :nv
      ","  nil ;; Switch buffer
      "."  nil ;; Find file
      ":"  nil ;; M-x
      "<"  nil ;; Switch buffer
      "`"  nil ;; Switch to last buffer

      "bO" nil ;; Kill other buffers
      "bZ" nil ;; Kill buried buffers
      "b[" nil ;; previous buffer
      "b]" nil ;; next buffer:
      "bu" nil ;; Save buffer as root
      "bz" nil ;; Bury buffer

      "cc" nil ;; compile
      "cC" nil ;; Recompile
      "cd" nil ;; Jump to definition (mimics `gd')
      "cD" nil ;; Jump to references (mimics `gD')

      ;; File-related actions
      "fc" nil ;; Edit project editorconfig
      "fd" nil ;; Find directory
      "fe" nil ;; Find file in emacs directory
      "fE" nil ;; Browse emacs directory
      "fF" nil ;; Find file from here
      "fl" nil ;; Locate file
      "fp" nil ;; Browse private config
      "fP" nil ;; Browse private config
      "fr" nil ;; Find recent file
      "fs" nil ;; Save file
      "fS" nil ;; Save file as

      ;; VC-related actions
      "g." nil ;; vc-gutter
      "g/" nil ;; Magit Dispatch
      "gD" nil ;; Magit file delete
      "gF" nil ;; Magit fetch
      "gG" nil ;; Magit status HERE
      "gR" nil ;; Git revert file
      "gS" nil ;; Git stage file
      "gU" nil ;; Magit unstage file
      "g[" nil ;; Jump to previous hunk
      "g]" nil ;; Jump to next hunk
      "gb" nil ;; Magit switch branch
      "gr" nil ;; Git revert hunk at point
      "gs" nil ;; Git stage hunk at point
      "gc" nil ;; "Create" menu
      "g'" nil ;; "Forge" dispatch

      ;; Note-related actions
      "nC" nil ;; Cancel current org clock
      "nc" nil ;; Toggle last org-clock
      "nd" nil ;; Open Deft
      "no" nil ;; Active org clock

      ;; "Open" actions
      "o-" nil ;; dired
      "oA" nil ;; Org Agenda Dispatch (Duplicates `o -> a -> a')
      "oD" nil ;; Docker
      "oL" nil ;; Send to Project Launchbar
      "oU" nil ;; Send project to Transmit
      "ou" nil ;; Send to Transmit
      "ol" nil ;; Send to Launchbar


      ;; Project Actions
      "p>" nil ;; Browse other project
      "p." nil ;; Browse project
      "pb" nil ;; Switch to project buffer
      "pk" nil ;; Kill project buffers
      "pr" nil ;; Find recent file in project

      ;; Search Actions
      "sm" nil ;; Jump to bookmark
      "sr" nil ;; Jump to mark
      "sf" nil ;; Locate File
      "sj" nil ;; "Jump List"
      "se" nil ;; Search emacs.d
      "sb" nil ;; Search current buffer
      "sT" nil ;; Search Thesaurus
      "sK" nil ;; Search in all docsets
      "sB" nil ;; Search all open buffers
      "st" nil ;; Search Dictionary

      ;; Window-related actions - Doom sets up A LOT by default
      "w :" nil
      "w <down>" nil
      "w <left>" nil
      "w <right>" nil
      "w <up>" nil
      "w C-<down>" nil
      "w C-<left>" nil
      "w C-<right>" nil
      "w C-<up>" nil
      "w C-S-h" nil
      "w C-S-j" nil
      "w C-S-k" nil
      "w C-S-l" nil
      "w C-S-r" nil
      "w C-S-s" nil
      "w C-S-w" nil
      "w C-_" nil
      "w C-b" nil
      "w C-f" nil
      "w C-h" nil
      "w C-j" nil
      "w C-k" nil
      "w C-l" nil
      "w C-n" nil
      "w C-o" nil
      "w C-p" nil
      "w C-q" nil
      "w C-s" nil
      "w C-t" nil
      "w C-u" nil
      "w C-v" nil
      "w C-w" nil
      "w C-x" nil
      "wW" nil
      "wb" nil
      "wc" nil
      "wp" nil
      "ws" nil
      "wt" nil
      "wv" nil
      "ww" nil

      )

(map! :leader (:prefix "TAB"
                       "N" nil ;; new named workspace
                       "R" nil ;; restore last session
                       "." nil ;; Switch workspace (replaces by TAB -> SPC)
                       "[" nil ;; previous workspace
                       "]" nil ;; next workspace
                       "x" nil ;; delete session
                       "s" nil ;; Save workspace to file
                       "l" nil ;; Load workspace from file
                       ))
