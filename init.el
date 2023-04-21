;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       :completion
       (company +childframe)
       (vertico +icons +childframe)

       :ui
       doom
       doom-dashboard
       doom-quit
       (emoji +unicode)
       hl-todo
       hydra
       indent-guides
       (ligatures +extra)
       modeline
       nav-flash
       ophints
       (popup +defaults)
       (treemacs +lsp)
       vc-gutter
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       format
       rotate-text
       snippets
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs (dired +icons) electric undo vc
       :term vterm
       :checkers syntax

       :tools
       (debugger +lsp)
       direnv
       (docker +lsp)
       editorconfig
       ein
       (eval +overlay)
       (lookup +docsets)
       lsp
       magit
       pdf
       ;;prodigy           ; FIXME managing external services & code builders
       terraform
       tree-sitter

       :lang
       data
       emacs-lisp
       (go +lsp)
       (json +lsp)
       (java +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (kotlin +lsp)
       (lua +lsp)
       (markdown +grip)
       (org +dragndrop +pandoc +present)
       plantuml
       (python +lsp +pyright +poetry +tree-sitter)
       rest
       (sh +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (yaml +lsp)

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app calendar
       :config (default +bindings +smartparens)
       :os macos tty)
