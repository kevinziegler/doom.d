;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
;(package! pinned-package :pin nil)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! bazel)
(package! bookmark+)
(package! copy-as-format)
(package! dirvish)
(package! evil-args)
(package! fwb-cmds)
(package! git-link)
(package! info-colors)
(package! jq-mode)
(package! jupyter)
(package! lorem-ipsum)
(package! magit-delta)
(package! magit-org-todos)
(package! mermaid-mode)
(package! modern-fringes)
(package! page-break-lines)
(package! protobuf-mode)
(package! string-inflection)
;; (package! topspace)
(package! uuidgen)
(package! websocket) ; dependency of `org-roam-ui'
(package! which-key-posframe)
(package! wordel)

;; NOTE This needs an 'openscad' binary in emac's path, but the Homebrew cask
;;      does not install such a binary.  This can be fixed by linking 'openscad'
;;      in '$HOME/.bin' to the installed 'OpenSCAD.app'
(package! scad-preview)

(package! markdown-xwidget
  :recipe (:host github
           :repo "cfclrk/markdown-xwidget"
           :files (:defaults "resources")))

;; Extra org-mode packages (MELPA)
(package! doct)
(package! ob-http)
(package! ob-mermaid)
(package! org-appear)
(package! org-autolist)
(package! org-jira)
(package! org-mac-link)
(package! org-modern)
(package! org-sticky-header)
;; (package! org-super-agenda)
(package! ox-clip)
(package! ox-gfm)
(package! valign)

;; Extra org-mode packages (Github)

(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

;; Themes.  Lots of themes.
(package! afternoon-theme)
(package! apropospriate-theme)
(package! arc-dark-theme)
(package! busybee-theme)
(package! clues-theme)
(package! darkmine-theme)
(package! darktooth-theme)
(package! flatland-theme)
(package! flatui-theme)
(package! gotham-theme)
(package! immaterial-theme)
(package! kaolin-themes)
(package! kooten-theme)
(package! madhat2r-theme)
(package! material-theme)
(package! mbo70s-theme)
(package! obsidian-theme)
(package! overcast-theme)
(package! planet-theme)
(package! reykjavik-theme)
(package! slime-theme)
(package! smyx-theme)
(package! soft-charcoal-theme)
(package! subatomic256-theme)
(package! sublime-themes)
(package! tao-theme)
(package! tron-legacy-theme)
(package! twilight-theme)
