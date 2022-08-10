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

(package! ample-theme)
(package! git-link)
(package! kaolin-themes)
(package! magit-org-todos)
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
(package! org-jira)
(package! tron-legacy-theme)
(package! ob-http)
(package! protobuf-mode)
(package! ob-mermaid)
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))
(package! etrace :recipe (:host github :repo "aspiers/etrace"))

(package! info-colors :pin "47ee73cc19b1")
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))

(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fe")

(package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
  :pin "cd1aefd56f648d32a25aae672ac1ab90893c0133")
(package! websocket :pin "fda4455333309545c0787a79d73c19ddbeb57980") ; dependency of `org-roam-ui'

(package! topspace)
(package! mermaid-mode)

(package! org-super-agenda)
(package! good-scroll)

;; NOTE This needs an 'openscad' binary in emac's path, but the Homebrew cask
;;      does not install such a binary.  This can be fixed by linking 'openscad'
;;      in '$HOME/.bin' to the installed 'OpenSCAD.app'
(package! scad-preview)

(package! bookmark+)

(package! jupyter)

(package! string-inflection)
;;(package! ob-grpc :recipe (:host github :repo "shsms/ob-grpc"))
