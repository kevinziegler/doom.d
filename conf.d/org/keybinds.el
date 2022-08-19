(after! org
  (map! :map org-mode-map
        :localleader
        :desc "Schedule" "S" #'org-schedule)

  (map! :map org-mode-map
        :localleader
        :desc "Outline" "O" #'org-ol-tree
        (:prefix ("s" . "Subtrees")
        :desc "Cut subtree" "d" #'org-cut-subtree
        :desc "Promote subtree" "h" #'org-promote-subtree
        :desc "Demote subtree" "l" #'org-demote-subtree
        :desc "Move Subtree Up" "k" #'org-move-subtree-up
        :desc "Move Subtree Down" "j" #'org-move-subtree-down))

  (map! :map org-mode-map
        :leader
        (:prefix "b"
         :desc "New empty ORG buffer" "o" #'evil-buffer-org-new)
        (:prefix "i"
         :desc "Heading" "h" #'org-insert-heading
         :desc "Parent Heading" "H" #'kdz/org-insert-heading-up
         :desc "Subheading" "s" #'org-insert-subheading
         :desc "Link" "l" #'org-insert-link)
        (:prefix ("ia" . "Application Links")
         :desc "Active Firefox Tab" "f" #'org-mac-link-firefox-insert-frontmost-url
         :desc "Finder Item" "F" #'org-mac-link-finder-insert-selected)))
