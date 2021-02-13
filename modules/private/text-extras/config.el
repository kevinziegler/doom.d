;;; private/text-extras/config.el -*- lexical-binding: t; -*-

;;; Code:
(map! :leader "x" nil)
(map! :leader
      (:prefix-map ("x" . "Text Manipulation")
        :desc "Upcase region" "U" #'upcase-region
        :desc "Downcase region" "u" #'downcase-region
        (:prefix-map ("c" . "Commenting")
          :desc "(Un)comment Lines" "l" #'evilnc-comment-or-uncomment-lines)
        (:prefix-map ("y" . "Copy as Format")
          :desc "Copy for Slack" "s" #'copy-as-format-slack
          :desc "Copy for Jira"  "j" #'copy-as-format-jira
          :desc "Copy as HTML"   "h" #'copy-as-format-html)
        (:prefix-map ("g" . "Generate")
         (:prefix-map ("l" . "Lorem Ipsum")
          :desc "Paragraph" "p" #'Lorem-ipsum-insert-paragraphs
          :desc "Sentence"  "s" #'Lorem-ipsum-insert-sentences
          :desc "List"      "l" #'Lorem-ipsum-insert-list)
         :desc "UUID" "u" #'uuidgen)
        (:prefix-map ("i" . "Inflections")
          :desc "snake_case" "s" #'string-inflection-underscore
          :desc "camelCase"  "c" #'string-inflection-lowercamelcase
          :desc "PascalCase" "p" #'string-inflection-camelcase
          :desc "kebab-case" "k" #'string-inflection-kebab-case
          :desc "UPPER_CASE" "u" #'string-inflection-upcase)))
;;; config.el ends here
