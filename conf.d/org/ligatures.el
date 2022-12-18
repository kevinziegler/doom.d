
(after! org
  (appendq! +ligatures-extra-symbols
            `(:checkbox      ""
              :pending       ""
              :checkedbox    ""
              :created       ""
              :closed        ""
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :property      ""
              :header        "›"
              :properties    ""
              :end           ""
              :scheduled     ""
              :deadline      ""))

  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :created       ":created:"
    :closed        "CLOSED:"
    :closed        "closed:"
    :properties    ":PROPERTIES:"
    :properties    ":properties:"
    :end           ":END:"
    :end           ":end:"
    :filetags      "#+filetags:"
    :scheduled     "SCHEDULED:"
    :scheduled     "scheduled:"
    :deadline      "DEADLINE:"
    :deadline      "deadline:")
  (plist-put +ligatures-extra-symbols :name "⁍"))
