
(after! org
  ;; TODO Check these against existing org-config

  (appendq! +ligatures-extra-symbols
            `(:checkbox      ""
              :pending       ""
              :checkedbox    ""
              :created       ""
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
