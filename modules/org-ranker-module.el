(use-package org-ranker
  :straight (org-ranker :type git :host github :repo "CSJ7701/org-ranker"))

(defhydra org-ranker-hydra (:color blue :hint nil)
  "Org Ranker Actions: "
  ;; Basic Actions
  ("s" org-ranker-sort "Process Rules" :column "Common")
  ("b" org-ranker-set-base-score "Set Entry's Base Score" :column "Common")
  ("r" org-ranker-add-rule "Add Rule" :column "Common")
  ("x" org-ranker-add-exclude "Add Exclude" :column "Common")
  ("h" org-ranker-add-highlight "Add Highlight" :column "Common")
  ("p" org-ranker-examine-property "Examine Property" :column "Common")
  ("P" org-ranker-list-properties "List All Properties" :column "Common")
  ;; Manual
  ("mh" org-ranker-manual-highlight "Highlight Entry" :column "Manual")
  ("mH" org-ranker-remove-highlight "Remove Highlight on Entry" :column "Manual")
  ("mp" org-ranker-move-headline-up "Move Up" :column "Manual")
  ("mn" org-ranker-move-headline-down "Move Down" :column "Manual")
  ("ma" org-ranker-move-headline-start "Move to Start" :column "Manual")
  ("me" org-ranker-move-headline-end "Move to End" :column "Manual")
  ("mP" org-ranker-move-headline-up-n "Move Up N Lines" :column "Manual")
  ("mN" org-ranker-move-headline-down-n "Move Down N Lines" :column "Manual")
  ;; Manual Actions
  ("]" org-ranker-exclude "Process New Excludes" :column "Actions")
  ("[" org-ranker-unexclude "Remove Old Excludes" :column "Actions")
  ("{" org-ranker-highlight "Process New Highlights" :column "Actions")
  ("}" org-ranker-remove-highlights "Remove Old Highlights" :column "Actions")
  ("'" org-ranker-sort-headlines "Sort Headlines by Score" :column "Actions")
  ("\"" org-ranker-populate-scores "Populate Scores" :column "Actions")
  ;; Import
  ("c" org-ranker-import-csv "Import CSV File" :column "Import"))
