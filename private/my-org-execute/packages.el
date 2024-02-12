(defconst my-org-execute-packages
  '((org-execute :location local)))

(defun my-org-execute/init-org-execute ()
  (use-package org-execute
    ;; :demand t
    :commands (
               org-execute-block-and-get-results
               org-execute-block-and-get-results-projectile
               org-execute-get-runnable
               org-execute-get-runnable-projectile
               eshell/org-execute-block-and-get-results
               eshell/org-execute-block-and-get-results-projectile
               eshell/prj-run
               pcomplete/org-execute-block-and-get-results
               pcomplete/org-execute-block-and-get-results-projectile
               pcomplete/prj-run)
    ;; Optional: Here you can configure the package, e.g., set custom variables, keybindings, etc.
    ;; For example, to set keybindings, you might use:
    ;; (spacemacs/set-leader-keys "o e" #'execute-org-block-and-get-results)
    ))
