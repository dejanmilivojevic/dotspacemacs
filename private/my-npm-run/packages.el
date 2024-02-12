(defconst my-npm-run-packages
  '((npm-run :location local)))

(defun my-npm-run/init-npm-run ()
  (use-package npm-run
    :commands (npm-run)
    :init
    (progn
      (spacemacs/declare-prefix "p" "project")
      (spacemacs/set-leader-keys "pn" 'npm-run))))
