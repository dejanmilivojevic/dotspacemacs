(require 'projectile)
(require 'json)
(require 'helm)

(defcustom npm-terminal-command "gnome-terminal"
  "The terminal command to run npm tasks."
  :type 'string)

(defvar npm-last-task nil
  "The last NPM task that was run.")
(defvar npm-last-params nil
  "The parameters used in the last NPM task.")

(defun npm-get-tasks ()
  "Retrieve the list of npm tasks from the package.json."
  (let* ((project-root (projectile-project-root))
         (package-file (concat project-root "package.json"))
         (package-data (json-read-file package-file))
         (scripts-alist (cdr (assoc 'scripts package-data))) ; Get the alist of scripts
         (sorted-scripts (sort scripts-alist (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))) ; Sort scripts by name
    sorted-scripts))

(defun npm-run-task-in-terminal (task &optional params)
  "Run the given TASK with PARAMS in the configured terminal, displaying TASK in the title."
  (setq npm-last-task task)
  (setq npm-last-params params)
;;   (let ((cmd (format "%s -- bash -c 'echo -ne \"\\033]0;%s\\007\"; npm run %s %s; echo \"Press any key to wait, closing in 5sec...\"; read -t 5 -n 1 -s -r; if [ $? -eq 0 ]; then echo \"A key was pressed, waiting indefinitely...\"; read -n 1 -s -r; fi; exit'"
  (let ((cmd (format "%s -- bash -c 'echo -ne \"\\033]0;%s\\007\"; npm run %s %s; echo \"Waiting indefinitely...\"; read -n 1 -s -r; exit'"
                     npm-terminal-command task task (or params ""))))
    (start-process-shell-command "npm-terminal" nil cmd)))

(defun npm-run ()
  "Display npm tasks in a Helm menu and run the selected task(s) in a terminal.
Press F2 to input parameters. Supports selecting multiple tasks."
  (interactive)
  (let* ((tasks (npm-get-tasks))
         (helm-source (helm-build-sync-source "Npm Tasks"
                        :candidates (mapcar (lambda (task) (symbol-name (car task))) tasks)
                        :action (helm-make-actions
                                 "Run task" (lambda (selected-tasks)
                                              (dolist (task (helm-marked-candidates))
                                                (npm-run-task-in-terminal task)))
                                 "Run task with parameters" (lambda (task)
                                                              (let ((params (read-string "Parameters: ")))
                                                                (npm-run-task-in-terminal task params)))
                                 "Repeat last task with parameters" (lambda (_)
                                                                      (when npm-last-task
                                                                        (npm-run-task-in-terminal npm-last-task npm-last-params)))))))
    (helm :sources helm-source
          :buffer "*helm npm*"
          :keymap (let ((map (make-sparse-keymap)))
                    (set-keymap-parent map helm-map)
                    (define-key map (kbd "<C-return>") 'helm-select-2nd-action-in-minibuffer)
                    map)
          :candidate-number-limit 9999))) ; Optional: Increase if you have a lot of tasks

(defun helm-run-npm-task-with-params ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (task)
       (let ((params (read-string "Parameters: ")))
         (npm-run-task-in-terminal task params))))))

(define-key helm-map (kbd "<C-return>") nil)
;; (define-key helm-map (kbd "<f2>") 'helm-run-npm-task-with-params)

(provide 'npm-run)
