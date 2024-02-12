(require 'org)
(require 'ob)
(require 'projectile)
(require 'pcomplete)
(require 'table)

;; Customizable variable for the Org file name within the project directory
(defcustom org-execute-org-file-name "prj_utils.org"
  "Name of the Org file to execute blocks from within the project root."
  :type 'string)

(defmacro org-execute--projectile-wrapper (wrapper-name base-function file-var-name)
  "Define a new function WRAPPER-NAME that wraps BASE-FUNCTION.
FILE-VAR-NAME is a symbol for the variable name that will hold the file path.
The new function will automatically resolve the file path using Projectile and call BASE-FUNCTION."
  `(defun ,wrapper-name (&rest args)
     (let* ((project-root (projectile-project-root))
            (org-file-name org-execute-org-file-name)
            (full-path (expand-file-name org-file-name project-root)))
       (message "Resolved path: %s" full-path)
       (if project-root
           (if (file-exists-p ,file-var-name)
               (apply ',base-function ,file-var-name args)
             (error "File %s does not exist in the project root %s" ,file-var-name project-root))
         (error "Not in a Projectile project")))))

(defun org-execute-block-and-get-results-fn (file-name block-name)
  "Execute the block with BLOCK-NAME in the Org file FILE-NAME and return the results."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents file-name)
    (goto-char (point-min))
    ;; Insert #+CALL: line at the end of the file
    (goto-char (point-max))
    (insert (concat "\n#+CALL: " block-name))
    ;; Execute the block
    (org-ctrl-c-ctrl-c)
    ;; Move to the results
    (if (search-forward "#+RESULTS:" nil t)
        (progn
          (forward-line 1)
          (let ((start (point)))
            ;; Capture everything until the next Org structural element or buffer end
            (while (not (eobp))
              (forward-line 1))
            (buffer-substring-no-properties start (point))))
      (error "Results block for %s not found" block-name))))

(org-execute--projectile-wrapper org-execute-block-and-get-results-projectile-fn
                            org-execute-block-and-get-results-fn
                            full-path)

(defun org-execute-get-runnable (file-name)
  "Search FILE-NAME for blocks named with '#+NAME: run-something', capture text between #+NAME and #+begin_src (case-insensitive), and return a list of tuples (name . description)."
  (with-temp-buffer
    (insert-file-contents file-name)
    (goto-char (point-min))
    (let (runnable-blocks)
      ;; Loop through the document looking for #+NAME: run-...
      (while (re-search-forward "#\\+NAME: \\(run-.+\\)$" nil t)
        (let* ((block-name (match-string 1))
               start-pos end-pos description)
          (save-excursion (let ((case-fold-search t))
            (forward-line -1)
            (if (looking-at ".*END_COMMENT.*")
                (progn
                  (setq end-pos (point))
                  (re-search-backward "#\\+BEGIN_COMMENT" nil t)
                  (forward-line 1)
                  (setq start-pos (point))
                  (setq description (string-trim (buffer-substring-no-properties start-pos end-pos))))
                (progn
                 (setq description ""))
            )
            (forward-line 2)
            (push (cons block-name description) runnable-blocks)
          ))))
      (nreverse runnable-blocks))))

(org-execute--projectile-wrapper org-execute-get-runnable-projectile
                                 org-execute-get-runnable
                                 full-path)

(defun eshell/org-execute-block-and-get-results (&rest args)
  "Eshell command wrapper for `org-execute-block-and-get-results`."
  (apply 'org-execute-block-and-get-results-fn args))

(defun eshell/org-execute-block-and-get-results-projectile (&rest args)
  "Eshell command wrapper for `org-execute-block-and-get-results-projectile`."
  (apply 'org-execute-block-and-get-results-projectile-fn args))


(defun eshell/prj-run (command &rest args)
  "Eshell command wrapper for `org-execute-block-and-get-results-projectile`, 
   taking a command and optional args."
  (let ((command-string (if (> (length args) 0)
                            (format "%s(%s)" command
                                    (mapconcat (lambda (arg)
                                                 (if (string-match "=" arg)
                                                     (let ((parts (split-string arg "=")))
                                                       (format "%s=\"%s\"" (car parts) (cadr parts)))
                                                   arg))
                                               args ","))
                          command)))
    (apply 'org-execute-block-and-get-results-projectile-fn (list command-string))))


;; (defun eshell/prj-show ()
;;   "Display runnable projects from `org-execute-get-runnable-projectile` with colors in Eshell."
;;   (let ((projects (org-execute-get-runnable-projectile)))
;;     (if (not projects)
;;         (eshell-print "No runnable projects found.\n")
;;       (with-temp-buffer
;;         ;; Insert the header
;;         (insert "| Project Name | Parameters |\n")
;;         (insert "|---------------+-------------|\n")

;;         ;; Iterate over projects and insert each row into the table
;;         (dolist (project projects)
;;           (let ((project-name (car project))
;;                 (project-params (cdr project)))
;;             (insert "| " (propertize project-name 'face '(:foreground "green"))
;;                     " | " (if (string= project-params "")
;;                               (propertize "No parameters" 'face '(:foreground "light gray"))
;;                             (mapconcat (lambda (line) (string-trim line))
;;                                        (split-string project-params "\n")
;;                                        ", "))
;;                     " |\n")))

;;         ;; Convert the inserted text into an org table
;;         (org-mode)
;;         (org-table-align)

;;         ;; Print the entire buffer content to Eshell
;;         (eshell-print (buffer-string)))
;;       ;; Ensure the output is displayed immediately
;;       (eshell-flush))))

(defun eshell/prj-show ()
  "Display runnable projects from `org-execute-get-runnable-projectile` with colors in Eshell."
  (let ((projects (org-execute-get-runnable-projectile))) ; Ensure this function exists and returns the expected list
    (if (not projects)
        (eshell-print "No runnable projects found.\n")
      (dolist (project projects)
        (let ((project-name (car project))
              (project-params (cdr project)))
          ;; Check if there are parameters, and format them nicely
          (let ((output (concat
                         (propertize project-name 'face '(:foreground "green"))
                         ": "
                         (if (string= project-params "")
                             (propertize "No parameters" 'face '(:foreground "light gray"))
                           (let ((params-lines (split-string project-params "\n")))
                             (concat " " ;; (propertize "Description:" 'face '(:foreground "orange"))
                                     (string-join (mapcar (lambda (line) (concat "\n" (string-trim line))) params-lines) "")))
                           ))))
            ;; Print the output string to Eshell
            (eshell-print (concat output "\n")))))))
    ;; Ensure the output is displayed immediately
    (eshell-flush))

(defun pcomplete/org-execute-block-and-get-results ()
  "Custom pcomplete completion for `org-execute-block-and-get-results` command."
  ;; Assuming `org-execute-block-and-get-results` takes an Org file and a block name as arguments.
  (pcomplete-here (pcomplete-entries "\\.org\\'")) ;; Complete Org files for the first argument.
  (let ((org-file (pcomplete-arg 1))) ;; Assume the Org file is the first argument.
    ;; For the second argument, complete block names within the Org file.
    (when (and org-file (file-exists-p org-file))
      (let* ((blocks (with-temp-buffer
                       (insert-file-contents org-file)
                       ;; Extract block names from the Org file.
                       ;; This is a placeholder; you'll need a function to parse the Org file and extract block names.
                       (org-execute-get-runnable (expand-file-name org-file)))))
        (pcomplete-here blocks)))))

(defun pcomplete/org-execute-block-and-get-results-projectile ()
  "Completion rule for `org-execute-block-and-get-results-projectile`."
  ;; This version does not require the org file name because it's determined by the projectile context.
  (pcomplete-here (mapcar #'car (org-execute-get-runnable-projectile))))

(defun pcomplete/prj-run ()
  "Completion rule for `org-execute-block-and-get-results-projectile`."
  ;; This version does not require the org file name because it's determined by the projectile context.
  (pcomplete-here (mapcar #'car (org-execute-get-runnable-projectile))))

(provide 'org-execute)
