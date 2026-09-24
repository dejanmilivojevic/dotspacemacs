;;; funcs.el --- my-pai layer functions -*- lexical-binding: t; -*-
;;
;; Install and update the pai clones asynchronously: git runs as a background
;; process, so Emacs never waits on the network.

(require 'cl-lib)
(require 'subr-x)

(defconst my-pai-layer-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the my-pai layer.")

(defconst my-pai-core-dir (expand-file-name "pai" my-pai-layer-dir)
  "Git clone of the pai core.")

(defconst my-pai-extensions-dir (expand-file-name "extensions" my-pai-core-dir)
  "Git clone of pai-extensions, inside the core clone.
There the core's `make test' and `make compile' pick them up as well.")

(defconst my-pai-core-repo "https://github.com/dejanmilivojevic/pai.git")
(defconst my-pai-extensions-repo "https://github.com/dejanmilivojevic/pai-extensions.git")

(defconst my-pai-home-extensions
  (expand-file-name "extensions" (expand-file-name ".pai" "~"))
  "Where pai loads home-wide extensions from: a symlink to `my-pai-extensions-dir'.")

(defun my-pai//git (args dir done)
  "Run git with ARGS in DIR in the background, then call DONE with success."
  (let ((default-directory (file-name-as-directory dir))
        (buf (get-buffer-create "*my-pai git*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n$ git %s\n" (string-join args " "))))
    (make-process
     :name "my-pai-git" :buffer buf :command (cons "git" args) :noquery t
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (funcall done (zerop (process-exit-status proc))))))))

(defun my-pai//link-extensions ()
  "Point ~/.pai/extensions at `my-pai-extensions-dir'.
Only a missing path or an existing symlink is replaced; a real directory is
left alone (with a warning), so nothing is ever deleted."
  (let ((link my-pai-home-extensions))
    (cond
     ((not (file-directory-p my-pai-extensions-dir)) nil)
     ((equal (file-symlink-p link) my-pai-extensions-dir) t)
     ((and (file-exists-p link) (not (file-symlink-p link)))
      (message "my-pai: %s is a real directory; not replacing it with a link" link)
      nil)
     (t (make-directory (file-name-directory link) t)
        (make-symbolic-link my-pai-extensions-dir link t)
        (message "my-pai: %s -> %s" link my-pai-extensions-dir)
        t))))

(defun my-pai//exclude-extensions ()
  "Keep the extensions clone out of the core clone's `git status'."
  (let ((exclude (expand-file-name ".git/info/exclude" my-pai-core-dir)))
    (when (and (file-exists-p exclude)
               (not (with-temp-buffer
                      (insert-file-contents exclude)
                      (re-search-forward "^/extensions/$" nil t))))
      (with-temp-buffer
        (insert-file-contents exclude)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "/extensions/\n")
        (write-region nil nil exclude nil 'silent)))))

(defun my-pai/install ()
  "Clone the pai core and its extensions into the layer, in the background.
Already present clones are kept.  When done, link ~/.pai/extensions and load pai."
  (interactive)
  (let ((finish (lambda ()
                  (my-pai//exclude-extensions)
                  (my-pai//link-extensions)
                  (my-pai//activate)
                  (message "my-pai: installed; M-x pai to start"))))
    (cl-flet ((extensions ()
                (if (file-directory-p my-pai-extensions-dir)
                    (funcall finish)
                  (my-pai//git (list "clone" "--quiet" my-pai-extensions-repo my-pai-extensions-dir)
                               my-pai-layer-dir
                               (lambda (ok)
                                 (if ok (funcall finish)
                                   (message "my-pai: cloning extensions failed, see *my-pai git*")))))))
      (if (file-directory-p my-pai-core-dir)
          (extensions)
        (message "my-pai: cloning pai...")
        (my-pai//git (list "clone" "--quiet" my-pai-core-repo my-pai-core-dir)
                     my-pai-layer-dir
                     (lambda (ok)
                       (if ok (extensions)
                         (message "my-pai: cloning pai failed, see *my-pai git*"))))))))

(defun my-pai/update ()
  "Pull the pai core and extensions clones in the background (fast-forward only).
Run /reload in open pai buffers afterwards (or restart Emacs for core changes)."
  (interactive)
  (dolist (dir (list my-pai-core-dir my-pai-extensions-dir))
    (when (file-directory-p dir)
      (let ((name (file-name-nondirectory dir)))
        (my-pai//git '("pull" "--ff-only" "--quiet") dir
                     (lambda (ok)
                       (message "my-pai: %s %s" name
                                (if ok "updated" "update failed, see *my-pai git*"))))))))

(defun my-pai//activate ()
  "Put pai on `load-path' and set up its entry points.
pai itself loads lazily, on the first command that needs it."
  (when (file-directory-p my-pai-core-dir)
    (dolist (d '("lisp" "vendor/vui"))
      (add-to-list 'load-path (expand-file-name d my-pai-core-dir)))
    (autoload 'pai "pai" "Open the pai agent chat buffer." t)
    (autoload 'pai-new-session "pai" "Open a fresh pai session buffer." t)
    (autoload 'pai-add-to-prompt "pai" "Add a reference to this buffer to the pai prompt." t)
    (autoload 'pai-settings-ui-open "pai" "Open the pai settings screen." t)
    (autoload 'pai-add-provider "pai" "Add an LLM provider to pai." t)
    (autoload 'pai-settings-restore-backup "pai" "Restore a pai settings backup." t)
    (autoload 'pai-oneshot "pai" "Run a prompt through pai headlessly.")
    t))

;;; funcs.el ends here
