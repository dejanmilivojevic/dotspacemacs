;;; funcs.el --- my-flutter layer functions

(require 'cl-lib)

;; Load inspector module (no websocket dependency at load time)
(load (expand-file-name "inspector"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; ── Flavor detection ────────────────────────────────────────────────────────

(defun my/flutter-get-flavors ()
  "Return list of flavor names parsed from android/app/build.gradle."
  (let ((gradle (expand-file-name "android/app/build.gradle"
                                  (projectile-project-root))))
    (when (file-exists-p gradle)
      (with-temp-buffer
        (insert-file-contents gradle)
        (goto-char (point-min))
        (let (flavors)
          (when (re-search-forward "^[ \t]*productFlavors[ \t]*{" nil t)
            (let ((depth 1))
              (while (and (> depth 0) (not (eobp)))
                (forward-line 1)
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
                  (when (and (= depth 1)
                             (string-match "^[ \t]+\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t]*{" line))
                    (push (match-string 1 line) flavors))
                  (setq depth (+ depth
                                 (cl-count ?{ (string-to-list line))
                                 (- (cl-count ?} (string-to-list line)))))))))
          (nreverse flavors))))))

(defun my/flutter--select-flavor ()
  "Prompt user to select a Flutter flavor. Returns flavor string."
  (let ((flavors (my/flutter-get-flavors)))
    (unless flavors
      (user-error "No flavors found in android/app/build.gradle"))
    (if (= (length flavors) 1)
        (car flavors)
      (completing-read "Flavor: " flavors nil t))))

;;; ── Local entry point detection ─────────────────────────────────────────────

(defun my/flutter--target-flag (root)
  "Return '--target lib/main_emacs.dart' if that file exists in ROOT, else nil."
  (let ((f (expand-file-name "lib/main_emacs.dart" root)))
    (when (file-exists-p f) (concat " --target " f))))

;;; ── Run (no debugger) ───────────────────────────────────────────────────────

(defun my/flutter-run (flavor)
  "Run Flutter with FLAVOR in a comint buffer at the bottom.
Keys in the buffer: r = hot-reload  R = hot-restart  q = quit."
  (interactive (list (my/flutter--select-flavor)))
  (let* ((root (projectile-project-root))
         (buf-name (format "*flutter[%s]*" flavor))
         (default-directory root)
         (compilation-buffer-name-function (lambda (_) buf-name)))
    (when-let ((old (get-buffer buf-name)))
      (when (get-buffer-process old)
        (interrupt-process (get-buffer-process old))
        (sit-for 0.5))
      (kill-buffer old))
    (save-window-excursion
      (compile (format "flutter run --flavor %s%s"
                       flavor (or (my/flutter--target-flag root) "")) t))
    (display-buffer (get-buffer buf-name)
                    '((display-buffer-reuse-window display-buffer-at-bottom)
                      (window-height . 0.3)))
    (message "flutter run --flavor %s  |  r=reload  R=restart  q=quit" flavor)))

;;; ── Debug (DAP + breakpoints) ───────────────────────────────────────────────

(defun my/flutter-debug (flavor)
  "Launch Flutter with FLAVOR under the DAP debugger.
Set breakpoints first with \\[dap-breakpoint-toggle] (SPC d b b)."
  (interactive (list (my/flutter--select-flavor)))
  (require 'lsp-dart-dap)
  (let* ((root (projectile-project-root))
         (entry  (let ((f (expand-file-name "lib/main_emacs.dart" root)))
                   (if (file-exists-p f) f (expand-file-name "lib/main.dart" root))))
         (config (list :type "dart"
                       :request "launch"
                       :name (format "Flutter debug [%s]" flavor)
                       :program entry
                       :args (list "--flavor" flavor)
                       :flutterMode "debug"
                       :cwd root)))
    (dap-register-debug-template (plist-get config :name) config)
    (dap-debug config)))

;;; ── Attach to running app ───────────────────────────────────────────────────

(defun my/flutter--find-vm-uri ()
  "Scan flutter run buffers for a VM service URI."
  (catch 'found
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*flutter[" (buffer-name buf))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-max))
            (when (re-search-backward
                   "\\(http://127\\.0\\.0\\.1:[0-9]+/[a-zA-Z0-9_=/+-]+/\\)"
                   nil t)
              (throw 'found (match-string-no-properties 1)))))))))

(defun my/flutter-show-window ()
  "Show the flutter run buffer at the bottom. Focuses it if already visible."
  (interactive)
  (let ((buf (cl-find-if (lambda (b) (string-prefix-p "*flutter[" (buffer-name b)))
                         (buffer-list))))
    (if buf
        (display-buffer buf '((display-buffer-reuse-window display-buffer-at-bottom)
                              (window-height . 0.3)))
      (message "No flutter buffer — start one with SPC o f r"))))

(defun my/flutter-attach ()
  "Attach DAP debugger to an already-running Flutter app.
Shows the flutter run buffer first so the VM URI is visible."
  (interactive)
  (my/flutter-show-window)
  (require 'lsp-dart-dap)
  (let* ((detected (my/flutter--find-vm-uri))
         (uri (read-string "VM Service URI: "
                           (or detected "http://127.0.0.1:"))))
    (when (string= uri "http://127.0.0.1:")
      (user-error "VM service URI is required"))
    (dap-debug (list :type "dart"
                     :request "attach"
                     :name "Flutter attach"
                     :vmServiceUri uri
                     :cwd (projectile-project-root)))))
