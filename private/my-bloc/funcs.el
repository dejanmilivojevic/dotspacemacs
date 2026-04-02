;;; funcs.el --- my-bloc layer functions

(require 'cl-lib)

(defconst my/bloc-buffer-name "*bloc-monitor*")
(defvar my/bloc-refresh-timer nil)
(defvar my/bloc-newest-first t
  "When non-nil, newest events appear at the top of the monitor buffer.")
(defvar my/bloc-filter nil
  "When non-nil, only entries whose bloc name matches this regexp are shown.")
(defvar my/bloc-show-states nil
  "When non-nil, show the current state snapshot section.")

;;; ── Current state snapshot ───────────────────────────────────────────────────

(defun my/bloc--current-states (entries)
  "Derive the current state of each live bloc from INIT/CHANGE/CLOSE entries.
Returns an alist of (bloc-name . current-state-string), sorted by bloc name."
  (let ((states (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (cl-destructuring-bind (type _time bloc detail) entry
        (cond
         ((string= type "INIT")
          (puthash bloc detail states))
         ((string= type "CHANGE")
          (let* ((arrow (string-match " → " detail))
                 (new   (if arrow (substring detail (match-end 0)) detail)))
            (puthash bloc new states)))
         ((string= type "CLOSE")
          (remhash bloc states)))))
    (sort (let (result)
            (maphash (lambda (k v) (push (cons k v) result)) states)
            result)
          (lambda (a b) (string< (car a) (car b))))))

;;; ── Flutter buffer lookup ────────────────────────────────────────────────────

(defun my/bloc--flutter-buffer ()
  "Return the first active flutter run buffer, or nil."
  (cl-find-if (lambda (b) (string-prefix-p "*flutter[" (buffer-name b)))
              (buffer-list)))

;;; ── Log line parsing ─────────────────────────────────────────────────────────
;;
;; Expected format from DraegerBlocObserver:
;;   [BLOC:TYPE] HH:MM:SS | BlocName | detail

(defun my/bloc--parse-line (line)
  "Parse a BLOC log line. Returns (type time bloc detail) or nil."
  (when (string-match
         "\\[BLOC:\\([A-Z]+\\)\\] \\([0-9:]+\\) | \\([^|]+\\)| ?\\(.*\\)"
         line)
    (list (match-string 1 line)
          (match-string 2 line)
          (string-trim (match-string 3 line))
          (string-trim (match-string 4 line)))))

(defun my/bloc--scan-flutter-buffer ()
  "Collect all BLOC log entries from the active flutter run buffer."
  (when-let ((buf (my/bloc--flutter-buffer)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let (entries)
          (while (re-search-forward "\\[BLOC:[A-Z]+\\]" nil t)
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))
                   (entry (my/bloc--parse-line line)))
              (when entry (push entry entries))))
          (nreverse entries))))))

;;; ── Display ──────────────────────────────────────────────────────────────────

(defun my/bloc--type-face (type)
  (pcase type
    ("EVENT"  '(:foreground "cyan"          :weight bold))
    ("CHANGE" '(:foreground "green"         :weight bold))
    ("ERROR"  '(:foreground "red"           :weight bold))
    ("INIT"   '(:foreground "gray"          :weight normal))
    ("CLOSE"  '(:foreground "gray"          :weight normal))
    (_        '(:foreground "white"))))

;;; ── State diff rendering ─────────────────────────────────────────────────────

(defun my/bloc--split-fields (s)
  "Split field string S on ', ' respecting nested parens/brackets."
  (let ((depth 0) (start 0) (result '()) (i 0) (len (length s)))
    (while (< i len)
      (let ((ch (aref s i)))
        (cond ((memq ch '(?\( ?\[ ?\{)) (cl-incf depth))
              ((memq ch '(?\) ?\] ?\})) (cl-decf depth))
              ((and (= depth 0) (= ch ?,)
                    (< (1+ i) len) (= (aref s (1+ i)) ?\s))
               (push (string-trim (substring s start i)) result)
               (setq start (+ i 2) i (1+ i)))))
      (cl-incf i))
    (when (< start len)
      (push (string-trim (substring s start)) result))
    (nreverse result)))

(defun my/bloc--parse-state (s)
  "Parse 'TypeName(f: v, ...)' into (type-name . ((f . v) ...)), or nil."
  (let ((trimmed (string-trim s)))
    (if (string-match "^\\([A-Za-z][A-Za-z0-9_.]*\\)(\\(.*\\))$" trimmed)
        (let* ((type  (match-string 1 trimmed))
               (inner (match-string 2 trimmed))
               (pairs (mapcar (lambda (pair)
                                (when (string-match "^\\([^:]+\\):\\s-*\\(.*\\)$" pair)
                                  (cons (string-trim (match-string 1 pair))
                                        (string-trim (match-string 2 pair)))))
                              (my/bloc--split-fields inner))))
          (cons type (delq nil pairs)))
      ;; No parens — treat the whole string as just a type name
      (when (string-match "^[A-Za-z][A-Za-z0-9_.]*$" trimmed)
        (cons trimmed nil)))))

(defun my/bloc--render-state-diff (old-str new-str)
  "Return a propertized diff string comparing two freezed state strings."
  (let ((old (my/bloc--parse-state old-str))
        (new (my/bloc--parse-state new-str)))
    (cond
     ;; Same type — field-level diff
     ((and old new (string= (car old) (car new)))
      (let* ((old-fields (cdr old))
             (new-fields (cdr new))
             (all-keys   (delete-dups
                          (append (mapcar #'car old-fields)
                                  (mapcar #'car new-fields)))))
        (concat
         (propertize (car old) 'face '(:foreground "#abb2bf"))
         "("
         (mapconcat
          (lambda (k)
            (let ((ov (cdr (assoc k old-fields)))
                  (nv (cdr (assoc k new-fields))))
              (if (equal ov nv)
                  (propertize (concat k ": " (or ov "")) 'face '(:foreground "#4b5263"))
                (concat
                 (propertize (concat k ": ") 'face '(:foreground "#e5c07b"))
                 (propertize (or ov "nil")   'face '(:foreground "#e06c75"))
                 (propertize "→"             'face '(:foreground "#888888"))
                 (propertize (or nv "nil")   'face '(:foreground "#98c379" :weight bold))))))
          all-keys ", ")
         ")")))
     ;; Different types — show old name and full new state with its fields
     ((and old new)
      (concat
       (propertize (car old) 'face '(:foreground "#e06c75"))
       (propertize " → "    'face '(:foreground "#888888"))
       (propertize (car new) 'face '(:foreground "#98c379" :weight bold))
       (when (cdr new)
         (concat
          "("
          (mapconcat
           (lambda (pair)
             (concat (propertize (car pair) 'face '(:foreground "#e5c07b"))
                     ": "
                     (propertize (cdr pair) 'face '(:foreground "#98c379"))))
           (cdr new) ", ")
          ")"))))
     ;; Unparseable — fall back to plain string
     (t
      (concat (propertize old-str 'face '(:foreground "#e06c75"))
              (propertize " → "   'face '(:foreground "#888888"))
              (propertize new-str 'face '(:foreground "#98c379")))))))

(defun my/bloc--render-detail (type detail)
  "Render the detail column, applying diff rendering for CHANGE entries."
  (if (not (string= type "CHANGE"))
      (propertize detail 'face '(:foreground "#abb2bf"))
    (let* ((arrow (string-match " → " detail))
           (old-str (if arrow (substring detail 0 arrow) detail))
           (new-str (if arrow (substring detail (match-end 0)) "")))
      (my/bloc--render-state-diff old-str new-str))))

;;; ─────────────────────────────────────────────────────────────────────────────

(defun my/bloc--format-entry (entry)
  (cl-destructuring-bind (type time bloc detail) entry
    (concat
     "  "
     (propertize time  'face '(:foreground "#888888"))
     "   "
     (propertize (format "%-8s" type)  'face (my/bloc--type-face type))
     "   "
     (propertize (format "%-38s" bloc) 'face '(:foreground "#e5c07b" :weight bold))
     (my/bloc--render-detail type detail)
     "\n")))

(defun my/bloc-refresh-monitor ()
  "Refresh the BLoC monitor buffer."
  (interactive)
  (let* ((entries (my/bloc--scan-flutter-buffer))
         (flutter-buf (my/bloc--flutter-buffer))
         (buf (get-buffer-create my/bloc-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'my/bloc-monitor-mode)
        (my/bloc-monitor-mode))
      (let* ((inhibit-read-only t)
             (win (get-buffer-window buf))
             (saved-start (when win (window-start win))))
        (erase-buffer)
        ;; Header
        (insert (propertize
                 (format "  BLoC Monitor %s\n"
                         (format-time-string
                          "── %H:%M:%S ──────────────────────────────────────────────"))
                 'face '(:foreground "#5c6370" :weight bold)))
        (insert (propertize
                 (format "  Source: %s\n"
                         (if flutter-buf (buffer-name flutter-buf)
                           "no flutter buffer running"))
                 'face '(:foreground "#5c6370")))
        (insert (propertize
                 (concat "  " (make-string 74 ?─) "\n")
                 'face '(:foreground "#3e4451")))
        ;; Current state snapshot
        (when my/bloc-show-states
          (let ((snapshot (my/bloc--current-states entries)))
            (insert (propertize "  Current States\n"
                                'face '(:foreground "#5c6370" :weight bold)))
            (if snapshot
                (dolist (pair snapshot)
                  (insert (propertize (format "  %-38s" (car pair))
                                      'face '(:foreground "#e5c07b" :weight bold)))
                  (insert (my/bloc--render-detail "CHANGE"
                                                  (concat "_ → " (cdr pair))))
                  (insert "\n"))
              (insert (propertize "  (no active blocs)\n"
                                  'face '(:foreground "#5c6370"))))
            (insert (propertize (concat "  " (make-string 74 ?─) "\n")
                                'face '(:foreground "#3e4451")))))
        (insert (propertize
                 "  Time       Type       BLoC                                       Detail\n"
                 'face '(:foreground "#5c6370")))
        (insert (propertize
                 (concat "  " (make-string 74 ?─) "\n")
                 'face '(:foreground "#3e4451")))
        ;; Entries
        (let* ((ordered (if my/bloc-newest-first (reverse entries) entries))
               (visible (if my/bloc-filter
                            (cl-remove-if-not
                             (lambda (e) (string-match-p my/bloc-filter (cl-third e)))
                             ordered)
                          ordered)))
          (if visible
              (dolist (entry visible)
                (insert (my/bloc--format-entry entry)))
            (insert (propertize
                     (if entries
                         (format "\n  No entries match filter: %s\n" my/bloc-filter)
                       (concat "\n  No BLoC activity yet.\n"
                               "  Start the app with SPC o f r and trigger some events.\n"
                               "  Make sure DraegerBlocObserver is registered in main.dart.\n"))
                     'face '(:foreground "#5c6370"))))
          ;; Footer
          (insert (propertize
                   (concat "\n  " (make-string 74 ?─) "\n")
                   'face '(:foreground "#3e4451")))
          (insert (propertize
                   (format "  %d/%d  |  f = filter [%s]  F = reset  g = refresh  c = clear  o = %s  s = states [%s]  a = auto  q = close\n"
                           (length visible) (length entries)
                           (or my/bloc-filter "all")
                           (if my/bloc-newest-first "newest↑" "oldest↑")
                           (if my/bloc-show-states "on" "off"))
                   'face '(:foreground "#5c6370"))))
        (if (and win saved-start)
            (let ((pos (min saved-start (point-max))))
              (goto-char pos)
              (set-window-start win pos t))
          (goto-char (point-min)))))))

;;; ── Show / clear / auto-refresh ──────────────────────────────────────────────

(defun my/bloc-show-monitor ()
  "Show the BLoC monitor at the bottom and start auto-refresh."
  (interactive)
  (my/bloc-refresh-monitor)
  (display-buffer (get-buffer-create my/bloc-buffer-name)
                  '((display-buffer-reuse-window display-buffer-at-bottom)
                    (window-height . 0.35)))
  (my/bloc-start-auto-refresh))

(defun my/bloc-clear-monitor ()
  "Clear all entries in the BLoC monitor buffer."
  (interactive)
  (when-let ((buf (get-buffer my/bloc-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "  BLoC Monitor ── cleared ──\n"
                            'face '(:foreground "#5c6370")))))))

(defun my/bloc-start-auto-refresh ()
  "Auto-refresh the monitor every second while it is visible."
  (unless my/bloc-refresh-timer
    (setq my/bloc-refresh-timer
          (run-with-timer 1 1
                          (lambda ()
                            (when (get-buffer-window my/bloc-buffer-name)
                              (my/bloc-refresh-monitor)))))
    (message "BLoC monitor: auto-refresh ON")))

(defun my/bloc-stop-auto-refresh ()
  "Stop auto-refreshing the BLoC monitor."
  (when my/bloc-refresh-timer
    (cancel-timer my/bloc-refresh-timer)
    (setq my/bloc-refresh-timer nil)
    (message "BLoC monitor: auto-refresh OFF")))

(defun my/bloc-toggle-states ()
  "Toggle the current state snapshot section in the BLoC monitor."
  (interactive)
  (setq my/bloc-show-states (not my/bloc-show-states))
  (my/bloc-refresh-monitor)
  (message "BLoC state snapshot: %s" (if my/bloc-show-states "ON" "OFF")))

(defun my/bloc-set-filter ()
  "Filter the BLoC monitor to a specific bloc name (regexp). Use completing-read."
  (interactive)
  (let* ((entries (my/bloc--scan-flutter-buffer))
         (blocs   (delete-dups (mapcar #'cl-third entries)))
         (input   (completing-read "Filter by bloc (regexp): " blocs nil nil my/bloc-filter)))
    (setq my/bloc-filter (if (string-empty-p input) nil input))
    (my/bloc-refresh-monitor)
    (message "BLoC filter: %s" (or my/bloc-filter "none"))))

(defun my/bloc-reset-filter ()
  "Remove the bloc filter and show all entries."
  (interactive)
  (setq my/bloc-filter nil)
  (my/bloc-refresh-monitor)
  (message "BLoC filter cleared"))

(defun my/bloc-toggle-order ()
  "Toggle between newest-first and oldest-first display order."
  (interactive)
  (setq my/bloc-newest-first (not my/bloc-newest-first))
  (my/bloc-refresh-monitor)
  (message "BLoC monitor: %s" (if my/bloc-newest-first "newest first" "oldest first")))

(defun my/bloc-toggle-auto-refresh ()
  "Toggle auto-refresh of the BLoC monitor."
  (interactive)
  (if my/bloc-refresh-timer
      (my/bloc-stop-auto-refresh)
    (my/bloc-start-auto-refresh)))
