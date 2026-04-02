;;; inspector.el --- Flutter Widget Inspector integration for my-flutter layer  -*- lexical-binding: t -*-
;;
;; Tap a widget on the running device → Emacs jumps to its source code.
;;
;; Approach:
;;   1. Toggle inspector overlay via 'i' sent to the flutter process stdin
;;   2. Poll ext.flutter.inspector.getSelectedSummaryWidget every 0.5s
;;   3. When the creationLocation changes, jump to that file/line/column
;;
;; No Dart-side changes required. Needs only the VM service WebSocket.

(require 'cl-lib)
(require 'json)
;; websocket is required lazily inside my/flutter-inspector--connect

;;; ── State variables ──────────────────────────────────────────────────────────

(defvar my/flutter-inspector--ws nil
  "Active WebSocket connection to the Flutter VM service, or nil.")

(defvar my/flutter-inspector--rpc-id 0
  "Monotonically increasing JSON-RPC request counter.")

(defvar my/flutter-inspector--isolate-id nil
  "Dart isolate ID that has the Flutter inspector extensions.")

(defvar my/flutter-inspector--active nil
  "Non-nil when widget select mode is currently active on the device.")

(defvar my/flutter-inspector--poll-timer nil
  "Repeating timer that polls getSelectedSummaryWidget when active.")

(defvar my/flutter-inspector--last-location nil
  "Last seen creationLocation as \"file:line:col\" string, to detect changes.")

(defvar my/flutter-inspector--callbacks (make-hash-table :test 'eql)
  "Maps integer RPC id to a one-arg callback function for pending requests.")

;;; ── Low-level JSON-RPC helpers ───────────────────────────────────────────────

(defun my/flutter-inspector--next-id ()
  "Return the next JSON-RPC request ID."
  (cl-incf my/flutter-inspector--rpc-id))

(defun my/flutter-inspector--send (method params &optional callback)
  "Send a JSON-RPC 2.0 request with METHOD and PARAMS over the WebSocket.
CALLBACK, if given, is called with the parsed result on success."
  (unless (and my/flutter-inspector--ws
               (websocket-openp my/flutter-inspector--ws))
    (error "Flutter inspector: not connected"))
  (let* ((id   (my/flutter-inspector--next-id))
         (base `((jsonrpc . "2.0") (id . ,id) (method . ,method)))
         (msg  (let ((json-encoding-pretty-print nil))
                 (json-encode (if params
                                  (append base `((params . ,params)))
                                base)))))
    (when callback
      (puthash id callback my/flutter-inspector--callbacks))
    (websocket-send-text my/flutter-inspector--ws msg)))

;;; ── WebSocket event handlers ─────────────────────────────────────────────────

(defun my/flutter-inspector--on-message (_ws frame)
  "Dispatch incoming WebSocket FRAME: route RPC responses to callbacks."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type  'vector)
             (json-key-type    'symbol)
             (json (json-read-from-string (websocket-frame-text frame))))
        (cond
         ;; RPC error response
         ((alist-get 'error json)
          (let ((id (alist-get 'id json)))
            ;; Silently discard errors for poll calls (id >= 100) to avoid spam
            (unless (and id (> id 99))
              (message "Flutter inspector RPC error: %s"
                       (alist-get 'message (alist-get 'error json) "unknown")))
            (when id (remhash id my/flutter-inspector--callbacks))))
         ;; RPC success response — dispatch to callback
         ((alist-get 'id json)
          (let ((id (alist-get 'id json)))
            (when-let ((cb (gethash id my/flutter-inspector--callbacks)))
              (remhash id my/flutter-inspector--callbacks)
              (funcall cb (alist-get 'result json)))))))
    (error (message "Flutter inspector message error: %s" err))))

(defun my/flutter-inspector--on-close (_ws)
  "Handle WebSocket close."
  (my/flutter-inspector--stop-poll)
  (setq my/flutter-inspector--ws         nil
        my/flutter-inspector--isolate-id nil
        my/flutter-inspector--active     nil
        my/flutter-inspector--last-location nil
        my/flutter-inspector--rpc-id     0)
  (clrhash my/flutter-inspector--callbacks)
  (message "Flutter inspector: disconnected"))

(defun my/flutter-inspector--on-error (_ws _type err)
  "Handle WebSocket error."
  (message "Flutter inspector WebSocket error: %s" err)
  (my/flutter-inspector--disconnect))

;;; ── Navigate handler ─────────────────────────────────────────────────────────

(defun my/flutter-inspector--handle-navigate (file-uri line column)
  "Open FILE-URI at LINE and COLUMN in Emacs."
  (let* ((path  (cond ((string-prefix-p "file:///" file-uri) (substring file-uri 7))
                      ((string-prefix-p "file://"  file-uri) (substring file-uri 6))
                      (t file-uri)))
         (line-n (if (numberp line)   line   (string-to-number (format "%s" line))))
         (col-n  (if (numberp column) column (string-to-number (format "%s" column)))))
    (if (not (file-exists-p path))
        (message "Flutter inspector: file not found: %s" path)
      (find-file path)
      (goto-char (point-min))
      (forward-line (1- line-n))
      (forward-char col-n)
      (recenter)
      (pulse-momentary-highlight-one-line (point))
      (message "Flutter inspector → %s:%d:%d"
               (file-name-nondirectory path) line-n col-n))))

;;; ── Polling ──────────────────────────────────────────────────────────────────

(defun my/flutter-inspector--poll ()
  "Poll getSelectedSummaryWidget; navigate when the creationLocation changes."
  (when (and my/flutter-inspector--active
             my/flutter-inspector--isolate-id
             my/flutter-inspector--ws
             (websocket-openp my/flutter-inspector--ws))
    (my/flutter-inspector--send
     "ext.flutter.inspector.getSelectedSummaryWidget"
     `((isolateId   . ,my/flutter-inspector--isolate-id)
       (objectGroup . "emacs-inspector"))
     (lambda (result)
       ;; Flutter service extensions wrap the actual payload one level deeper:
       ;; JSON-RPC result → { type, method, result: <DiagnosticsNode> }
       (let* ((node   (alist-get 'result result))
              (loc    (alist-get 'creationLocation node))
              (uri    (when loc (alist-get 'file loc)))
              (line   (when loc (alist-get 'line loc)))
              (column (when loc (alist-get 'column loc))))
         (when (and uri line)
           (let ((key (format "%s:%s:%s" uri line column)))
             (unless (equal key my/flutter-inspector--last-location)
               (setq my/flutter-inspector--last-location key)
               (my/flutter-inspector--handle-navigate uri line column)))))))))

(defun my/flutter-inspector--start-poll ()
  "Start the 0.5-second polling timer."
  (my/flutter-inspector--stop-poll)
  (setq my/flutter-inspector--poll-timer
        (run-with-timer 0.5 0.5 #'my/flutter-inspector--poll)))

(defun my/flutter-inspector--stop-poll ()
  "Cancel the polling timer if running."
  (when my/flutter-inspector--poll-timer
    (cancel-timer my/flutter-inspector--poll-timer)
    (setq my/flutter-inspector--poll-timer nil)))

;;; ── Setup chain ──────────────────────────────────────────────────────────────

(defun my/flutter-inspector--find-isolate (isolates idx)
  "Try each isolate in ISOLATES starting at IDX for the Flutter inspector."
  (if (>= idx (length isolates))
      (message "Flutter inspector: no Flutter isolate found — is the app fully started?")
    (let ((isolate-id (alist-get 'id (aref isolates idx))))
      (my/flutter-inspector--send
       "getIsolate"
       `((isolateId . ,isolate-id))
       (lambda (result)
         (let* ((exts     (alist-get 'extensionRPCs result))
                (ext-list (when exts (append exts nil))))
           (if (member "ext.flutter.inspector.getSelectedSummaryWidget" ext-list)
               (progn
                 (setq my/flutter-inspector--isolate-id isolate-id)
                 ;; Tell Flutter which directories are "your code" so the summary
                 ;; widget tree is not empty. Without this, getSelectedSummaryWidget
                 ;; returns nil because nothing passes the pub-root filter.
                 (my/flutter-inspector--send
                  "ext.flutter.inspector.setPubRootDirectories"
                  `((isolateId . ,isolate-id)
                    (arg0 . ,(expand-file-name "lib" (projectile-project-root))))
                  (lambda (_)
                    (message "Flutter inspector: ready — SPC o f i to toggle select mode"))))
             (my/flutter-inspector--find-isolate isolates (1+ idx)))))))))

(defun my/flutter-inspector--setup ()
  "After connecting: discover the Flutter isolate."
  (my/flutter-inspector--send
   "getVM" nil
   (lambda (result)
     (let ((isolates (alist-get 'isolates result)))
       (if (or (not isolates) (zerop (length isolates)))
           (message "Flutter inspector: no isolates — is the app fully started?")
         (my/flutter-inspector--find-isolate isolates 0))))))

;;; ── Process stdin helpers ────────────────────────────────────────────────────

(defun my/flutter-inspector--flutter-process ()
  "Return the flutter run process, or nil."
  (let ((buf (cl-find-if (lambda (b) (string-prefix-p "*flutter[" (buffer-name b)))
                         (buffer-list))))
    (when buf (get-buffer-process buf))))

(defun my/flutter-inspector--toggle-overlay ()
  "Send 'i' to the flutter run process to toggle the widget inspector overlay."
  (if-let ((proc (my/flutter-inspector--flutter-process)))
      (process-send-string proc "i")
    (message "Flutter inspector: no flutter process — start with SPC o f r")))

;;; ── Connection management ────────────────────────────────────────────────────

(defun my/flutter-inspector--ws-url ()
  "Convert the HTTP VM service URI to a WebSocket URL, or return nil."
  (when-let ((http-uri (my/flutter--find-vm-uri)))
    (concat (replace-regexp-in-string "^http://" "ws://" http-uri) "ws")))

(defun my/flutter-inspector--attach-sentinel ()
  "Disconnect inspector when the flutter process exits."
  (dolist (buf (buffer-list))
    (when (string-prefix-p "*flutter[" (buffer-name buf))
      (when-let ((proc (get-buffer-process buf)))
        (let ((existing (process-sentinel proc)))
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string-match-p "\\(finished\\|exited\\|killed\\|failed\\)" event)
               (my/flutter-inspector--disconnect))
             (when (functionp existing)
               (funcall existing process event)))))))))

(defun my/flutter-inspector--connect ()
  "Open a WebSocket to the Flutter VM service and discover the Flutter isolate."
  (require 'websocket)
  (let ((ws-url (my/flutter-inspector--ws-url)))
    (unless ws-url
      (user-error "No Flutter app running — start one with SPC o f r"))
    (message "Flutter inspector: connecting…")
    (condition-case err
        (setq my/flutter-inspector--ws
              (websocket-open ws-url
                :on-open    (lambda (_ws) (my/flutter-inspector--setup))
                :on-message #'my/flutter-inspector--on-message
                :on-close   #'my/flutter-inspector--on-close
                :on-error   #'my/flutter-inspector--on-error))
      (error (message "Flutter inspector: connection failed — %s"
                      (error-message-string err))))
    (my/flutter-inspector--attach-sentinel)))

(defun my/flutter-inspector--disconnect ()
  "Close the WebSocket and reset all state."
  (my/flutter-inspector--stop-poll)
  (when (and my/flutter-inspector--ws
             (websocket-openp my/flutter-inspector--ws))
    (websocket-close my/flutter-inspector--ws))
  (setq my/flutter-inspector--ws         nil
        my/flutter-inspector--isolate-id nil
        my/flutter-inspector--active     nil
        my/flutter-inspector--last-location nil))

;;; ── Activate / deactivate ────────────────────────────────────────────────────

(defun my/flutter-inspector--activate ()
  "Enable widget select mode and start polling."
  (setq my/flutter-inspector--last-location nil)
  (my/flutter-inspector--toggle-overlay)
  (setq my/flutter-inspector--active t)
  (my/flutter-inspector--start-poll)
  (message "Flutter inspector: SELECT MODE active — tap a widget"))

(defun my/flutter-inspector--deactivate ()
  "Disable widget select mode and stop polling."
  (my/flutter-inspector--stop-poll)
  (my/flutter-inspector--toggle-overlay)
  (setq my/flutter-inspector--active nil)
  (message "Flutter inspector: select mode OFF"))

;;; ── Public command ───────────────────────────────────────────────────────────

(defun my/flutter-inspector-toggle ()
  "Toggle Flutter widget inspect mode.
When active, tapping a widget on the device jumps to its source in Emacs."
  (interactive)
  (cond
   ;; Active → deactivate
   (my/flutter-inspector--active
    (my/flutter-inspector--deactivate))
   ;; Connected and isolate known → activate immediately
   ((and my/flutter-inspector--ws
         (websocket-openp my/flutter-inspector--ws)
         my/flutter-inspector--isolate-id)
    (my/flutter-inspector--activate))
   ;; Connected but isolate not yet found → wait briefly then activate
   ((and my/flutter-inspector--ws
         (websocket-openp my/flutter-inspector--ws))
    (run-with-timer 1.5 nil
                    (lambda ()
                      (if my/flutter-inspector--isolate-id
                          (my/flutter-inspector--activate)
                        (message "Flutter inspector: isolate not found — try again")))))
   ;; Not connected → connect, then activate after setup completes
   (t
    (my/flutter-inspector--connect)
    (run-with-timer 2.0 nil
                    (lambda ()
                      (if my/flutter-inspector--isolate-id
                          (my/flutter-inspector--activate)
                        (message "Flutter inspector: setup incomplete — try SPC o f i again")))))))
