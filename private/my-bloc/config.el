;;; config.el --- my-bloc layer configuration

;;; ── BLoC monitor major mode ──────────────────────────────────────────────────

(defvar my/bloc-monitor-mode-map (make-sparse-keymap)
  "Keymap for `my/bloc-monitor-mode'.")

;; Define keys unconditionally so reloads always pick up new bindings.
(define-key my/bloc-monitor-mode-map "g" #'my/bloc-refresh-monitor)
(define-key my/bloc-monitor-mode-map "c" #'my/bloc-clear-monitor)
(define-key my/bloc-monitor-mode-map "a" #'my/bloc-toggle-auto-refresh)
(define-key my/bloc-monitor-mode-map "o" #'my/bloc-toggle-order)
(define-key my/bloc-monitor-mode-map "s" #'my/bloc-toggle-states)
(define-key my/bloc-monitor-mode-map "f" #'my/bloc-set-filter)
(define-key my/bloc-monitor-mode-map "F" #'my/bloc-reset-filter)
(define-key my/bloc-monitor-mode-map "q" #'quit-window)

(define-derived-mode my/bloc-monitor-mode special-mode "BLoC"
  "Read-only buffer showing live BLoC events and state transitions."
  (setq truncate-lines t)
  (add-hook 'kill-buffer-hook #'my/bloc-stop-auto-refresh nil t))

;;; ── Bottom popup window ──────────────────────────────────────────────────────

(add-to-list 'display-buffer-alist
             '("\\*bloc-monitor\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.35)
               (dedicated . t)))

;;; ── Keybindings ──────────────────────────────────────────────────────────────

(spacemacs/set-leader-keys "ob" #'my/bloc-show-monitor)
(spacemacs/set-leader-keys "oB" #'my/bloc-clear-monitor)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC o b" "bloc monitor")
  (which-key-add-key-based-replacements "SPC o B" "bloc clear"))
