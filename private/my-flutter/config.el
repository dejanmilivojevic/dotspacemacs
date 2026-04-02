;;; config.el --- my-flutter layer configuration

;;; ── Bottom popup window (full width, like shell-pop) ───────────────────────

(add-to-list 'display-buffer-alist
             '("\\*flutter\\[.*\\]\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)
               (dedicated . t)))

;;; ── Hydra menu ──────────────────────────────────────────────────────────────

(with-eval-after-load 'hydra
  (defhydra my/flutter-hydra (:exit t :hint nil)
    "
  Flutter ──────────────────────────────────────────────────────
   _r_ run            (no debugger, interactive comint)
   _d_ debug          (DAP — set breakpoints first: SPC d b b)
   _a_ attach DAP     (to running app — needs VM service URI)
   _s_ show window    (focus flutter output buffer)
   _i_ inspect        (toggle widget select mode → jump to source)
  ─────────────────────────────────────────────────────────────
   _q_ cancel
"
    ("r" my/flutter-run)
    ("d" my/flutter-debug)
    ("a" my/flutter-attach)
    ("s" my/flutter-show-window)
    ("i" my/flutter-inspector-toggle)
    ("q" nil)))

;;; ── Keybindings ─────────────────────────────────────────────────────────────

(spacemacs/set-leader-keys "of" #'my/flutter-hydra/body)
(spacemacs/set-leader-keys "oF" #'my/flutter-show-window)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC o f" "flutter")
  (which-key-add-key-based-replacements "SPC o F" "flutter window"))
