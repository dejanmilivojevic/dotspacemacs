;;; packages.el --- my-eca layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; ECA (Editor Code Assistant) in Emacs, driving Claude.
;;
;; ECA has no dedicated "claude-code" provider: Claude runs through ECA's
;; built-in `anthropic' provider.  To get the Claude Code-style subscription
;; experience (reusing your Claude Pro/Max login instead of an API key), start
;; a chat and run the `/login' slash command, then type `anthropic' and follow
;; the browser OAuth flow.  Alternatively, export ANTHROPIC_API_KEY, which
;; takes precedence over the login.
;;
;; The `eca' server binary is provisioned automatically on first use: ECA looks
;; at `eca-custom-command', then `$PATH', and finally auto-downloads and caches
;; the server.  So no manual server install is required.
;;
;; Sibling to the `my-agent-shell' layer (ACP + claude-agent-acp); this is the
;; ECA-based alternative.

(defconst my-eca-packages
  '((eca :location (recipe :fetcher github
                           :repo "editor-code-assistant/eca-emacs"
                           :files ("*.el")))))

(defun my-eca/init-eca ()
  (use-package eca
    :defer t
    :init
    ;; Leader-key bindings under the reserved user prefix `SPC o'.
    (spacemacs/declare-prefix "oe" "eca")
    (spacemacs/set-leader-keys
      "oee" 'eca                      ; start ECA / open chat in this workspace
      "oen" 'eca-chat-new             ; new chat
      "oet" 'eca-chat-toggle-window   ; show/hide the chat window
      "oei" 'eca-chat-inline-prompt   ; ask without leaving the current buffer
      "oem" 'eca-chat-select-model    ; switch model
      "oeb" 'eca-chat-select-agent    ; switch agent/behavior
      "oeS" 'eca-settings             ; settings panel (providers, models, login)
      "oeq" 'eca-stop
      "oeR" 'eca-restart)
    :config
    ;; Dock the chat on the right, like the agent-shell setup.
    (setq eca-chat-window-side 'right)
    ;; Include a repo map as context automatically when a chat starts.
    (setq eca-chat-auto-add-repomap t)
    ;; Leave the model unset so ECA uses its default Claude model
    ;; (currently anthropic/claude-sonnet-4.5); pick another any time with
    ;; `SPC o e m'.  To pin one, e.g.:
    ;;   (setq eca-chat-custom-model "anthropic/claude-opus-4-6")
    ))

;;; packages.el ends here
