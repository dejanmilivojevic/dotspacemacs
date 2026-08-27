;;; packages.el --- my-agent-shell layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Agentic coding in Emacs via agent-shell (ACP) + Claude Code.
;;
;; External requirement (installed globally via npm):
;;   npm install -g @agentclientprotocol/claude-agent-acp
;; This provides the `claude-agent-acp' binary that agent-shell talks to.
;;
;; Auth: login-based, reusing your existing `claude' CLI subscription login.
;; Run `claude' once in a terminal to log in, then use it from Emacs.

(defconst my-agent-shell-packages
  '(shell-maker
    acp
    agent-shell))

(defun my-agent-shell/init-shell-maker ()
  (use-package shell-maker :defer t))

(defun my-agent-shell/init-acp ()
  (use-package acp :defer t))

(defun my-agent-shell/init-agent-shell ()
  (use-package agent-shell
    :defer t
    :init
    ;; Ensure the node bin holding `claude-agent-acp' (and `node' itself) is
    ;; visible to Emacs' `executable-find' and to the spawned agent process.
    (let ((node-bin "/home/madman/.nvm/versions/node/v18.20.4/bin"))
      (when (file-directory-p node-bin)
        (add-to-list 'exec-path node-bin)
        (setenv "PATH" (concat node-bin path-separator (getenv "PATH")))))
    ;; Leader-key bindings under the reserved user prefix `SPC o'.
    (spacemacs/declare-prefix "oa" "agent-shell")
    (spacemacs/set-leader-keys
      "oaa" 'agent-shell
      "oac" 'agent-shell-anthropic-start-claude-code)
    :config
    ;; Reuse the Claude Code subscription login (from `claude' CLI).
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication :login t))
    ;; Let the spawned adapter inherit PATH/HOME/etc. so it can locate `node',
    ;; `claude-agent-acp', and your Claude login credentials.
    (setq agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables :inherit-env t))))

;;; packages.el ends here
