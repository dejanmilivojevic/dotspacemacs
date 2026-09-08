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
    agent-shell
    ;; Org-mode transcripts (markdown->org on the fly, org-id + #+FILETAGS).
    ;; Not on MELPA yet, so pull it straight from GitHub.
    (agent-shell-org-transcript
     :location (recipe :fetcher github
                       :repo "lllShamanlll/agent-shell-org-transcript"))))

(defun my-agent-shell/init-shell-maker ()
  (use-package shell-maker :defer t))

(defun my-agent-shell/init-acp ()
  (use-package acp :defer t))

(defun my-agent-shell/org-transcript-file-path ()
  "Return an agent-shell transcript path in the repo, with a .org extension.

Mirrors `agent-shell--default-transcript-file-path' (transcripts live in
the project's .agent-shell/transcripts/ directory) but uses a .org
suffix.  The .org extension is what activates
`agent-shell-org-transcript's markdown->org conversion advice, so the
streamed transcript is written directly as Org."
  (let ((dir (agent-shell--dot-subdir "transcripts")))
    (expand-file-name (format-time-string "%F-%H-%M-%S.org") dir)))

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
      "oac" 'agent-shell-anthropic-start-claude-code
      "oam" 'agent-shell-org-transcript-migrate)
    :config
    ;; Reuse the Claude Code subscription login (from `claude' CLI).
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication :login t))
    ;; Restore sessions via `session/load' rather than the default `minimal'
    ;; (`session/resume').  The hermes ACP server *always* replays the full
    ;; transcript on resume, but agent-shell's `minimal' strategy assumes
    ;; resume yields no replay, so it never buffers those notifications --
    ;; the history renders but the shell stays stuck "mid-turn" and the
    ;; buffer is left read-only.  `full' forces `session/load', which buffers
    ;; the replay (`pending-restore') and finalizes into a live, editable
    ;; prompt.  Use `last' or `first-last' instead if replaying the entire
    ;; conversation is too slow for long transcripts.
    (setq agent-shell-session-restore-verbosity 'full)
    ;; Let the spawned adapter inherit PATH/HOME/etc. so it can locate `node',
    ;; `claude-agent-acp', and your Claude login credentials.
    (setq agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables :inherit-env t))
    ;; Save transcripts as Org.  Requiring `agent-shell-org-transcript'
    ;; installs advice on agent-shell's transcript writer that converts
    ;; markdown to org on the fly -- but only when the transcript path ends
    ;; in .org.  The package's own path function targets `org-roam-directory',
    ;; which we don't rely on; override it to keep transcripts in-repo (as
    ;; .org) so the conversion still kicks in without needing org-roam.
    (require 'agent-shell-org-transcript)
    (setq agent-shell-transcript-file-path-function
          #'my-agent-shell/org-transcript-file-path)))

;; Loaded via `require' from agent-shell's :config above; nothing to do here
;; beyond letting Spacemacs install the package from its GitHub recipe.
(defun my-agent-shell/init-agent-shell-org-transcript ()
  (use-package agent-shell-org-transcript
    :defer t
    :after agent-shell))

;;; packages.el ends here
