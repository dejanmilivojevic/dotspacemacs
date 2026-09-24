;;; config.el --- my-pai layer configuration -*- lexical-binding: t; -*-
;;
;; pai -- Pi Agent for Emacs (https://github.com/dejanmilivojevic/pai), with
;; all of its extensions (https://github.com/dejanmilivojevic/pai-extensions).
;;
;; Layout (both are git clones, .gitignored here):
;;   my-pai/pai/              the pai core
;;   my-pai/pai/extensions/   pai-extensions
;; ~/.pai/extensions is a symlink to my-pai/pai/extensions/, so every pai
;; instance loads all extensions automatically.  Which ones are on is kept
;; in ~/.pai/settings.json ("extensions"; /menu -> Extensions).
;;
;; Providers, models and API keys live in ~/.pai (settings.json, auth.json),
;; never in this layer: this config repository is public.
;;
;; First use: M-x my-pai/install (clones in the background).
;; Update:    M-x my-pai/update, then /reload in pai buffers.

(if (not (file-directory-p my-pai-core-dir))
    (spacemacs-buffer/warning
     "my-pai: pai not found at %s.  Run M-x my-pai/install" my-pai-core-dir)
  (my-pai//activate)
  (unless (file-directory-p my-pai-extensions-dir)
    (spacemacs-buffer/warning
     "my-pai: extensions not found at %s.  Run M-x my-pai/install" my-pai-extensions-dir))
  (my-pai//link-extensions))

;; `C-c i' from any buffer adds a reference to it to the pai prompt (pai binds
;; it too once loaded; binding it here makes it work before that).
(global-set-key (kbd "C-c i") #'pai-add-to-prompt)

;; Leader keys: SPC o p (o = user prefix, p = pai).
(spacemacs/set-leader-keys
  "opp" 'pai                    ; chat for this project
  "opn" 'pai-new-session        ; fresh session
  "opi" 'pai-add-to-prompt      ; reference this buffer/region in the prompt
  "ops" 'pai-settings-ui-open   ; settings (/menu)
  "opa" 'pai-add-provider       ; add an LLM provider
  "opI" 'my-pai/install         ; clone pai + extensions
  "opu" 'my-pai/update)         ; git pull both
;; The prefix label needs which-key, which is not loaded yet at this point.
(with-eval-after-load 'which-key
  (spacemacs/declare-prefix "op" "pai"))

;;; config.el ends here
