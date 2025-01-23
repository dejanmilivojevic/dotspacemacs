(defconst my-aider-packages
  '(
    (aider :location (recipe
                      :fetcher github
                      :repo "tninja/aider.el"
                      :files ("aider.el")))
    ))



(defun my-aider/init-aider ()
  (use-package aider
    :config

    ;; ------------------------------------------------------------------------
    ;; Default Model (Anthropic Claude) - recommended in aider benchmark
    ;; ------------------------------------------------------------------------
    (setq aider-args '("--openai-api-base=http://127.0.0.1:1235/v1/" "--openai-api-key=123"))
    ;; Replace the string below with your real key:
    ;; (setenv "ANTHROPIC_API_KEY" "YOUR_ANTHROPIC_API_KEY")

    ;; ------------------------------------------------------------------------
    ;; Alternative Model (ChatGPT)
    ;; Uncomment these lines to use GPT-4o-mini, and comment out the lines above.
    ;; ------------------------------------------------------------------------
    ;; (setq aider-args '("--model" "gpt-4o-mini"))
    ;; (setenv "OPENAI_API_KEY" "YOUR_OPENAI_API_KEY")

    ;; ------------------------------------------------------------------------
    ;; Another Model (Gemini)
    ;; Uncomment these lines to use Gemini, and comment out the lines above.
    ;; ------------------------------------------------------------------------
    ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
    ;; (setenv "GEMINI_API_KEY" "YOUR_GEMINI_API_KEY")

    ;; Optional: Set a global keybinding for the Aider menu
    (global-set-key (kbd "C-c a") 'aider-transient-menu)))
