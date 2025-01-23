(setq my-treesit-packages
      '((treesit :location built-in)
        (tsx-ts-helper-mode :location local)))


(defun mp-setup-install-grammars ()
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
             (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
             (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
             (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
             (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
    (add-to-list 'treesit-language-source-alist grammar)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(defun my-treesit/init-treesit ()
  (use-package treesit
    :init
    ;; Access the layer variable
    (when (and (boundp 'my-treesit-enable-remap)
               (symbol-value 'my-treesit-enable-remap))
      (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . tsx-ts-mode)
                 (typescript-tsx-mode . tsx-ts-mode)
                 (js2-mode . js-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (conf-toml-mode . toml-ts-mode)
                 (go-mode . go-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping)))
    :config
    (mp-setup-install-grammars)))

(defun my-treesit/init-tsx-ts-helper-mode ()
  (use-package tsx-ts-helper-mode
    :after tsx-ts-mode
    :hook (tsx-ts-mode . tsx-ts-helper-mode)))
