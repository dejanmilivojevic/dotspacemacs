(defconst my-jtsx-packages
  '(
    (jtsx :ensure t)
    ))

(defun my-jtsx/init-jtsx ()
  (use-package jtsx
    ;; :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
    ;;        ("\\.tsx\\'" . jtsx-tsx-mode)
    ;;        ("\\.ts\\'" . jtsx-typescript-mode))
    :commands jtsx-install-treesit-language
    :hook ((jtsx-jsx-mode . hs-minor-mode)
           (jtsx-tsx-mode . hs-minor-mode)
           (jtsx-typescript-mode . hs-minor-mode))
    :config
    ;; Define custom functions and key bindings
    (defun jtsx-bind-keys-to-mode-map (mode-map)
      "Bind keys to MODE-MAP."
      (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
      (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
      (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
      (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
      (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
      (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
      (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
      (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
      (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
      (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
      (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
      (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
      (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node)
      (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
      (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
      (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))

    (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

    (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

    (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
    (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)))
