(setq combobulate-packages
      '((combobulate :location local)))

(defun combobulate/init-combobulate ()
  (use-package combobulate
    :defer nil
    :init
    (progn
      ;; Set key prefix for Combobulate (optional)
      (setq combobulate-key-prefix "C-c o")
      ;; Enable Combobulate mode for programming modes
      (add-hook 'prog-mode-hook #'combobulate-mode))
    :config
    (require 'combobulate)))
