;;; packages.el --- my-treemacs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: dexy <dexy@dexys-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-treemacs-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-treemacs/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-treemacs/pre-init-PACKAGE' and/or
;;   `my-treemacs/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(defconst my-treemacs-packages '(treemacs treemacs-projectile))

(defun my-treemacs/init-treemacs ()
  (use-package treemacs
    :defer t
    :init
    (spacemacs/set-leader-keys
      "fT" #'treemacs
      "ft" #'treemacs-toggle
      "pt" #'treemacs-projectile)
    :config
    (require 'treemacs-projectile)))

(defun my-treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :defer t))


  ;; (use-package treemacs
  ;;   :ensure t
  ;;   :defer t
  ;;   :config
  ;;   (progn
  ;;     (setq treemacs-change-root-without-asking nil
  ;;           treemacs-collapse-dirs              (if (executable-find "python") 3 0)
  ;;           treemacs-file-event-delay           5000
  ;;           treemacs-follow-after-init          t
  ;;           treemacs-goto-tag-strategy          'refetch-index
  ;;           treemacs-indentation                2
  ;;           treemacs-indentation-string         " "
  ;;           treemacs-is-never-other-window      nil
  ;;           treemacs-never-persist              nil
  ;;           treemacs-no-png-images              nil
  ;;           treemacs-recenter-after-file-follow nil
  ;;           treemacs-recenter-after-tag-follow  nil
  ;;           treemacs-show-hidden-files          t
  ;;           treemacs-silent-filewatch           nil
  ;;           treemacs-silent-refresh             nil
  ;;           treemacs-sorting                    'alphabetic-desc
  ;;           treemacs-tag-follow-cleanup         t
  ;;           treemacs-tag-follow-delay           1.5
  ;;           treemacs-winum-number               10
  ;;           treemacs-width                      35)

  ;;     (treemacs-follow-mode t)
  ;;     (treemacs-filewatch-mode t)
  ;;     (pcase (cons (not (null (executable-find "git")))
  ;;                  (not (null (executable-find "python3"))))
  ;;       (`(t . t)
  ;;        (treemacs-git-mode 'extended))
  ;;       (`(t . _)
  ;;        (treemacs-git-mode 'simple))))
  ;;   :bind
  ;;   (:map global-map
  ;;         ([f8]         . treemacs-toggle)
  ;;         ("M-0"        . treemacs-select-window)
  ;;         ("C-c 1"      . treemacs-delete-other-windows)
  ;;         ("M-m ft"     . treemacs-toggle)
  ;;         ("M-m fT"     . treemacs)
  ;;         ("M-m fB"     . treemacs-bookmark)
  ;;         ("M-m f C-t"  . treemacs-find-file)
  ;;         ("M-m f M-t"  . treemacs-find-tag)))
  ;; (use-package treemacs-projectile
  ;;   :defer t
  ;;   :ensure t
  ;;   :config
  ;;   (setq treemacs-header-function #'treemacs-projectile-create-header)
  ;;   :bind (:map global-map
  ;;               ("M-m fP" . treemacs-projectile)
  ;;               ("M-m fp" . treemacs-projectile-toggle)))


;;; packages.el ends here
