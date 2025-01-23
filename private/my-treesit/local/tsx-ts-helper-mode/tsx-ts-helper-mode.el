;;; tsx-ts-helper-mode.el --- Helpers for writing TypeScript TSX code -*- lexical-binding: t -*-

;; Copyright (C) 2022-2022  Christian Kruse <christian@kruse.cool>

;; Author: Christian Kruse <christian@kruse.cool>
;; URL: https://codeberg.org/ckruse/tsx-ts-helper-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: typescript, tsx, treesit, tree-sitter

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; With this mode, you can manage tree-sitter grammars.  It allows you to
;; install, update and remove tree-sitter grammars.

;;; Code:
(defcustom tsx-ts-helper-mode-auto-close-tags t
  "If non-nil, automatically close tags."
  :type 'boolean
  :group 'tsx-ts-helper-mode)

(defcustom tsx-ts-helper-mode-keymap-prefix (kbd "C-c t")
  "Tsx-ts-helper-mode keymap prefix."
  :group 'tsx-ts-helper-mode
  :type 'string)


(defvar tsx-ts-helper-command-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'tsx-ts-helper-mode-rename-tag)
    (define-key map (kbd "di") 'tsx-ts-helper-mode-delete-inner)
    (define-key map (kbd "dt") 'tsx-ts-helper-mode-delete-all)
    (define-key map (kbd "da") 'tsx-helper-mode-delete-attribute)
    map)
  "Keymap for `tsx-ts-helper-mode'.")

(defvar tsx-ts-helper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<") 'tsx-helper-mode-maybe-insert-self-closing-tag)
    (define-key map (kbd ">") 'tsx-helper-mode-maybe-close-tag)

    (when tsx-ts-helper-mode-keymap-prefix
      (define-key map tsx-ts-helper-mode-keymap-prefix tsx-ts-helper-command-mode-map))
    map)
  "Keymap for `tsx-ts-helper-mode'.")

(defun tsx-helper-mode-maybe-insert-self-closing-tag ()
  "Insert a self-closing tag if the point is at the end of an opening tag."
  (interactive)

  (if tsx-ts-helper-mode-auto-close-tags
      (let ((node-type (treesit-node-type (treesit-node-at (point))))
            (parent-type (treesit-node-type (treesit-node-parent (treesit-node-at (point))))))

        (insert "<")
        (when (or (string= node-type "jsx_text")
                  (string= parent-type "jsx_opening_element")
                  (string= parent-type "jsx_closing_element")
                  (string= parent-type "jsx_fragment"))
          (insert "/>")
          (backward-char 2)))
    (insert "<")))

(defun tsx-ts-helper-mode--close-at-point-p ()
  "Return t if a self-closing tag at point can be turned into an opening and a closing tag."
  (or
   (when-let* ((current-named-node (treesit-node-at (point) nil t))
               (current-named-node-type (treesit-node-type current-named-node)))
     ;; self-closing tags can be turned into regular tag sets
     (or (string= current-named-node-type "jsx_self_closing_element")
         (string= current-named-node-type "jsx_fragment")))
   (save-excursion
     (backward-char 1)
     (looking-at-p "</>"))))

(defun tsx-ts-helper-mode--is-fragment ()
  "Return t if the current node is a fragment."
  (save-excursion
    (backward-char 1)
    (looking-at-p "</>")))

(defun tsx-helper-mode-maybe-close-tag ()
  "Close the current tag if the point is at the end of an opening tag."
  (interactive)
  (if (and
       tsx-ts-helper-mode-auto-close-tags
       (tsx-ts-helper-mode--close-at-point-p))
      (let* ((node-element-name
              (if (tsx-ts-helper-mode--is-fragment)
                  ""
                (save-excursion
                  (goto-char (treesit-node-start (treesit-node-at (point) nil t)))
                  (re-search-forward "<\\([-a-zA-Z0-9$_.]+\\)" nil t)
                  (match-string 1))))
             (str (format "></%s>" (if (string= node-element-name "/") "" node-element-name))))
        (re-search-forward "/>" nil t)
        (delete-char -2)
        (insert str)
        (backward-char (- (length str) 1)))
    (insert ">")))

;;;###autoload
(defun tsx-ts-helper-mode-rename-tag ()
  "Rename the current tag."
  (interactive)

  (let* ((cur-node (treesit-node-at (point) nil t))
         (parent-node (treesit-parent-until cur-node
                                            (lambda (node)
                                              (or
                                               (string= (treesit-node-type node) "jsx_element")
                                               (string= (treesit-node-type node) "jsx_self_closing_element")))))
         (parent-node-type (treesit-node-type parent-node))
         (element-node (treesit-node-child
                        parent-node
                        (if (string= parent-node-type "jsx_element")
                            0
                          1)))
         (closing-node (if (string= parent-node-type "jsx_element")
                           (treesit-node-child parent-node (- (treesit-node-child-count parent-node) 1))
                         nil))
         (node-name (treesit-node-text
                     (if (string= parent-node-type "jsx_element")
                         (treesit-node-child element-node 1)
                       element-node)))

         (new-name (read-string (format "New name for %s: " node-name)))
         (start (if (string= parent-node-type "jsx_element")
                    (treesit-node-start element-node)
                  (treesit-node-start parent-node)))
         (start-closing (if closing-node
                            (treesit-node-start closing-node)
                          nil))
         (name-len-diff (- (length new-name) (length node-name))))
    (save-excursion
      (goto-char start)
      (re-search-forward "<\\([-a-zA-Z0-9$_.]+\\)" nil t)
      (replace-match new-name nil nil nil 1)

      (when start-closing
        (goto-char (+ start-closing name-len-diff))
        (re-search-forward "</\\([-a-zA-Z0-9$_.]+\\)" nil t)
        (replace-match new-name nil nil nil 1)))))

(defun tsx-ts-helper-mode--delete-tag-inner (parent-node)
  "Delete the inner content of the current non-self-closing tag."
  (let* ((element-node (treesit-node-child parent-node 0))
         (closing-node (treesit-node-child parent-node (- (treesit-node-child-count parent-node) 1)))
         (start (treesit-node-end element-node))
         (end (treesit-node-start closing-node)))
    (kill-region start end)))

;;;###autoload
(defun tsx-ts-helper-mode-delete-inner ()
  "Delete the inner content of the current tag."
  (interactive)
  (let* ((cur-node (treesit-node-at (point) nil t))
         (parent-node (treesit-parent-until cur-node
                                            (lambda (node)
                                              (or
                                               (string= (treesit-node-type node) "jsx_element")
                                               (string= (treesit-node-type node) "jsx_self_closing_element"))))))
    (if (string= (treesit-node-type parent-node) "jsx_element")
        (tsx-ts-helper-mode--delete-tag-inner parent-node)
      (message "A self-closing tag has no inner conetnt."))))

(defun tsx-ts-helper-mode--delete-tag-all (parent-node)
  "Delete the current non-selfclosing tag and all its children."
  (let* ((element-node (treesit-node-child parent-node 0))
         (closing-node (treesit-node-child parent-node (- (treesit-node-child-count parent-node) 1)))
         (start (treesit-node-start element-node))
         (end (treesit-node-end closing-node)))
    (kill-region start end)))

(defun tsx-ts-helper-mode--delete-self-closing-tag-all (parent-node)
  "Delete the current self-closing tag and all its children."
  (let* ((start (treesit-node-start parent-node))
         (end (treesit-node-end parent-node)))
    (kill-region start end)))

;;;###autoload
(defun tsx-ts-helper-mode-delete-all ()
  "Delete the current tag and its content."
  (interactive)
  (let* ((cur-node (treesit-node-at (point) nil t))
         (parent-node (treesit-parent-until cur-node
                                            (lambda (node)
                                              (or
                                               (string= (treesit-node-type node) "jsx_element")
                                               (string= (treesit-node-type node) "jsx_self_closing_element"))))))
    (if (string= (treesit-node-type parent-node) "jsx_element")
        (tsx-ts-helper-mode--delete-tag-all parent-node)
      (tsx-ts-helper-mode--delete-self-closing-tag-all parent-node))))

(defun tsx-helper-mode-delete-attribute ()
  "Delete the current attribute."
  (interactive)
  (let* ((cur-node (treesit-node-at (point)))
         (attr-node (treesit-parent-until cur-node (lambda (node)
                                                     (string= (treesit-node-type node) "jsx_attribute"))))
         (node-type (treesit-node-type attr-node))
         (start (treesit-node-start attr-node))
         (end (treesit-node-end attr-node)))
    (if (string= node-type "jsx_attribute")
        (kill-region start end)
      (message "Not an attribute."))))

;;;###autoload
(define-minor-mode tsx-ts-helper-mode
  "Helper mode for ‘tsx-ts-mode’, enabling auto-closing tags."
  :lighter " tsx-ts-helper"
  :keymap tsx-ts-helper-mode-map)

(provide 'tsx-ts-helper-mode)
;;; tsx-ts-helper-mode.el ends here
