;;; packages.el --- my-flutter layer packages

(defconst my-flutter-packages '(websocket))

(defun my-flutter/init-websocket ()
  (use-package websocket :defer t))
