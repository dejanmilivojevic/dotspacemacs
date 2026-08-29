;;; efrit.lisp --- Spacemacs layer for Efrit (AI-powered Emacs coding assistant) -*- lexical-binding: t -*-

;; Copyright (C) 2025
;;
;; Author: Auto-generated for local use
;; Keywords: tools, convenience, ai, spacemacs
;; URL: https://github.com/steveyegge/efrit

;; Commentary:

;; This Spacemacs layer integrates Efrit — an AI-powered coding agent for Emacs.
;; It uses a local LLM server (llama.cpp) instead of the default Anthropic API.
;;
;; Features:
;; - M-x efrit-chat       : Start conversational chat
;; - M-x efrit-do         : Execute natural language commands
;; - M-x efrit-do-sync    : Execute command synchronously
;; - M-x efrit-do-show-progress : View progress buffer
;; - M-x efrit-do-show-queue  : View queued commands
;; - M-x efrit-doctor     : Run health diagnostics
;;
;; Configuration:
;;   (use-package efrit
;;     :config
;;       (setq efrit-api-backend 'openai)
;;       (setq efrit-default-model "meta-llama/Llama-3.1-8B-Instruct")
;;       ;; For llama.cpp with no auth:
;;       ;; (setq efrit-api-key nil)
;;       ;; For servers that require a key:
;;       ;; (setq efrit-api-key "your-key")
;;       ;; Load the OpenAI-compatible backend:
;;       (require 'efrit-openai))

;; Code:

(require 'spacemacs)

;; Load Efrit from the local copy in dotspacemacs
;; The layer expects efrit to be cloned into the dotspacemacs directory
(let ((efrit-path (expand-file-name "efrit" user-emacs-directory)))
  (when (file-directory-p efrit-path)
    ;; Add lisp subdirectory to load-path
    (add-to-list 'load-path (expand-file-name "lisp" efrit-path))
    
    ;; Add the top-level directory to load-path too
    (add-to-list 'load-path efrit-path)
    
    ;; Load efrit
    (require 'efrit)
    
    ;; Load the OpenAI-compatible backend
    (require 'efrit-openai)
    
    ;; Configure for llama.cpp backend
    (unless (boundp 'efrit-config-loaded)
      (let ((efrit-config (load-file (concat efrit-path "/lisp/core/efrit-config.el"))))
        ;; Only apply defaults if user hasn't overridden them
        (when (not (boundp 'efrit-api-backend))
          (eval '(defcustom efrit-api-backend 'openai
                   :type '(choice (const :tag "Anthropic" 'anthropic)
                                (const :tag "OpenAI-compatible" 'openai))
                   :group 'efrit)))
        (when (not (boundp 'efrit-default-model))
          (eval '(defcustom efrit-default-model "meta-llama/Llama-3.1-8B-Instruct"
                   :type 'string
                   :group 'efrit)))
        (setf 'efrit-config-loaded t)))
    
    ;; Configure keybindings
    (if (boundp 'efrit-keybindings)
        (if (not (boundp 'efrit-enable-keybindings))
            (eval '(efrit-setup-keybindings)))
        (if (not (boundp 'efrit--keybindings-set))
            (progn
              (eval '(efrit-setup-keybindings))
              (setf 'efrit--keybindings-set t)))))

;; Key bindings for the layer
(define-key (make-keymap 'efrit-layer)
  (kbd "M-x efrit-chat")
  #'efrit-chat

(define-key (make-keymap 'efrit-layer)
  (kbd "M-x efrit-do")
  #'efrit-do

(define-key (make-keymap 'efrit-layer)
  (kbd "M-x efrit-do-show-progress")
  #'efrit-do-show-progress

(define-key (make-keymap 'efrit-layer)
  (kbd "M-x efrit-do-show-queue")
  #'efrit-do-show-queue

(define-key (make-keymap 'efrit-layer)
  (kbd "M-x efrit-doctor")
  #'efrit-doctor

(provide 'my-efrit)
