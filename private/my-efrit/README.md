# Spacemacs Layer: my-efrit

> **Spacemacs layer for Efrit — AI-powered Emacs coding assistant.**

This layer integrates [Efrit](https://github.com/steveyegge/efrit) into Spacemacs,
with pre-configured support for a **local LLM server** (llama.cpp).

## What you get

| Command | Description |
|---------|-------------|
| `M-x efrit-chat` | Start conversational chat |
| `M-x efrit-do` | Execute natural language commands |
| `M-x efrit-do-sync` | Execute command synchronously |
| `M-x efrit-do-show-progress` | View progress buffer |
| `M-x efrit-do-show-queue` | View queued commands |
| `M-x efrit-doctor` | Run health diagnostics |

## Quick Start

### 1. Make sure you have a llama.cpp server running

```bash
./llama.cpp/llama-cpp-server -m Llama-3.1-8B-Instruct-Q4_K_M.gguf \
  --host 127.0.0.1 --port 8080 \
  --openai-api --chat-template
```

### 2. Enable the layer in `~/.spacemacs/custom.el`

```elisp
(spacemacs-configure-layer 'my-efrit)
```

### 3. (Optional) Override the model

```elisp
(spacemacs-configure-layer 'my-efrit
  :config '(
    ;; Use a different model
    (setq efrit-default-model "mistralai/Mistral-7B-Instruct-v0.1")
  ))
```

## How It Works

This layer:

1. **Clones efrit** into `~/.spacemacs/efrit/`
2. **Patches it** to use OpenAI-compatible API calls instead of Anthropic
3. **Points the API URL** to `http://127.0.0.1:8080/v1/chat/completions`
4. **Loads `efrit-openai`** — the custom backend module that handles:
   - OpenAI-style request building (`/v1/chat/completions`)
   - OpenAI-style response parsing (`{"choices": [...]}`)
   - Unified tool-use handling (both `tool_use` and `tool_calls` formats)

## Supported Models

Any model that supports the OpenAI-compatible API and tool calling:

| Model | Where to get it |
|-------|----------------|
| Llama-3.1-8B-Instruct | HuggingFace / Ollama |
| Llama-3.1-70B-Instruct | HuggingFace |
| Claude 3.5 Sonnet | Ollama (`ollama pull claude-3-5-sonnet`) |
| Mistral-7B-Instruct-v0.1 | HuggingFace / Ollama |
| Qwen2.5-7B-Instruct | HuggingFace |

## Keybindings

By default, Efrit uses `C-c C-e` prefix:

| Key | Action |
|-----|--------|
| `C-c C-e c` | Chat mode |
| `C-c C-e d` | Async command with progress buffer |
| `C-c C-e D` | Async command in background |

## Troubleshooting

```elisp
;; Run diagnostics
M-x efrit-doctor

;; Check if your server is reachable
M-x efrit-do RET "what buffer am I in?"

;; Enable debug logging
M-: (setq efrit-log-level 'debug)
M-x efrit-log-show
```

## Files

```
~/.spacemacs/private/my-efrit/
└── my-efrit.lisp          ← Layer file (this one)
└── README.md              ← This file

~/.spacemacs/efrit/      ← Cloned efrit project (patched)
├── lisp/
│   ├── efrit.el                    # Efrit main entry point
│   ├── core/
│   │   ├── efrit-config.el         # Added efrit-api-backend custom (default: 'openai)
│   │   ├── efrit-chat-classic.el   # Patched: OpenAI response parsing
│   │   ├── efrit-chat-streamlined.el # Already OpenAI-compatible
│   │   ├── efrit-common.el         # Added efrit-openai--* helpers
│   │   └── efrit-executor.el       # Backend-agnostic
│   ├── interfaces/
│   │   ├── efrit-do.el             # Backend-agnostic
│   │   └── efrit-agent.el          # Backend-agnostic
│   └── efrit-openai.el              # NEW: OpenAI backend module
```

