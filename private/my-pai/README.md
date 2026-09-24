# my-pai

Spacemacs layer for [pai](https://github.com/dejanmilivojevic/pai), the Pi
Agent for Emacs, with all of
[its extensions](https://github.com/dejanmilivojevic/pai-extensions).

## Layout

| Path | What |
|------|------|
| `my-pai/pai/` | git clone of the pai core (ignored by this repo) |
| `my-pai/pai/extensions/` | git clone of pai-extensions (ignored by this repo) |
| `~/.pai/extensions` | symlink to `my-pai/pai/extensions/` |

pai loads every extension under `~/.pai/extensions` for each session. Which
ones are active is kept in `~/.pai/settings.json` (`/menu` → *Extensions*);
`pai-subagents` stays off because it overlaps with
`pai-interactive-subagents` (both register the `subagent` tool).

Providers, models and API keys live in `~/.pai/`, never in this layer.

## Install

Add `my-pai` to `dotspacemacs-configuration-layers`, then run
`M-x my-pai/install`. It clones both repositories in the background, links
`~/.pai/extensions` and sets pai up. `M-x my-pai/update` pulls both.

## Keys

| Key | Command |
|-----|---------|
| `SPC o p p` | `pai`: chat for this project |
| `SPC o p n` | `pai-new-session` |
| `SPC o p i`, `C-c i` | `pai-add-to-prompt`: reference this buffer or region |
| `SPC o p s` | settings screen |
| `SPC o p a` | add an LLM provider |
| `SPC o p I` | `my-pai/install` |
| `SPC o p u` | `my-pai/update` |
