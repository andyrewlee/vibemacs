# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is **vibemacs** - an Emacs configuration optimized for "vibe coders" with a worktree management system and chat-first AI assistants (Claude, Gemini, or any CLI you map in `vibemacs-worktrees-chat-assistants`). Codex-specific plan/apply helpers were removed; chat buffers now simply launch the configured command in vterm. The configuration uses Evil mode (Vim keybindings) with a custom SPC-based leader key system.

## Core Architecture

### Worktree System (`etc/worktrees*.el`)

The heart of vibemacs is a modular worktree management system split into focused components:

- **worktrees-core.el**: Data structures, faces, customizations, and shared utilities
- **worktrees-git.el**: Git operations (discover worktrees, branch creation/cleanup)
- **worktrees-registry.el**: Persistent project list in `~/.vibemacs/projects.json`
- **worktrees-metadata.el**: Per-worktree metadata (assistant config, custom env vars)
- **worktrees-process.el**: Terminal and script execution (setup/run/archive scripts)
- **worktrees-chat.el**: AI assistant chat buffer management via vterm
- **worktrees-dashboard.el**: Dashboard UI (tabulated-list-mode) and dispatcher
- **worktrees-git-status.el**: Git status sidebar with auto-refresh + sidebar terminal
- **worktrees-layout.el**: Window layout management and pane orchestration (center tabs for chat/diff/terminal)
- **worktrees.el**: Entry point that requires all modules in dependency order

### Key Concepts

1. **Worktree Entry Structure**: Each worktree has name, branch, repo (primary checkout), root (worktree path), base (branch it was created from), created timestamp
2. **Active Root**: `vibemacs-worktrees--active-root` tracks the currently focused worktree
3. **Three-Pane Layout**: Dashboard (left) | Chat/Terminal (center) | Git Status (right)
4. **Assistant Integration**: Configurable AI assistants via vterm with custom interrupt handling
5. **Metadata Storage**: Per-worktree JSON files in `~/.vibemacs/worktrees-metadata/<repo-hash>/worktree.json`

## Development Commands

### Package Management

```elisp
;; Refresh package archives manually
(package-refresh-contents)

;; Install a package
(package-install 'package-name)
```

Packages are managed via `use-package` with automatic installation through a custom `vibemacs-use-package-ensure` function that auto-refreshes archives once on failure.

### Evaluating Changes

```elisp
;; Evaluate buffer (from within Emacs)
M-x eval-buffer

;; Or with keybinding
SPC b e
```

### Testing Worktree Functions

```elisp
;; Open dashboard
M-x vibemacs-worktrees-dashboard

;; Create new worktree
M-x vibemacs-worktrees-new

;; List all worktrees
M-x vibemacs-worktrees-list
```

## Code Organization Patterns

### Module Dependencies

When adding features to the worktree system:
1. Core definitions go in `worktrees-core.el`
2. Use `declare-function` for forward declarations to avoid circular dependencies
3. Module load order in `worktrees.el` is critical - respect the dependency chain

### Buffer Naming Conventions

- Dashboard: `*vibemacs*`
- Git status sidebar: `*vibemacs-git*`
- Chat buffers: `*vibemacs Chat <worktree-name>*`
- Terminal buffers: `*worktree-<name>-term*`
- Diff review: `*vibemacs Diff*`

### Metadata Access Pattern

Always use the helper functions:
- `vibemacs-worktrees--load-metadata` to read
- `vibemacs-worktrees--save-metadata` to persist
- `vibemacs-worktrees--ensure-metadata` to initialize

### Evil Mode Integration

When adding new keybindings to custom modes:
```elisp
(eval-after-load 'evil
  '(when (fboundp 'evil-define-key)
     (dolist (state '(normal motion))
       (evil-define-key state your-mode-map
         (kbd "key") #'your-function))))
```

## TypeScript/JavaScript Development

The config includes full TypeScript support via:
- **treesit-auto**: Automatic tree-sitter grammar installation
- **eglot**: LSP client (requires `typescript-language-server` installed via npm)
- **corfu**: Auto-completion with orderless matching
- **apheleia**: Format-on-save with eslint_d and prettier

External dependencies (must be installed globally):
```bash
npm i -g typescript typescript-language-server prettier eslint_d
```

## Git Integration

- **magit**: Full-featured Git interface
- **diff-hl**: Gutter highlights for changes
- Custom git status sidebar with real-time auto-refresh (file watcher on `.git/index`, after-save hook, periodic timer)

## Common Patterns

### Vterm Integration

Vterm is required for terminals and chat buffers. Check availability:
```elisp
(vibemacs-worktrees--ensure-vterm)
```

### Transient Menus

The dispatcher uses `transient.el`:
```elisp
(transient-define-prefix vibemacs-worktrees-dispatch ()
  ...)
```

Access with `SPC a w` or `M-x vibemacs-worktrees-dispatch`.

### Tabulated List Mode

Dashboard and list views use `tabulated-list-mode`:
- Entries are lists of `(ID VECTOR)`
- ID is typically the worktree root path
- Use `vibemacs-entry` text property to attach full entry objects to cells

## Important Notes

- **Custom File**: Customizations are stored in `custom.el` (not `init.el`)
- **Theme**: doom-one with disabled bold/italic and subdued comments/modeline
- **Leader Key**: SPC in normal/visual/motion, M-SPC globally
- **Port Allocation**: Each worktree gets a reserved port range (starting at 6200, 10 ports per worktree)
- **Auto-save**: `delete-trailing-whitespace` runs on every save
- **Primary Checkout Protection**: Cannot delete a worktree if its root equals the repo path
- **AI flows**: Codex plan/apply/diff automation is gone; use chat buffers (e.g., `vibemacs-worktrees-create-plan`, `vibemacs-worktrees-research-codebase`) with whichever assistant command is configured.

## File Locations

- **Main init**: `~/.emacs.d/init.el`
- **Custom settings**: `~/.emacs.d/custom.el`
- **Worktree modules**: `~/.emacs.d/etc/worktrees*.el`
- **Registry**: `~/.vibemacs/projects.json` (or `$VIBEMACS_HOME/projects.json`)
- **Per-project scripts**: `.vibemacs/worktrees.json` inside a repo controls setup/run/archive commands
- **Metadata**: `~/.vibemacs/worktrees-metadata/<repo-hash>/worktree.json`
- **Worktree checkouts**: `~/.vibemacs/worktrees/<repo-name>/<worktree-name>/`
