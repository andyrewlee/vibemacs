# CLAUDE.md

Guide for AI assistants and contributors working with vibemacs.

## Purpose

Vibemacs is an Emacs Lisp configuration delivering modal, leader-key-driven editing for JavaScript/TypeScript and Markdown work. It provides streamlined Git worktree management with AI-assisted diffs via Codex CLI integration.

**Core workflows:**
- JavaScript/TypeScript development with LSP, auto-completion, and formatting
- Markdown editing with visual polish
- Git worktree management with per-worktree terminals and activity tracking
- AI-assisted code changes through Codex CLI (plan/review/apply diffs)

## Project Overview

**Key files:**
- [`init.el`](./init.el) – Bootstraps packages, configures Evil/general.el, sets up treesit-auto, Eglot, Corfu, Apheleia, Vertico/Consult, Magit, vterm, solarized-theme
- [`etc/worktrees.el`](./etc/worktrees.el) – Custom module for worktree registry, dashboard layout, per-worktree terminals, run/setup scripts, activity logs, Codex CLI integration

**Dependencies:**
- Evil mode + evil-collection for modal editing
- general.el for leader-key bindings (SPC prefix)
- treesit-auto for syntax highlighting (auto-installs grammars)
- Eglot for LSP integration
- Corfu + Orderless for completions
- Apheleia for format-on-save (prettier + eslint_d)
- Vertico + Consult + Marginalia for minibuffer enhancements
- Magit + diff-hl for Git operations
- vterm + multi-vterm for terminal integration
- solarized-theme for colors

## Repository Layout

```
/
├── init.el              # Main configuration entry point
├── etc/
│   └── worktrees.el     # Worktree workflow module
├── custom.el            # User customizations (auto-generated, ignored by git)
└── README.md            # Project description
```

**Runtime behavior:**
- `init.el` adds `etc/` to load-path, requires `worktrees` module
- Startup hook launches worktrees dashboard if `vibemacs-worktrees-startup-layout` is non-nil
- User customizations live in `custom.el` (separate from `init.el`)
- Package installs go to `~/.emacs.d/elpa/` (standard MELPA location)

## Setup & Run

**Launch Emacs with this config:**

```bash
# Use as main config (symlink or copy to ~/.emacs.d)
ln -s /path/to/vibemacs ~/.emacs.d
emacs

# Or use --init-directory (Emacs 29+)
emacs --init-directory=/path/to/vibemacs
```

**Requirements:**
- Emacs 29+ (for treesit, init-directory, modern LSP support)
- Git (for worktree operations)
- Node.js + npm (for TypeScript/JavaScript tooling)

**External tools (install via npm/homebrew):**

```bash
# TypeScript LSP
npm i -g typescript typescript-language-server

# Formatters/linters
npm i -g prettier eslint_d

# Search
brew install ripgrep  # or apt-get install ripgrep

# Codex CLI (optional, for AI-assisted diffs)
# Installation depends on your Codex setup
```

**First launch:**
- Emacs auto-installs packages from MELPA on first run
- treesit-auto prompts to install tree-sitter grammars (confirm for TypeScript/JavaScript/Markdown)
- Eglot starts LSP servers when opening JS/TS files (requires typescript-language-server)
- Apheleia formats buffers on save (requires prettier/eslint_d in PATH)
- Worktrees dashboard appears at startup (if `vibemacs-worktrees-startup-layout` is t)

## Development Workflow

### Validate Changes

**Byte-compile Emacs Lisp:**

```bash
emacs -Q --batch -L . -f batch-byte-compile init.el etc/worktrees.el
```

**Run checkdoc:**

```elisp
;; In Emacs, open init.el or etc/worktrees.el, then:
M-x checkdoc
```

**package-lint (if available):**

```elisp
;; Install package-lint from MELPA
M-x package-install RET package-lint RET

;; Run on buffer
M-x package-lint-current-buffer
```

### Formatting

**Apheleia auto-formats on save** for configured modes (TypeScript, TSX, Markdown).

**Manual format:**
- `SPC l f` (or `M-x apheleia-format-buffer`) formats current buffer
- Configured formatters: `prettier`, `eslint_d` (chained for TS/TSX)

**To disable auto-format:**

```elisp
M-x apheleia-global-mode  ; toggles global mode
```

### Git Worktree Workflow

**Concepts:**
- Worktrees are tracked in `~/.vibemacs/worktrees.json` (registry)
- Dashboard shows all worktrees at startup (left pane, 24 columns)
- Center pane displays active worktree's terminal or chat buffer
- Per-worktree metadata includes assistant choice, run/setup scripts, activity logs

**Dashboard operations:**
- `RET` on worktree row: Activate (focus center pane on that worktree)
- `c`: Run Codex plan for selected worktree
- `A`: Run Codex plan + auto-apply changes
- `D`: Delete worktree (removes from registry + filesystem)
- `q`: Quit dashboard

**Transient dispatcher (from anywhere):**
- `SPC a w` opens worktrees transient menu
- Options: create, sync, open dashboard, run Codex plan, apply diffs

**Per-worktree terminals:**
- Named `*worktree-<name>-term*`
- Auto-created when activating a worktree
- Displayed in center pane of startup layout
- Use `SPC a t` for new terminal, `SPC a n`/`SPC a p` to cycle terminals

**Activity logs:**
- Captured in `*Worktrees Activity*` buffer (persistent across sessions)
- Shows Codex invocations, diffs applied, errors
- Limit: 40 entries per worktree (configurable via `vibemacs-worktrees-codex-log-limit`)

### Codex CLI Integration

**Prerequisites:**
- Install `codex` CLI and ensure it's in PATH
- Configure `vibemacs-worktrees-codex-executable` if non-standard location

**Workflow:**
1. Open dashboard (`SPC a w` → dashboard) or use transient menu
2. Select worktree, press `c` (plan) or `A` (plan + apply)
3. Codex runs in background, streams output to activity log
4. On completion:
   - Diff appears in `*vibemacs Diff*` buffer (custom diff-mode)
   - If `vibemacs-worktrees-review-display` is `magit`, Magit status opens automatically
5. Review diff:
   - `a` apply hunk
   - `r` reject hunk
   - `C-c C-f` follow-up (prompt Codex again with context)
6. Magit integration:
   - Changes surface in Magit status
   - Use `SPC g s` to open Magit anytime
   - Commit/push as usual

**Transient entry points:**
- `vibemacs-worktrees-dispatch` (bound to `SPC a w`)
- Dashboard actions (`c`, `A` keys on worktree rows)

**Customization:**
- `vibemacs-worktrees-chat-assistants`: Map assistant names to CLI commands
- `vibemacs-worktrees-default-assistant`: Default assistant for new worktrees
- `vibemacs-worktrees-review-display`: `magit` or `none` (controls auto-opening Magit status)

## Conventions & Constraints

### Emacs Lisp Style

**Required headers:**
- `;;; <filename>.el --- <description> -*- lexical-binding: t; -*-` at top
- `(provide '<feature-name>)` at bottom (for library files)

**Naming:**
- Prefix all symbols with package name: `vibemacs-`, `vibemacs-worktrees--` (double-dash for internal)
- Use `defcustom` for user-facing options, `defvar` for internals
- Autoload interactive commands with `;;;###autoload` if in separate module

**Conventions:**
- Always enable `lexical-binding: t`
- Use `require` for dependencies at top of file
- Declare external functions with `declare-function` to avoid byte-compile warnings
- Avoid global side effects in library files (move `add-hook`, `global-mode` calls to `init.el`)

**Example:**

```elisp
;;; my-module.el --- Brief description -*- lexical-binding: t; -*-

(require 'some-package)

(defcustom my-module-option t
  "User-facing option."
  :type 'boolean
  :group 'my-module)

(defvar my-module--internal nil
  "Internal variable.")

;;;###autoload
(defun my-module-command ()
  "Interactive command users call."
  (interactive)
  ...)

(provide 'my-module)
;;; my-module.el ends here
```

### Keybindings Philosophy

**Leader key:** `SPC` (normal/visual/insert states)
- `SPC SPC`: execute-extended-command (M-x)
- `SPC e`: Emacs config shortcuts
- `SPC t`: toggles
- `SPC c`: comment
- `SPC s`: search (buffer, project)
- `SPC l`: language (LSP, format)
- `SPC a`: applications (terminal, worktrees)
- `SPC g`: git (Magit)
- `SPC w`: window management
- `SPC b`: buffer operations

**Add/modify bindings:**
Edit `vibemacs-leader` block in `init.el`:

```elisp
(vibemacs-leader
  "xy" '(my-command :which-key "description"))
```

**Avoid conflicting with Evil/Emacs defaults:**
- Don't rebind `C-g`, `C-u`, `C-w`, `M-x` directly
- Use `general.el` definer for consistency

### Performance Considerations

**Lazy loading:**
- Use `:commands`, `:hook`, `:after` in `use-package` blocks to defer loading
- Avoid `:config` for side effects if possible (use `:init` for settings)
- Example:

```elisp
(use-package magit
  :commands (magit-status magit-dispatch)  ; load only when called
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window))
```

**Defer strategies:**
- LSP (Eglot): `:hook` ensures load only when entering TS/JS modes
- Completion (Corfu): `:init` sets variables, `:config` enables mode (deferred until Corfu loads)
- Terminal (vterm): `:commands` loads only when opening terminal

**Startup time:**
- Current config is optimized for fast startup (worktrees module defers terminal creation)
- Measure with `emacs --debug-init --timed-requires`

## Troubleshooting

**LSP server not starting:**
- Check `typescript-language-server` installed: `npm list -g typescript-language-server`
- Verify Eglot log: `M-x eglot-events-buffer`
- Restart Eglot: `M-x eglot-reconnect`

**Tree-sitter grammar missing:**
- Error: "No language grammar available for X"
- Fix: `M-x treesit-install-language-grammar`, select language (typescript, tsx, javascript, markdown)
- Or answer "yes" when prompted by treesit-auto

**vterm/multi-vterm build issues:**
- vterm requires compilation (libvterm + CMake)
- On Ubuntu: `apt-get install cmake libtool-bin libvterm-dev`
- On macOS: `brew install cmake libvterm`
- Rebuild: `M-x vterm-module-compile`

**Codex command not found:**
- Ensure `codex` CLI in PATH: `which codex`
- Set `vibemacs-worktrees-codex-executable` to full path if needed:

```elisp
(setq vibemacs-worktrees-codex-executable "/usr/local/bin/codex")
```

**Apheleia formatting not working:**
- Check formatter in PATH: `which prettier eslint_d`
- Verify Apheleia is active: `M-x apheleia-mode` (should show "Apheleia" in modeline)
- Check `*Apheleia*` log buffer for errors

**Worktrees dashboard not appearing:**
- Verify `vibemacs-worktrees-startup-layout` is non-nil
- Check `etc/worktrees.el` loaded: `M-x describe-function RET vibemacs-worktrees-dispatch`
- Manually open: `SPC a w` then select "Dashboard"

**Performance issues (slow startup/editing):**
- Disable Eglot inlay hints: remove `(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)` from init.el
- Reduce Corfu auto-completion delay: `(setq corfu-auto-delay 0.5)`
- Profile startup: `M-x profiler-start RET cpu RET`, restart Emacs, `M-x profiler-report`
