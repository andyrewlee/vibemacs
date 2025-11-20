# vibemacs

Emacs config for managing git worktrees with chat-first AI assistants (Claude, Gemini, or any CLI you map in `vibemacs-worktrees-chat-assistants`). Codex-specific plan/apply helpers have been removed; chat buffers now just launch the configured commands.

## Setup

For custom setup scripts when creating worktrees, add a `worktrees.json` file:

```json
{
  "setup-worktree": [
    "pnpm i",
    "cp $ROOT_WORKTREE_PATH/.env.local .env.local",
    "cp -r $ROOT_WORKTREE_PATH/.clerk ."
  ]
}
```

## Directory Structure

Worktrees are created under `~/.vibemacs/worktrees/<repo-name>/`:

- **Worktrees**: `~/.vibemacs/worktrees/<repo-name>/<worktree-name>/`
- **Metadata**: `~/.vibemacs/worktrees-metadata/<hash>/worktree.json`
- **Registry**: `~/.vibemacs/projects.json`
- **Per-project scripts**: `.vibemacs/worktrees.json` inside a repo defines setup/run/archive commands
