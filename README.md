# vibemacs

Emacs config for running multiple Codex, Claude Code and Gemini agents in parallel.

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
