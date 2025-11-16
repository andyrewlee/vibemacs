# vibemacs

emacs config for running multiple codex, claude code and gemini agents in parallel.

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
