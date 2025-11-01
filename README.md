# vibemacs

emacs for vibe coders

## Worktree setup commands

When a new worktree is created, `vibemacs` looks for a JSON file at
`.vibemacs/worktrees.json` relative to the repository root. If it finds a
`setup-worktree` array, each string in the array will be executed (via the
shell) in the fresh worktree immediately after checkout.

Example configuration:

```
{
  "setup-worktree": [
    "pnpm install",
    "cp $ROOT_REPO_PATH/.env .env"
  ]
}
```

Commands run in this phase have access to several helpful environment
variables:

- `ROOT_REPO_PATH` / `ROOT_WORKTREE_PATH` — absolute path to the root checkout
  where `.vibemacs/worktrees.json` lives (the "main" worktree).
- `NEW_WORKTREE_PATH` / `WORKTREE_PATH` — the absolute path of the newly created
  worktree.
- `WORKTREE_NAME` — the requested worktree name.
- `WORKTREE_BRANCH` — the branch created for the worktree.
- `WORKTREE_BASE` — the base ref used when creating the worktree.

These environment variables allow setup commands to copy files or initialise
state using either the original repository checkout or the new worktree.
