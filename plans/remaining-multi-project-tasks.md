# Remaining Multi-Project Tasks

1. **Migrate the registry automatically** – `vibemacs-worktrees-registry` now points to `projects.json`, but nothing copies existing entries out of `worktrees.json`. Launching `emacs .` inside a repo no longer shows the repo’s worktrees unless the user manually re-adds it via `vibemacs-project-add`. Design and implement a migration path (or auto-register the repo you started from) so existing setups keep working.

2. **Hook the dirty-only filter back up** – the dashboard still tracks `vibemacs-worktrees-dashboard--filter`, yet `vibemacs-worktrees-dashboard--entries` ignores it. Toggling `f` always renders all worktrees; reinstate the filtering logic so dirty-only view works again.

3. **Keep the active worktree in view when switching** – `vibemacs-worktrees-dashboard--activate` rebuilds the table but no longer calls `tabulated-list-goto-id`, so the point jumps to the top whenever you activate a worktree near the bottom. Restore cursor preservation to avoid disorienting scrolling.

Feel free to extend this list as we uncover more polish items for the multi-project flow.
