# Remaining Multi-Project Tasks

## Landed in `multiple-projects`
- Registry now records repositories instead of individual worktrees (`projects.json`) and migrates legacy `worktrees.json` entries on first load (`etc/worktrees-core.el`, `etc/worktrees-registry.el`).
- Dashboard groups worktrees by project, adds a `Home` row that restores the welcome screen, and scopes `+ Create` to the highlighted repo (`etc/worktrees-dashboard.el`).
- Startup layout shows dashboard + welcome buffer, while `vibemacs-worktrees--activate-workspace-layout` recreates the classic three-pane editor when a worktree is activated (`etc/worktrees-layout.el`).

## Follow-ups
1. **Home layout regression tests** – Manually confirm that pressing `Home` after entering a worktree restores the logo welcome buffer plus slim dashboard sidebar on both wide and narrow frames.
2. **Registry migration QA** – Run against a machine with an existing `worktrees.json`, ensure `projects.json` is created, duplicates are de-duped, and dirty-filter actions still work post-migration.
3. **Workspace layout monitoring** – Exercise tab switching, git-status refresh, and terminal panes after repeated project switches to ensure the resurrected three-pane code path doesn't leak buffers or lose the right sidebar split.
