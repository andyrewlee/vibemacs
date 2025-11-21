## Phase 1 — Implement `vibemacs-worktrees--run-script`

**Objective**  
Add the missing run-script engine so setup/run/archive commands work, manage processes, and refresh UI state.

**Additional Context**  
Use metadata `scripts` entries, build env with `vibemacs-worktrees--script-env`, honor `script-mode` concurrency, store processes in `vibemacs-worktrees--processes`, attach sentinels to refresh dashboard/git-status, and name output buffers per worktree (e.g., `*worktree-<name>-<script>*`).

**Checklist Items**  
- [ ] Locate or create `vibemacs-worktrees--run-script` in `etc/worktrees-process.el`.  
- [ ] Load metadata via `vibemacs-worktrees--load-metadata`; resolve script text for the requested key (`setup`/`run`/`archive`).  
- [ ] Build environment list with `vibemacs-worktrees--script-env` and bind `process-environment` before launch.  
- [ ] Invoke `vibemacs-worktrees--stop-existing-process` when `script-mode` is nonconcurrent.  
- [ ] Start process with working directory at worktree root; route output to a dedicated buffer per worktree+script.  
- [ ] Store process in `vibemacs-worktrees--processes` and tag properties `vibemacs-kind`, `vibemacs-root`.  
- [ ] Add process sentinel to clear hash entry and trigger dashboard/git-status refresh on exit.  
- [ ] Handle empty/missing script strings with a clear user-facing error.  

**User Stories (Gherkin Format)**  
- Given a worktree with a `run` script in metadata, when I invoke `SPC a r`, then the command launches in the worktree root and its output appears in a named buffer.  
- Given `script-mode` is `nonconcurrent` and a run is already active, when I start another run, then the previous process is stopped before the new one starts.  
- Given a run script finishes, when it exits successfully or fails, then the dashboard/git status sidebar updates within the current session.  
- Given a worktree without a configured script, when I try to run it, then I see a descriptive error instead of a crash.  

## Phase 2 — Fix upstream/base parsing & add unit tests

**Objective**  
Ensure base refs are parsed correctly and covered by automated tests.

**Additional Context**  
The git call in `vibemacs-worktrees--entry-from-record` currently passes a malformed `@{upstream}` argument; correct it and cover with ERT using a stub for `vibemacs-worktrees--call-git`.

**Checklist Items**  
- [ ] Fix the `@{upstream}` invocation in `etc/worktrees-git.el` to remove embedded whitespace/newline.  
- [ ] Add `etc/worktrees-git-test.el` with stubbed `vibemacs-worktrees--call-git` to assert `:base` is populated from upstream.  
- [ ] Add tests for main-entry promotion and default target directory naming/hashing.  
- [ ] Wire tests to run via `emacs -Q --batch -L etc -l ert -l etc/worktrees-git-test.el -f ert-run-tests-batch-and-exit`.  

**User Stories (Gherkin Format)**  
- Given a worktree record with `branch` and upstream, when `vibemacs-worktrees--entry-from-record` runs, then the resulting entry sets `base` to the upstream ref.  
- Given multiple worktrees including the primary checkout, when entries are promoted, then the repo-root entry appears first.  
- Given a repo path and worktree name, when computing the default target directory, then it nests under `~/.vibemacs/worktrees/<repo>/<name>/`.  

## Phase 3 — Refactor layout builder into explicit helpers

**Objective**  
Make layout creation deterministic and readable, with correct window role assignment.

**Additional Context**  
Introduce pure helpers (`vibemacs-worktrees--desired-widths frame-width` and `vibemacs-worktrees--apply-layout entry widths`) and ensure `vibemacs-worktrees--center-window` always references the chat/diff pane. Preserve existing three/two/single column behaviors.

**Checklist Items**  
- [ ] Extract width computation into a pure helper that returns left/center/right sizing decisions.  
- [ ] Extract window construction into an apply helper that returns a struct/alist of window roles.  
- [ ] Set `vibemacs-worktrees--center-window` to the chat/diff pane; avoid reusing variable names for different roles.  
- [ ] Reuse helpers in both startup and workspace layout code paths.  
- [ ] Manually verify layout on wide and narrow frames to confirm expected column fallbacks.  

**User Stories (Gherkin Format)**  
- Given a wide frame, when I launch vibemacs, then the left pane shows the dashboard, the center shows chat, and the right shows git status/terminal with correct widths.  
- Given a medium frame, when I launch vibemacs, then a two-column layout appears with dashboard on the left and chat on the right.  
- Given a narrow frame, when I launch vibemacs, then I see only the dashboard without errors.  

## Phase 4 — Load-path robustness with `EMACSLOADPATH`

**Objective**  
Keep built-in libraries available when `EMACSLOADPATH` is set for vibemacs.

**Additional Context**  
Expand `load-path` by prepending `EMACSLOADPATH` entries instead of replacing defaults; document the recommended `EMACSLOADPATH=$PWD:` pattern in README.

**Checklist Items**  
- [ ] Modify `init.el` to append split `EMACSLOADPATH` to `load-path` before requiring `jka-compr`.  
- [ ] Update README “Setup/byte-compile” instructions to include the trailing colon variant.  
- [ ] Re-run batch byte-compile with `EMACSLOADPATH=$PWD:` to confirm `jka-compr` loads.  

**User Stories (Gherkin Format)**  
- Given `EMACSLOADPATH` is set to the repo path, when I batch-byte-compile, then builtin libraries load and compilation completes.  
- Given a user follows README instructions with the trailing colon, when they launch vibemacs, then startup succeeds without missing builtins.  

## Phase 5 — Async or cached dashboard dirtiness checks

**Objective**  
Prevent dashboard freezes by avoiding synchronous `git status` per refresh.

**Additional Context**  
Queue `git status --short` via `make-process` per project or cache results with a short TTL; update rows when results arrive and fall back to last-known counts.

**Checklist Items**  
- [ ] Introduce a cache structure keyed by worktree root storing dirty counts and timestamps.  
- [ ] Replace synchronous `vibemacs-worktrees-dashboard--git-summary` calls with async jobs or cache lookups.  
- [ ] Update dashboard refresh to redraw rows when async results return.  
- [ ] Handle failures gracefully (show “Error” but keep UI responsive).  

**User Stories (Gherkin Format)**  
- Given many worktrees, when I open the dashboard, then it renders immediately and populates dirty counts progressively without blocking.  
- Given a git status call fails, when the dashboard refreshes, then the row shows an error message but the UI stays interactive.  

## Phase 6 — Add fast ERT coverage for git and layout

**Objective**  
Provide regression safety for git parsing and layout role assignment.

**Additional Context**  
House tests under `etc/` to mirror modules; use stubs for git calls and window sizing to keep tests fast and batchable.

**Checklist Items**  
- [ ] Implement `etc/worktrees-layout-test.el` to simulate wide/medium/narrow frame widths and assert window role assignment helpers.  
- [ ] Ensure tests don’t require GUI; mock width computations rather than creating frames.  
- [ ] Add test targets to developer docs (`README` or `AGENTS.md` notes).  
- [ ] Add a convenience script/command snippet for running both git and layout tests together.  

**User Stories (Gherkin Format)**  
- Given the layout helpers run in batch mode, when widths are wide, medium, and narrow, then the returned role map matches expected column counts.  
- Given the git parsing tests run, when upstream refs are present, missing, or malformed, then results match expectations without side effects on real git repos.  

## Phase 7 — Tame setup logging & harden vterm helpers

**Objective**  
Reduce noise during setup and guard vterm interactions against dead processes.

**Additional Context**  
Introduce a verbosity flag/variable for setup logging and guard `vibemacs-worktrees--send-multiline-to-vterm` with process checks.

**Checklist Items**  
- [ ] Add a defcustom (or internal var) to toggle verbose setup logging; default to quiet.  
- [ ] Wrap noisy `message` calls in setup command runner with the verbosity gate.  
- [ ] Check for a live vterm process before sending bracketed paste; emit a friendly warning otherwise.  

**User Stories (Gherkin Format)**  
- Given default settings, when setup commands run, then only high-level progress is logged to the echo area.  
- Given a chat/terminal buffer is closed, when a helper tries to send text, then it reports the buffer is inactive instead of throwing an error.  
