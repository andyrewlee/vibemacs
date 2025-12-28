# Repository Guidelines

## Project Structure & Module Organization
`init.el` is the entrypoint; it bootstraps `use-package`, disables default UI chrome, and wires in the worktree launcher from `etc/worktrees.el`. Feature-specific modules live in `etc/worktrees-*.el` (e.g., `worktrees-dashboard.el` renders the UI while `worktrees-chat.el` handles agent chat buffers); keep new modules in this directory and require them explicitly from `init.el`. Per-worktree metadata is stored under `worktrees-metadata/`, the project registry lives in `projects.json`, and repo-level automation reads `.vibemacs/worktrees.json` when present. Treat these as data stores rather than hand-editing them. Cached artifacts (`elpa/`, `tree-sitter/`, `var/`, `eln-cache/`, `auto-save-list/`) should rarely change in a PR unless you intentionally update packages or parsers.

## Build, Test, and Development Commands
- `EMACSLOADPATH=$PWD emacs -Q --load init.el` launches vibemacs with only this repo on the load path; use it for manual verification.
- `EMACSLOADPATH=$PWD emacs -Q --batch -l init.el -f batch-byte-compile etc/worktrees-*.el` ensures every module byte-compiles cleanly.
- `make ert-git`, `make ert-layout`, and `make ert-workflows` run the targeted ERT suites; `make test` runs all of them plus byte-compilation.
- `EMACSLOADPATH=$PWD emacs -Q --batch -l init.el -l etc/worktrees-workflows-test.el -f ert-run-tests-batch-and-exit` runs the high-level workflow stories directly.
- `npm install -g typescript typescript-language-server prettier eslint_d` matches the TypeScript tooling assumed in `init.el`; re-run when upgrading language support.

## Coding Style & Naming Conventions
Use two-space indentation and spaces over tabs (`indent-tabs-mode` is disabled globally). Name public symbols with the `vibemacs-` prefix and keep module-specific helpers in the corresponding `worktrees-*.el`. Configure features through `use-package` blocks, declaring `:init` intent separate from `:config`. For TypeScript buffers, rely on eglot + corfu + apheleia; keep formatter settings in `init.el` and avoid per-file overrides unless absolutely necessary.

## Testing Guidelines
Automated ERT suites live alongside their modules (e.g., `etc/worktrees-dashboard-test.el`, `etc/worktrees-git-status-test.el`) plus an end-to-end workflow suite (`etc/worktrees-workflows-test.el`). Run them via `make ert-git`, `make ert-layout`, `make ert-workflows`, or `make test`. UI-heavy changes should also be smoke-tested interactively by launching the dashboard (`SPC aw`) and exercising the chat/diff/terminal panes.

## Commit & Pull Request Guidelines
Recent history (e.g., `Add Gemini as third coding agent`) shows short, imperative subjects; follow that style and include agent context when relevant. Keep commits focused on one concern, and describe any user-visible change in the body. Pull requests should document the motivation, summarize manual test steps, attach screenshots/gifs for dashboard tweaks, and call out any migrations (package refresh, tree-sitter library updates). Avoid committing personal `.vibemacs/worktrees.json` edits unless they are intentional fixtures.

## Security & Configuration Tips
Do not hard-code tokens or user paths inside modules; load secrets from the env or external files ignored by git. Before pushing, inspect `projects.json`, `.vibemacs/worktrees.json` (if present), and `worktrees-metadata/` for sensitive paths and remove unintended entries. When upgrading tree-sitter dylibs, verify their provenance and document the source commit in the PR.
