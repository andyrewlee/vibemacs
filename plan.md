# Layout Update Plan

## Repository Reconnaissance
- [x] Survey existing layout components to understand current pane structure
- [x] Locate data sources for file tree, changed files list, and terminal integration

## Pane Structure Adjustments
- [x] Introduce a right pane that mirrors left pane width within the overall layout grid
- [x] Split left pane into top (existing workspace/work tree selector) and bottom (project file tree)
- [x] Split new right pane into top (changed files list) and bottom (terminal view)

## Interaction Behaviors
- [x] Ensure selecting "Work Tree" in top-left restores Codex CLI main view
- [x] Hook file tree selections to open the chosen file in main pane
- [x] Hook changed-files selections to open the chosen file in main pane
- [x] Confirm terminal remains interactive within bottom-right pane

## Verification & Cleanup
- [ ] Validate pane sizing and responsiveness match requirements
- [ ] Smoke-test navigation between work tree, file tree, and changed files
- [ ] Remove temporary logs/debug output before final review
