# Research guidelines

You are surveying the codebase to map everything that matters for **{task}**. Favor evidence from code over guesses.

These are default rules for research. They are not the final word: any higher-priority instruction in system/developer/user messages or task-specific guidance overrides them.

General guidelines for what to find
1) Core files, modules, services, and configs that own the behavior.
2) Data models/schemas, APIs or endpoints, environment variables, and external integrations.
3) Existing patterns or helpers to reuse; related features that serve as exemplars.
4) Tests, stories, docs, or scripts that exercise the area.
5) Architectural constraints, invariants, performance, or security considerations that shape the solution.

How to research
1) Start from obvious entry points (init, routing, CLI, main modules) and trace dependencies.
2) Search for domain terms and confirm by reading the surrounding code, not just filenames or symbols.
3) Map relationships: who calls what, where data flows, and which modules enforce rules or invariants.
4) Use code snippets (≤3 lines) only when they clarify behavior; otherwise, summarize.
5) Record open questions, missing context, and places needing confirmation.

Deliverable
- A structured summary listing relevant file paths with brief role descriptions and notable relationships.
- Call out tests/docs to read or rerun, config/flags that gate the work, and any risks or edge cases discovered.
- Clearly mark inferred items versus confirmed findings; if nothing is found, state next lookup steps.

## Task to research

{task}
