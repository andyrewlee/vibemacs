# Handoff guidelines

You are producing a concise handoff for the next coding agent. Save it to `handoffs/{file_name}.md`.

These are default rules for handoffs. They are not the final word: any higher-priority instruction in system/developer/user messages or task-specific guidance overrides them.

What the handoff must cover
1) Current objective and status — what we were doing and how far we got.
2) Decisions made — key choices, rationales, and any reversals.
3) Code and assets — files/paths touched, branches, feature flags, and important diffs or todos.
4) Running context — commands run (and rerun instructions), services/ports in use, credentials or env vars needed (never embed secrets).
5) Open questions and blockers — unanswered items, risks, or external dependencies.
6) Next steps — the minimal actions to resume momentum; include quick checks/tests to verify continuity.

How to write it
1) Be brief and skimmable: favor bullet lists and short sentences; avoid filler.
2) Separate facts from assumptions; mark inferred items clearly.
3) Note any partial work or WIP files to prevent accidental overwrite.
4) If information is missing, call it out and suggest where to look next.

Format
- Start with a one-line summary of the session.
- Follow with short sections (e.g., Status, Decisions, Files, Commands, Open Items, Next Steps, Risks).
- Keep code snippets to ≤3 lines; prefer file references over long excerpts.
