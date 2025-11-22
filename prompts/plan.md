# Planning guidelines

You are crafting a phased execution plan for **{task}**. Save it to `plans/{file_name}.md`.

These are the default rules for planning. They are not the final word: any higher-priority instruction in system/developer/user messages or task-specific guidance overrides them.

General principles
1) Outcome-first: every phase must yield a user-visible result, a risk reduction, or a validated learning.
2) Ordered by dependency: unblock downstream work early; defer polish and optional extras to later phases.
3) Lean and clear: prefer 3–6 well-scoped phases; write in direct, specific language without hedging.
4) Evidence over assumption: if research is missing, state assumptions explicitly and schedule their confirmation early.

Required structure for each phase
- **Objective** — one sentence stating the measurable outcome.
- **Assumptions & Context** — dependencies, risks, prior decisions, or inferred knowledge; flag anything assumed.
- **Checklist** — actionable steps starting with `- [ ]`; make them concrete (who/what/where) and include validation (tests, docs, monitoring) when relevant.
- **User Stories (Gherkin)** — concise Given/When/Then criteria that all pass when the checklist is complete, phrased as observable behavior.

Cross-phase guidance
- Add a discovery/spike phase only when meaningful unknowns exist; define its exit criteria.
- Separate migrations/infra from feature delivery so tracking, rollbacks, and ownership stay clear.
- Capture handoffs, approvals, rollback/mitigation paths, and data/backfill steps when applicable.
- Avoid repeating the phase objective inside checklist items; include only the steps needed to satisfy it.

## Task to plan for

{task}
