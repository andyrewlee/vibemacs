# Create Phased Plan Prompt

Using any research already completed (if any), create a Markdown file at `plans/{file_name}.md` containing a phased, sectioned checklist.

Each Phase must be ordered in a logical sequence from first to last.

For each Phase, include:

- **Objective** — a concise explanation of what the phase accomplishes.
- **Additional Context** — helpful notes, background information, examples, or code samples.
- **Checklist Items** — detailed, actionable steps.
  - Each item must start with `- [ ]` to allow progress tracking.
- **User Stories (Gherkin Format)** — acceptance criteria testable at the end of the phase.
  - All stories in a phase should pass when that phase's checklist is complete.

If no research was done, infer likely areas of the codebase and make reasonable assumptions.

## Task to plan for

{task}
