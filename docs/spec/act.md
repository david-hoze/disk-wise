# Act

When the user decides to clean something up:

- The agent shows exactly what will happen before doing it: description, command, risk level, size estimate, and wiki reference.
- **Dry-run validation**: Before presenting each action, the agent checks that referenced paths exist and command binaries are available. Warnings are shown inline.
- The user confirms with `[y/n]`.
- If the user declines, they are asked why: `[r]` too risky, `[l]` later, `[d]` already done, `[n]` doesn't apply, or freeform text. This skip reason is recorded in the session log.
- If the user confirms:
  - The agent measures available disk space before execution.
  - The agent executes the cleanup command.
  - The agent measures available disk space after execution and computes actual bytes freed.
  - If the action referenced a wiki page, the outcome (success/failure) is recorded on that page's metadata for verify/fail tracking.
- **Nothing is ever deleted without the user saying yes.**

## Outcome Tracking

Each executed action produces a `CleanupOutcome` that records:

- Whether the action succeeded or failed
- The output or error message
- Actual bytes freed (measured via `df` before/after)
- The original size estimate from the wiki (for comparison)
- The action's position in the proposed list (0-indexed)
- The actual execution order among accepted actions (0-indexed)

The position and order data lets the engine learn about user trust patterns — do users execute actions in order, or do they cherry-pick? Do they tend to accept low-risk actions first?
