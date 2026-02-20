# Session Learning

DiskWise learns from every session and carries that knowledge forward. This section describes the cross-session intelligence mechanisms.

## Session Summary

At the end of each session, a summary is persisted to `~/.diskwise/session-history.jsonl`. Each summary captures:

| Field | Description |
|---|---|
| `timestamp` | When the session occurred |
| `platform` | OS, architecture, and shell |
| `findingCount` | Number of findings from the scan |
| `actionsRun` | Number of cleanup actions the user executed |
| `actionsFailed` | Number that failed |
| `skipReasons` | List of (action description, reason) for skipped actions |
| `bytesFreed` | Total measured space freed |
| `userFeedback` | Post-cleanup feedback if something broke |
| `failedCmds` | List of (command, error message) for failed actions |
| `cleanedPaths` | List of (path, bytes freed) for regrowth tracking |
| `succeededCmds` | List of commands that succeeded (for reliability stats) |

## Skip Reason Tracking

When a user declines a cleanup action, they are asked why:

| Key | Reason |
|---|---|
| `r` | Too risky |
| `l` | Later (not now) |
| `d` | Already handled |
| `n` | Not applicable |
| (text) | Custom freeform reason |

Skip reasons are aggregated across sessions into **skip patterns**. If an action is skipped more than half the time:

- **Too risky**: The wiki page should raise the documented risk level or add caveats.
- **Not applicable**: The matching may be wrong, or the page needs platform notes.
- **Already handled / Not now**: No wiki change needed.

Skip patterns are included in the analysis prompt so the engine can adjust its recommendations.

## Space Measurement

Every executed cleanup action is measured:

1. `df` is called before execution to record available disk space.
2. The action runs.
3. `df` is called after execution to compute actual bytes freed.

The measured result is compared to the wiki's size estimate. If they differ significantly, the engine is prompted to amend the wiki page with real-world observations.

## Regrowth Detection

At the start of each session, the agent checks whether paths cleaned in the previous session have regrown:

1. Load the most recent session summary.
2. For each `(path, bytesFreed)` in `cleanedPaths`, measure the current size of that path.
3. If the current size exceeds half the previously freed amount, report it as regrowth.

Regrowth data is included in the analysis prompt with guidance to consider whether recurring cleanup should be automated.

## Delayed Feedback

After loading the previous session summary, if any actions were run, the agent asks:

> Did anything break after last session's cleanup? [n] No [y] Yes

If yes, the user provides a brief description, which is recorded as a `UserFeedback` event with a "delayed issue from previous session" prefix. This is the highest-value signal — it means a wiki recommendation caused harm.

## Post-Cleanup Feedback

At the end of the current session, if any actions were executed, the agent asks:

> Did anything break or behave unexpectedly after cleanup? [n] Everything's fine [y] Something went wrong

Feedback is recorded and included in the learn prompt. The engine is instructed to treat user feedback as the highest-priority signal and to amend the relevant wiki page's "What's NOT safe to delete" section.

## Action Acceptance Order

Each cleanup outcome records two positions:

- **Position**: Where the action appeared in the engine's proposed list (0-indexed).
- **Order**: The actual execution order among accepted actions (0-indexed).

This data lets the engine learn about user trust patterns over time — whether users execute in order or cherry-pick, and whether they tend to accept low-risk actions first.

## Command Reliability

The agent aggregates per-command success/failure statistics across all sessions. Only commands with 2+ total occurrences are included. For each command, the stats show:

- Number of successes and failures
- Success rate as a percentage
- Most recent error message (if any)

These statistics are included in both the investigation prompt and the learn prompt. The engine is guided to:

- Add platform-specific notes to wiki pages for unreliable commands
- Suggest alternative commands
- Raise risk levels for commands that fail frequently

## Platform Correlation

Each session records the platform (OS, architecture, shell). The learn prompt includes this information so the engine can:

- Document platform-specific failures in wiki pages' `## Platform notes` sections
- Use the format: "On [OS] ([arch]): [observation]"
- Correlate which commands fail on which platforms across sessions

## Contribution Approval Tracking

Each wiki contribution records how the user decided:

| Decision | Meaning |
|---|---|
| `ContribApproved` | User approved without edits |
| `ContribSkipped` | User declined the contribution |
| `ContribEdited` | User requested edits before pushing |

This informs the engine about what kinds of contributions users find valuable vs. unhelpful.
