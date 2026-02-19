# DiskWise — Behavioral Specification

## Overview

DiskWise is a disk cleanup tool that learns recurring cleanup patterns specific to the user's system. It uses an AI assistant for discovery and a local rule engine for execution, so that known patterns are handled without AI calls over time.

---

## Core Concepts

**Rule**: A persistent, user-approved instruction consisting of a condition ("when this is true about my filesystem") and an action ("do this about it"). Rules are the tool's memory.

**Investigation**: A one-time AI-assisted analysis of disk usage. Investigations may produce proposed rules as a side effect.

**Scan**: A local-only evaluation of all existing rules against the current filesystem. No AI is involved.

---

## Behaviors

### 1. Scanning (local only)

When the user requests a scan:

- The system evaluates every approved rule's condition against the filesystem.
- For each rule that matches, the system shows the user what was found and asks whether to execute the action.
- The user may accept or skip each match individually.
- If the user accepts, the action runs and the rule's hit count increments.
- If no rules exist or none match, the system suggests running an investigation.

### 2. Investigating (AI-assisted)

When the user requests an investigation:

- The system first runs a scan (as above) so known patterns are handled locally.
- The system gathers disk usage data: overall usage, large files, old files, caches, temp directories, package manager artifacts, and tool-specific directories.
- Only findings **not already covered** by existing rules are sent to the AI.
- The AI receives the full list of existing rules so it does not re-propose them.
- The AI returns two things: a human-readable analysis and zero or more proposed rules.
- The analysis is displayed to the user immediately.
- Each proposed rule is presented individually for approval (see below).

### 3. Rule Approval

When the AI proposes a rule:

- The system displays the rule's name, description, condition, action, and the AI's reasoning for why it would be useful.
- The user may **approve**, **reject**, or **skip remaining** proposals.
- Approved rules are persisted immediately and will fire on future scans.
- Rejected rules are discarded with no record.
- The AI cannot add or execute rules without explicit user approval.

### 4. Rule Management

At any time, the user may:

- **List** all approved rules, including their descriptions, conditions, actions, and how many times they have fired.
- **Delete** a rule by its identifier, removing it permanently.

### 5. Automated Mode

The system supports a non-interactive mode that:

- Evaluates all existing rules and reports matches to stdout.
- Does not prompt for input or call the AI.
- Is suitable for scheduled execution (e.g., cron).
- Exits after reporting.

---

## Rule Conditions

A rule's condition describes when it should fire. Supported conditions:

|Condition|Fires when...|
|---|---|
|Path exists|A specific path is present on the filesystem|
|Glob match|Files under a directory match a pattern|
|Older than N days|Contents of a path haven't been modified in N days|
|Larger than N MB|A path exceeds a size threshold|
|All of (conjunction)|Every sub-condition holds|
|Any of (disjunction)|At least one sub-condition holds|

## Rule Actions

A rule's action describes what to do when the condition fires:

|Action|Behavior|
|---|---|
|Suggest delete|Prompt the user to remove a path, showing a reason|
|Run command|Execute a shell command, showing a description|
|Suggest archive|Prompt the user to move a path elsewhere|
|Report size|Display the size of a path with a note (no mutation)|

All destructive actions require user confirmation at execution time, even for approved rules.

---

## Invariants

1. **No rule is ever created without user approval.**
2. **No destructive action is ever executed without user confirmation.**
3. **The AI only sees data not already covered by existing rules.**
4. **Scans never require network access or an API key.**
5. **Rules are portable** — the rule store is a single file that can be copied between machines.
6. **Hit counts are monotonically increasing** — they record how many times a rule has been useful.