# DiskWise — Behavioral Specification (v3)

## Overview

DiskWise is a disk cleanup assistant backed by a shared wiki. An AI agent scans the user's system, consults a community-maintained knowledge base for context, advises the user, and writes back anything new it learned. A separate **gardener** command reviews and improves wiki quality using the strongest available model, and maintains a meta wiki of its own observations about the knowledge base.

---

## Core Concepts

**Wiki**: A GitHub repository of markdown files organized by topic. It contains human-readable knowledge about what lives where on a filesystem, what's safe to delete, what isn't, and how to clean things up. The wiki is the tool's shared memory.

**Meta Wiki**: A section of the wiki (`_meta/`) maintained exclusively by the gardener. It contains observations about the wiki itself — organizational patterns, quality trends, recurring issues, gardening decisions and rationale. The meta wiki is the gardener's own memory across sessions.

**Agent**: A single running instance of DiskWise on a user's machine. Every agent has equal read/write access to the wiki. There is no distinction between contributors and consumers.

**Gardener**: A dedicated command (`diskwise garden`) that reviews and improves wiki quality. The gardener always runs with Claude Opus 4.6 at high effort, because wiki quality improvement requires deep reasoning about organization, duplication, clarity, and cross-page coherence. The gardener also reads and writes the meta wiki, building up its own understanding of the knowledge base over time. Gardening is never triggered automatically by the normal investigate flow — it is a separate, intentional operation.

**Investigation**: The agent scans the local filesystem, pulls relevant wiki pages, sends both to Claude, presents advice to the user, and pushes any new knowledge back to the wiki.

---

## Behaviors

### 1. Investigate

When the user runs the tool:

- The agent scans the local filesystem and gathers raw disk usage data. The scanner is generic — it runs `df`, `du`, and `find` to discover what's large without any tool-specific knowledge baked in. All interpretation is left to Claude and the wiki.
- The agent pulls the latest wiki from GitHub.
- The agent matches scan findings to wiki topics by path, tool name, and platform.
- The agent sends the scan data and matched wiki pages to Claude as context.
- Claude produces advice grounded in community knowledge.
- The user sees the advice and decides what to act on.

### 2. Learn

After an investigation, Claude reviews the entire session — the scan, its analysis, the user's actions, any errors encountered, any surprises — and drafts wiki contributions (new pages or amendments to existing ones).

The bar for contributing is low. If it might save a future agent five seconds of reasoning or prevent it from making a wrong suggestion, write it down.

The normal CLI **contributes only**. It does not refactor or reorganize the wiki. That responsibility belongs to the gardener.

Examples of things worth writing back:

- A tool or cache directory the wiki didn't know about yet.
- A safer cleanup method than what was previously documented.
- A caveat discovered the hard way (e.g., "don't delete X if Y is installed").
- Platform-specific paths (e.g., Homebrew on Apple Silicon vs Intel).
- An error encountered during cleanup and how it was resolved.
- A workaround for a permission issue on a specific OS.
- A better or faster way to scan a certain directory structure.
- A command that looked right but silently failed, and what to use instead.
- Unexpected interactions between tools (e.g., "removing Conda breaks pyenv shims").
- Observations about typical sizes, growth rates, or patterns on real systems.
- Anything the agent had to figure out that wasn't already in the wiki.

Examples of things NOT written back:

- User-specific paths or usernames.
- Anything containing secrets, tokens, or personal data.

### 3. Act

When the user decides to clean something up:

- The agent shows exactly what will happen before doing it.
- The user confirms.
- The agent executes the cleanup.
- Nothing is ever deleted without the user saying yes.

### 4. Garden

The gardener is a separate command (`diskwise garden`) that improves wiki quality. It is never invoked as part of the normal investigate flow.

**Model**: The gardener always uses Claude Opus 4.6 (`claude-opus-4-6`) at high effort. Wiki gardening requires the strongest reasoning available — it involves judging quality, spotting subtle duplication, making organizational decisions, and writing clear prose. Weaker or faster models are not suitable.

**Flow**:

1. The gardener fetches the entire wiki, including the meta wiki (`_meta/`).
2. It partitions pages into content pages and meta pages. Meta pages are presented to Claude in a separate section so it understands they are its own notes, not content to refactor.
3. It enters a gardening loop (up to 5 passes): build a prompt with content pages + meta pages + agent identity, call Claude Opus 4.6, parse the result, push improvements with the `diskwise-gardener:` commit prefix.
4. Between passes, it re-fetches the full wiki tree so it can see its own changes (including any `_meta/` pages it wrote as contributions during the pass).
5. The loop stops when Claude returns `done=true`, produces no contributions, or the pass limit (5) is reached.
6. After the loop completes, the gardener writes a session summary to `_meta/gardening-log.md` recording what each pass accomplished. If the page doesn't exist yet (first garden run), it is created; otherwise it is amended.
7. Each improvement is a separate commit so the history stays granular.

**Types of improvements**:

- Merge duplicate or overlapping pages.
- Rewrite unclear explanations.
- Reorganize structure (move content to better locations).
- Split pages that got too long.
- Create new category pages to tie related topics together.
- Fix formatting inconsistencies.
- Add missing `## History` entries.

**What the gardener does NOT do**:

- Remove useful information.
- Add speculative content not based on existing pages.
- Change the meaning of existing advice.
- Reorganize, merge, or rewrite `_meta/` pages as if they were wiki content. The gardener's system prompt explicitly states: "_meta/ pages are your notes to your future self, not content to refactor."

#### Meta Wiki

The meta wiki lives under `_meta/` in the wiki repository. It is the gardener's own knowledge base about the knowledge base. The gardener reads it at the start of every session and writes to it after every pass.

The meta wiki is freeform, but typical content includes:

- **Gardening log**: What was improved, when, and why. A running record of organizational decisions.
- **Structural observations**: Patterns the gardener noticed (e.g., "the haskell/ section has grown large and may need subcategories", "troubleshooting pages tend to drift into tool-specific advice that belongs in tool pages").
- **Deferred work**: Things the gardener noticed but didn't fix this session, and why (e.g., "the docker/ section needs a unifying overview page, but I want to wait until more tool pages exist").
- **Quality trends**: Whether the wiki is getting better or worse in specific areas, and what's driving it.
- **Conventions**: Emerging patterns that should be standardized (e.g., "agents are inconsistent about whether cleanup commands use flags — should standardize on long flags for clarity").

Example meta wiki page:

```markdown
# Gardening Log

## 2026-02-20

Reviewed 23 pages. Main improvements:
- Merged `haskell/ghcup.md` and `haskell/hls.md` overlap on symlink paths
- Created `haskell/overview.md` to tie the section together
- Rewrote `troubleshooting/permission-errors.md` intro for clarity
- Noticed: several tool pages are missing ## Platform notes sections.
  Not fixing now — want to see if agents start filling these in naturally.

## 2026-02-22

Reviewed 28 pages (5 new since last session). Main improvements:
- Split `docker/images.md` which had grown to cover volumes too
- Standardized cleanup command formatting across python/ section
- Deferred: the observations/ section only has 2 pages and may not
  justify its own category yet. Will revisit when it reaches 5+.
```

The meta wiki is committed with messages like: `diskwise-gardener: update gardening log` `diskwise-gardener: note structural observations`

Normal agents (during investigate) should **not** write to `_meta/`. It is exclusively the gardener's space.

---

## Wiki Structure

```
wiki/
  index.md                  # auto-generated table of contents
  _meta/                    # gardener's meta wiki (gardener-only)
    gardening-log.md
    structural-observations.md
    deferred-work.md
    conventions.md
  haskell/
    ghcup.md
    cabal.md
    stack.md
    hls.md
  node/
    npm.md
    yarn.md
    pnpm.md
    node-modules.md
  python/
    pip.md
    conda.md
    venv.md
  rust/
    cargo.md
    rustup.md
  docker/
    images.md
    volumes.md
    buildx.md
  editors/
    vscode.md
    jetbrains.md
  os/
    macos/
      system-caches.md
      xcode.md
      time-machine.md
      scanning-tips.md
    linux/
      apt.md
      snap.md
      journal.md
    windows/
      temp.md
      winsxs.md
      troubleshooting-deletes.md
      permission-issues.md
  general/
    logs.md
    downloads.md
    trash.md
  troubleshooting/
    permission-errors.md
    commands-that-lie.md
    silent-failures.md
    symlink-pitfalls.md
  techniques/
    fast-scanning.md
    estimating-reclaimable-space.md
    finding-duplicates.md
    safe-dry-runs.md
  observations/
    typical-sizes.md
    things-that-grow-fast.md
    surprising-space-hogs.md
```

### Page Format

Pages about specific tools follow a consistent structure:

```markdown
# Tool Name

Brief description of what this tool is.

## Where it stores data

- `~/.some/path` — what this contains
- `~/.some/other/path` — what this contains

## What's safe to delete

Explanation, with commands if applicable.

## What's NOT safe to delete

Caveats and warnings.

## Cleanup commands

Preferred commands to free space safely.

## Typical space usage

How much space this usually takes up.

## Platform notes

Differences across macOS, Linux, Windows if any.

## History

- 2026-02-20: Initial page (agent@user-abc)
- 2026-02-21: Added Apple Silicon path variant (agent@user-def)
```

Pages under `troubleshooting/`, `techniques/`, and `observations/` are freeform. They don't need a fixed template. The only requirement is a `## History` section at the bottom so the provenance of each addition is traceable. Examples:

```markdown
# Commands That Lie

## `du` vs actual disk usage on macOS APFS

`du -sh` on APFS volumes can report misleading numbers because
of clones and snapshots. The space shown may not be reclaimable.
Use `diskutil apfs list` to see actual purgeable space.

## `docker system df` after prune

`docker system df` sometimes still shows large reclaimable space
immediately after `docker system prune`. This is a display bug
in older Docker versions. Restarting the daemon refreshes it.

## History

- 2026-02-20: APFS du issue (agent@user-abc)
- 2026-02-22: Docker display bug (agent@user-ghi)
```

Meta wiki pages (`_meta/`) have no required format. They are the gardener's notes to its future self.

---

## Wiki Access

The wiki lives in a public GitHub repository. The agent accesses it via the GitHub REST API. No git binary is required. The user configures nothing — the tool handles wiki access out of the box.

**Reading**: The repo is public. The agent reads files via unauthenticated GET requests to the GitHub Contents API. No token needed.

**Writing**: The agent authenticates via a classic GitHub personal access token (PAT) provided in the `DISKWISE_WIKI_TOKEN` environment variable, scoped to write access on the single wiki repo and nothing else. Every agent uses the same token. This is not a secret — the repo is public and every agent is equally trusted. If the token is not set, wiki reads still work but writes will fail with a clear error.

**Conflict handling**: The GitHub Contents API requires the file's current SHA to update it. If the SHA is stale (another agent wrote to the same file concurrently), the PUT will fail. The agent re-fetches the current SHA, re-applies its amendment to the latest content, and retries. It keeps retrying with a short backoff (1s, 2s, 4s, ...) up to 5 attempts. If all retries are exhausted, the contribution is saved to a local pending queue and retried at the start of the next session.

**Commit messages**: Each write includes a descriptive commit message. Normal agents use: `diskwise-agent: add conda environments page` `diskwise-agent: add caveat about ghcup symlinks`

The gardener uses a distinct prefix: `diskwise-gardener: merge overlapping docker pages` `diskwise-gardener: update gardening log` `diskwise-gardener: note structural observations`

## Claude Access

The agent supports two ways to use Claude:

**API key**: The user sets their Anthropic API key. The agent calls the Claude API directly. This is pay-per-use.

**Claude subscription**: The user has a Claude Pro or Team subscription. The agent invokes Claude Code (`claude` CLI) as a subprocess, which authenticates via the user's existing subscription. No API key needed. This is the zero-config option — if the user already has Claude Code installed and logged in, it just works.

The agent tries Claude Code first. If it's not installed or not authenticated, it falls back to the API key. If neither is available, it exits with a clear message explaining the two options.

**CLI invocation details**: The agent calls Claude Code via `sh -c` to ensure PATH resolution works on all platforms (including MINGW/Windows). The prompt is written to a temporary file in UTF-8 encoding to avoid OS command-line length limits and Unicode encoding issues on Windows. The system prompt and user prompt are combined into a single file, which is piped to `claude --print` via stdin redirect (`< /path/to/file`). The temporary file uses the Windows temp directory (obtained via `getTemporaryDirectory`) for writing, and is converted to a MINGW path (via `toMingwPath`) for the shell command. The `CLAUDECODE` environment variable is unset in the shell command to allow the tool to be tested from within a Claude Code session. After the CLI call completes, the temporary file is cleaned up.

**Model selection**: The normal investigate flow uses whatever model is configured (default: `claude-sonnet-4-20250514`). The gardener always overrides this to use `claude-opus-4-6` via both the CLI `--model` flag and the API model parameter — wiki quality improvement demands the strongest model available. The model override is passed through both code paths (Claude Code CLI subprocess and direct API call) so the gardener always gets Opus regardless of which path succeeds.

---

## Batch Mode

In addition to the interactive CLI, DiskWise supports a batch mode that splits the investigation flow into composable subcommands. Each subcommand reads/writes JSON, making it usable by other tools (including Claude Code itself as an outer orchestrator).

**`diskwise scan`**: Runs the system scan and outputs JSON findings to stdout with `scan_output` (raw text) and `findings` (structured list). Redirect stderr for progress messages.

**`diskwise analyze SCAN_FILE`**: Takes the path to a scan output JSON file, fetches wiki pages, calls Claude, and outputs `ClaudeAdvice` JSON (analysis, cleanup_actions, wiki_contributions) to stdout.

**`diskwise cleanup ACTION_JSON`**: Executes a single cleanup action from a JSON string argument. Outputs `{"success": true/false, "message": "..."}`.

**`diskwise contribute CONTRIB_JSON`**: Pushes a single wiki contribution from a JSON string argument. Outputs `{"success": true/false, "message": "..."}`.

**`diskwise garden`**: Runs the gardener. Fetches the full wiki (including `_meta/`), enters the refactoring loop using Claude Opus 4.6, and updates the meta wiki. Progress is written to stderr. This command takes no input — it operates on the wiki as a whole.

This allows an outer agent (e.g., Claude Code) to: (1) run the scan, (2) read the findings, (3) call analyze, (4) present cleanup choices to the user, (5) selectively execute approved actions. Gardening is run separately, on its own schedule.

---

## Scanner Design

The scanner is intentionally generic. It runs three broad filesystem queries:

1. **`df -h`** — overall disk usage
2. **`du -sh <path>/*/`** and **`du -sh <path>/.*/`** — largest top-level directories (including hidden), sorted by size
3. **`find <path> -maxdepth 5 -type f -size +100M`** — large files

No tool-specific paths are hardcoded. The scanner does not know about npm, Docker, Cabal, or any other tool. All interpretation of what the findings mean, what's safe to delete, and how to clean up is delegated to Claude with the wiki as context.

On Windows/MINGW, the scanner converts Windows paths (e.g., `C:\Users\foo`) to MINGW paths (`/c/Users/foo`) and uses `sh -c` to run all commands through the MINGW shell.

---

## Invariants

1. **Nothing is deleted from the user's system without explicit confirmation.**
2. **No personal data is ever written to the wiki.**
3. **Every agent has equal access. There are no privileged contributors.**
4. **Agents can create and amend wiki content. The gardener can additionally reorganize, merge, and restructure.** The git history is the safety net. Every version is recoverable.
5. **The wiki is useful on its own, even without the tool.** A human can browse it on GitHub and get value from it.
6. **The tool is useful on its own, even without the wiki.** Claude can still analyze a system with no wiki context. The wiki just makes it better.
7. **The tool requires minimal configuration.** Wiki reads work out of the box (public repo). Wiki writes require the `DISKWISE_WIKI_TOKEN` environment variable (a classic GitHub PAT). Claude access falls through to an existing Claude Code subscription or an API key.
8. **The gardener always uses the strongest model.** Wiki quality is a long-term investment. Cutting corners on model quality defeats the purpose.
9. **Normal agents do not refactor the wiki.** They contribute raw knowledge. The gardener shapes it.
10. **The meta wiki belongs to the gardener.** Normal agents do not read from or write to `_meta/`. This is enforced at two levels: `fetchTree` (used by the normal flow) filters out `_meta/` paths, and `offerLearn` defensively drops any contributions targeting `_meta/` before presenting them to the user. The gardener uses `fetchFullTree` which includes `_meta/` paths.