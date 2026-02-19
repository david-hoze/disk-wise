# DiskWise — Behavioral Specification (v2)

## Overview

DiskWise is a disk cleanup assistant backed by a shared wiki. An AI agent scans the user's system, consults a community-maintained knowledge base for context, advises the user, and writes back anything new it learned. The knowledge base is a single GitHub repository of markdown files that every agent reads from and writes to directly.

---

## Core Concepts

**Wiki**: A GitHub repository of markdown files organized by topic. It contains human-readable knowledge about what lives where on a filesystem, what's safe to delete, what isn't, and how to clean things up. The wiki is the tool's shared memory.

**Agent**: A single running instance of DiskWise on a user's machine. Every agent has equal read/write access to the wiki. There is no distinction between contributors and consumers.

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

The agent writes back **everything useful it learned during the session**. This is not limited to cleanup instructions. Any knowledge that could help a future agent on a different machine is worth recording.

After an investigation, Claude reviews the entire session — the scan, its analysis, the user's actions, any errors encountered, any surprises — and does two things:

**Contribute**: Draft new pages or amend existing ones with what it learned.

**Refactor**: After contributing, the agent enters a refactoring loop. It reviews the pages it touched and the surrounding pages, and improves their quality — merging duplicates, rewriting unclear explanations, reorganizing structure, splitting pages that got too long, moving content to better locations, creating new categories. After each pass, it evaluates whether the wiki is better organized than before. If it is, it does another pass. It keeps going until a pass produces no meaningful improvements — i.e., until the wiki converges. Each pass is a separate commit so the history stays granular.

The bar for contributing is low. If it might save a future agent five seconds of reasoning or prevent it from making a wrong suggestion, write it down.

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
- Subjective preferences about what to keep.

### 3. Act

When the user decides to clean something up:

- The agent shows exactly what will happen before doing it.
- The user confirms.
- The agent executes the cleanup.
- Nothing is ever deleted without the user saying yes.

---

## Wiki Structure

```
wiki/
  index.md                  # auto-generated table of contents
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

---

## Wiki Access

The wiki lives in a public GitHub repository. The agent accesses it via the GitHub REST API. No git binary is required. The user configures nothing — the tool handles wiki access out of the box.

**Reading**: The repo is public. The agent reads files via unauthenticated GET requests to the GitHub Contents API. No token needed.

**Writing**: A fine-grained GitHub token is baked into the tool, scoped to write access on the single wiki repo and nothing else. Every agent uses the same token. This is not a secret — the repo is public and every agent is equally trusted.

**Conflict handling**: The GitHub Contents API requires the file's current SHA to update it. If the SHA is stale (another agent wrote to the same file concurrently), the PUT will fail. The agent re-fetches the current SHA, re-applies its amendment to the latest content, and retries. It keeps retrying with a short backoff (1s, 2s, 4s, ...) up to 5 attempts. If all retries are exhausted, the contribution is saved to a local pending queue and retried at the start of the next session.

**Commit messages**: Each write includes a descriptive commit message like: `diskwise-agent: add conda environments page` `diskwise-agent: add caveat about ghcup symlinks` `diskwise-agent: note that du lies on APFS`

## Claude Access

The agent supports two ways to use Claude:

**API key**: The user sets their Anthropic API key. The agent calls the Claude API directly. This is pay-per-use.

**Claude subscription**: The user has a Claude Pro or Team subscription. The agent invokes Claude Code (`claude` CLI) as a subprocess, which authenticates via the user's existing subscription. No API key needed. This is the zero-config option — if the user already has Claude Code installed and logged in, it just works.

The agent tries Claude Code first. If it's not installed or not authenticated, it falls back to the API key. If neither is available, it exits with a clear message explaining the two options.

---

## Batch Mode

In addition to the interactive CLI, DiskWise supports a batch mode that splits the investigation flow into composable subcommands. Each subcommand reads/writes JSON, making it usable by other tools (including Claude Code itself as an outer orchestrator).

**`diskwise scan`**: Runs the system scan and outputs JSON to stdout with `scan_output` (raw text) and `findings` (structured list). Redirect stderr for progress messages.

**`diskwise analyze SCAN_FILE`**: Takes the path to a scan output JSON file, fetches wiki pages, calls Claude, and outputs `ClaudeAdvice` JSON (analysis, cleanup_actions, wiki_contributions) to stdout.

**`diskwise cleanup ACTION_JSON`**: Executes a single cleanup action from a JSON string argument. Outputs `{"success": true/false, "message": "..."}`.

**`diskwise contribute CONTRIB_JSON`**: Pushes a single wiki contribution from a JSON string argument. Outputs `{"success": true/false, "message": "..."}`.

This allows an outer agent (e.g., Claude Code) to: (1) run the scan, (2) read the findings, (3) call analyze, (4) present cleanup choices to the user, (5) selectively execute approved actions.

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
4. **Agents can create, append, rewrite, reorganize, and delete wiki content.** The git history is the safety net. Every version is recoverable.
5. **The wiki is useful on its own, even without the tool.** A human can browse it on GitHub and get value from it.
6. **The tool is useful on its own, even without the wiki.** Claude can still analyze a system with no wiki context. The wiki just makes it better.
7. **The tool requires zero configuration if the user has Claude Code installed.** Wiki access is built in. Claude access falls through to an existing subscription. The user just runs it.