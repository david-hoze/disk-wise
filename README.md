# DiskWise

**AI-powered disk cleanup with shared wiki knowledge.**

DiskWise scans your filesystem, consults a community-maintained wiki for context, sends both to Claude for analysis, presents cleanup advice, and writes anything new it learned back to the wiki. Every agent reads from and writes to the same shared knowledge base.

## How It Works

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│   Scanner    │────▶│  Wiki Match  │────▶│   Claude     │
│  (du, find)  │     │ (GitHub API) │     │  (analysis)  │
└──────────────┘     └──────────────┘     └──────┬───────┘
                                                  │
                                          ┌───────▼──────┐
                                          │   Advice     │
                                          │ + Cleanup    │
                                          │ + Wiki edits │
                                          └──────┬───────┘
                                                  │
                                     ┌────────────┤
                                     ▼            ▼
                              ┌──────────┐ ┌───────────┐
                              │   Act    │ │   Learn   │
                              │ (cleanup)│ │(wiki push)│
                              └──────────┘ └───────────┘

                     ┌──────────────────────────────────────┐
                     │   diskwise garden (separate command)  │
                     │   Gardener (Opus 4.6) improves wiki  │
                     │   quality, maintains _meta/ notes     │
                     └──────────────────────────────────────┘
```

### The Flow

1. **Investigate**: A generic scanner runs `df`, `du`, and `find` to discover what's large — no tool-specific knowledge baked in. The agent fetches wiki pages from GitHub and matches them to findings by path patterns and tool names. Unmatched findings are flagged as novel. Everything goes to Claude for interpretation.
2. **Act**: Claude proposes cleanup actions with risk levels and size estimates. You confirm each one before it runs. Nothing is ever deleted without your say-so.
3. **Learn**: Claude reviews the entire session — what was scanned, what actions ran, what failed, what surprised it — and drafts wiki contributions for anything useful. The bar is low: if it might save a future agent five seconds, it gets written.
4. **Garden** (separate command): `diskwise garden` uses Claude Opus 4.6 to review the entire wiki and improve quality — merging duplicates, rewriting, reorganizing. It maintains its own notes in `_meta/` pages across sessions. Runs up to 5 passes per invocation.

### Example Session

```
$ diskwise

+===========================================+
|           DiskWise v2.0.0                 |
|   AI-powered disk cleanup with shared     |
|   wiki knowledge                          |
+===========================================+

Retrying 1 pending contribution(s) from previous session...
  Retried OK: tools/conda.md

What would you like to do?
  [i] Investigate (scan + analyze + learn)
  [q] Quit
> i

-- Scanning system --

Found 14 items of interest.

-- Fetching wiki knowledge --

Loaded 23 wiki page(s).
Wiki matched 5 page(s) to findings.
9 finding(s) not covered by wiki.

-- Asking Claude to analyze --

Claude's Analysis:
-------------------
Your system has ~8GB of reclaimable space. The largest items are
stale Bazel cache (2.3GB) and unused Docker images (3.1GB)...

-- 3 cleanup action(s) suggested --

  Action:   Clean Bazel cache
  Command:  rm -rf ~/.cache/bazel
  Risk:     low
  Estimate: ~2.3 GB
  Wiki ref: build-tools/bazel.md
  Execute? [y/n] > y
  OK: Done: Clean Bazel cache

-- 2 wiki contribution(s) suggested --

  Type:    CreatePage
  Path:    build-tools/bazel.md
  Summary: diskwise-agent: add bazel cache cleanup page
  Preview:
    # Bazel
    ...
  Push to wiki? [y/n] > y
  OK: Contribution pushed to wiki.

-- Refactoring wiki --

  Refactoring pass 1...
  Reorganized build-tools section, merged overlapping content.
    Pushed: build-tools/bazel.md
  Refactoring pass 2...
  Wiki converged — no more improvements needed.
```

## Installation

```bash
# Clone and build
git clone git@github.com:david-hoze/disk-wise.git
cd disk-wise
cabal build

# Run (zero-config if you have Claude Code installed)
cabal run diskwise
```

### Claude Access

DiskWise supports two ways to use Claude:

- **Claude Code CLI** (preferred): If you have `claude` installed and logged in, it just works. No API key needed.
- **API key fallback**: Set `ANTHROPIC_API_KEY` in your environment. The agent falls back to this if the CLI isn't available.

### Wiki Access

The wiki is built in. No configuration needed. Reads are unauthenticated (public repo). For write access, set `DISKWISE_WIKI_TOKEN` with a GitHub token scoped to the wiki repo.

## Usage

### Interactive Mode

```bash
# Interactive mode (default)
diskwise

# Scan specific directories
diskwise --scan-path /home --scan-path /var

# Set minimum file size to report
diskwise --min-size 100

# Override API key
diskwise --api-key sk-ant-...

# Use a different Claude model
diskwise --model claude-sonnet-4-20250514
```

### Batch Mode

Batch mode splits the flow into composable JSON subcommands, usable by scripts or outer AI agents:

```bash
# Step 1: Scan the system, get JSON findings
diskwise scan > scan.json

# Step 2: Analyze with wiki + Claude, get advice as JSON
diskwise analyze scan.json > advice.json

# Step 3: Execute a specific cleanup action
diskwise cleanup '{"description":"Remove pip cache","command":"pip cache purge","risk_level":"low","size_estimate":"~500M","wiki_ref":null}'

# Step 4: Push a wiki contribution
diskwise contribute '{"type":"CreatePage","path":"python/pip.md","content":"# pip\n...","summary":"add pip page"}'
```

All batch subcommands write progress to stderr and results to stdout.

### Garden Mode

The gardener command uses Claude Opus 4.6 to improve wiki quality — merging duplicates, rewriting unclear content, reorganizing structure. It maintains its own notes in `_meta/` pages across sessions.

```bash
# Run the gardener (uses Opus 4.6, no extra config needed)
diskwise garden
```

The gardener runs up to 5 improvement passes, re-reading the wiki between each pass so it can see its own changes. It records a session log in `_meta/gardening-log.md`.

## Project Structure

```
diskwise/
├── app/
│   └── Main.hs              # CLI entry point, arg parsing
├── src/DiskWise/
│   ├── Types.hs             # Domain types (WikiPage, ClaudeAdvice, SessionLog, etc.)
│   ├── Wiki.hs              # GitHub wiki API: fetch, match, create, update, retry
│   ├── Claude.hs            # Claude access: CLI subprocess + API fallback
│   ├── Scanner.hs           # Generic system scanning (du, find) + finding parser
│   ├── Batch.hs             # Non-interactive JSON subcommands (scan, analyze, cleanup, contribute)
│   └── CLI.hs               # Interactive UI: investigate, act, learn, refactor
├── test/DiskWise/
│   ├── TypesSpec.hs         # JSON round-trip tests for all types
│   ├── WikiSpec.hs          # Pattern matching and page extraction tests
│   ├── ScannerSpec.hs       # Finding parser tests
│   └── ClaudeSpec.hs        # Prompt building and response parsing tests
├── docs/
│   └── spec.md              # Behavioral specification (v2)
├── diskwise.cabal
└── README.md
```

## Design Decisions

- **Generic scanner, smart interpreter**: The scanner knows nothing about specific tools — it just finds what's big. All interpretation (what's safe to delete, how to clean up) comes from Claude + the wiki. This keeps the scanner simple and portable across platforms.
- **Wiki over rules**: Instead of a local rules engine, DiskWise uses a shared GitHub wiki. Every agent contributes to a growing knowledge base that benefits all users.
- **Session-aware learning**: Claude sees the full session — actions executed, errors encountered, user decisions — not just scan data. This grounds wiki contributions in real experience.
- **Refactoring loop**: After contributing, the agent improves wiki quality until convergence. The wiki gets better organized over time without human curation.
- **Optimistic concurrency**: Wiki writes use SHA-based conflict detection with exponential backoff retry. Failed writes are saved locally and retried next session.
- **Zero-config goal**: If you have Claude Code installed, the tool works out of the box. Wiki access is built in. The user configures nothing.
- **User approval is mandatory**: Nothing is deleted without explicit confirmation. Wiki contributions are previewed before pushing.
