# DiskWise

**Smart disk cleanup with shared wiki knowledge.**

DiskWise scans your filesystem, consults a community-maintained wiki for context, analyzes your disk usage, presents cleanup advice, and writes anything new it learned back to the wiki. Every instance reads from and writes to the same shared knowledge base — and learns from every session.

## How It Works

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│   Scanner    │────▶│  Wiki Match  │────▶│   Engine     │
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
                     │   Gardener improves wiki              │
                     │   quality, maintains _meta/ notes     │
                     └──────────────────────────────────────┘
```

### The Flow

1. **Follow-up**: At session start, DiskWise checks whether previously cleaned paths have regrown and asks about delayed side effects from the last session.
2. **Investigate**: A generic scanner runs `df`, `du`, and `find` to discover what's large — no tool-specific knowledge baked in. DiskWise fetches wiki pages from GitHub and matches them to findings by path patterns and tool names. Unmatched findings are flagged as novel. Everything goes to the analysis engine for interpretation, along with command reliability statistics from previous sessions.
3. **Act**: The engine proposes cleanup actions with risk levels and size estimates. Each action is validated (binary exists, paths exist) before presentation. You confirm each one before it runs. DiskWise measures actual space freed and records outcomes. Nothing is ever deleted without your say-so.
4. **Learn**: The engine reviews the entire session — what was scanned, what actions ran, what failed, what the user skipped and why, how much space was actually freed — and drafts wiki contributions for anything useful. Contributions are deduplicated against existing pages.
5. **Feedback**: The agent asks whether anything broke after cleanup. User feedback is the highest-priority learning signal.
6. **Garden** (separate command): `diskwise garden` reviews the entire wiki and improves quality — merging duplicates, rewriting, reorganizing. It maintains its own notes in `_meta/` pages across sessions. Runs up to 5 passes per invocation.

### Session Learning

DiskWise gets smarter over time by tracking patterns across sessions:

- **Skip patterns**: When users repeatedly skip an action (e.g., "too risky"), the wiki page's risk level may need updating.
- **Space measurement**: Actual bytes freed are compared to wiki estimates — significant mismatches trigger wiki corrections.
- **Regrowth detection**: Previously cleaned paths are re-measured at session start to detect directories that grow back quickly.
- **Command reliability**: Per-command success/failure rates are aggregated across sessions and included in prompts.
- **Platform correlation**: Failures are linked to OS/architecture/shell so platform-specific issues get documented.
- **User feedback**: Reports of breakage are treated as the highest-priority signal for wiki updates.

## Installation

```bash
# Clone and build
git clone git@github.com:david-hoze/disk-wise.git
cd disk-wise
cabal build

# Install to PATH
cabal install

# Run
DISKWISE_WIKI_TOKEN=your-key diskwise
```

### Wiki Access

The wiki is built in. Reads are unauthenticated (public repo). For write access, set `DISKWISE_WIKI_TOKEN` to a classic GitHub personal access token (PAT) with write access to the wiki repo.

### Gist Backend

For a simpler setup without a dedicated repo, set `DISKWISE_GIST_ID` to store wiki pages in a GitHub Gist instead. The same `DISKWISE_WIKI_TOKEN` PAT works (needs `gist` scope).

## Usage

### Interactive Mode

```bash
# Interactive mode (default)
diskwise

# Scan specific directories
diskwise --scan-path /home --scan-path /var

# Set minimum file size to report
diskwise --min-size 100

# Use gist backend instead of repo
diskwise --gist-id abc123def456
```

### Batch Mode

Batch mode splits the flow into composable JSON subcommands, usable by scripts or automation:

```bash
# Step 1: Scan the system, get JSON findings
diskwise scan > scan.json

# Step 2: Analyze with wiki context, get advice as JSON
diskwise analyze scan.json > advice.json

# Step 3: Execute a specific cleanup action
diskwise cleanup '{"description":"Remove pip cache","command":"pip cache purge","risk_level":"low","size_estimate":"~500M","wiki_ref":null}'

# Step 4: Push a wiki contribution
diskwise contribute '{"type":"CreatePage","path":"python/pip.md","content":"# pip\n...","summary":"add pip page"}'
```

All batch subcommands write progress to stderr and results to stdout.

### Garden Mode

The gardener command improves wiki quality — merging duplicates, rewriting unclear content, reorganizing structure. It maintains its own notes in `_meta/` pages across sessions.

```bash
# Run the gardener
diskwise garden
```

The gardener runs up to 5 improvement passes, re-reading the wiki between each pass so it can see its own changes. It records a session log in `_meta/gardening-log.md`.

## Project Structure

```
diskwise/
├── app/
│   └── Main.hs              # CLI entry point, arg parsing
├── src/DiskWise/
│   ├── Types.hs             # Domain types (WikiPage, AnalysisResult, SessionLog, etc.)
│   ├── Wiki.hs              # GitHub wiki API: fetch, match, create, update, retry
│   ├── WikiGist.hs          # Gist backend: same interface, flat file storage
│   ├── WikiRouter.hs        # Routes wiki calls to repo or gist based on config
│   ├── Engine.hs            # Analysis engine: API access, prompt building
│   ├── Scanner.hs           # Generic system scanning (du, find) + measurement utilities
│   ├── History.hs           # Session persistence: save/load summaries, compute patterns
│   ├── Batch.hs             # Non-interactive JSON subcommands (scan, analyze, cleanup, contribute)
│   └── CLI.hs               # Interactive UI: investigate, act, learn, feedback loops
├── test/DiskWise/
│   ├── TypesSpec.hs         # JSON round-trip tests for all types
│   ├── WikiSpec.hs          # Pattern matching and page extraction tests
│   ├── WikiGistSpec.hs      # Gist path encoding/decoding tests
│   ├── ScannerSpec.hs       # Finding parser and validation tests
│   ├── EngineSpec.hs        # Prompt building and response parsing tests
│   └── HistorySpec.hs       # Session summary, skip patterns, command stats tests
├── docs/
│   └── spec/                # Behavioral specification (v4), split by topic
│       ├── index.md         # Table of contents
│       ├── concepts.md      # Core concepts
│       ├── investigate.md   # Investigation flow
│       ├── act.md           # Cleanup execution
│       ├── learn.md         # Wiki contributions
│       ├── garden.md        # Wiki gardening + meta wiki
│       ├── session-learning.md  # Cross-session intelligence
│       ├── wiki-structure.md    # Wiki layout, access, gist backend
│       ├── engine-access.md     # Engine / API access
│       ├── batch-mode.md        # JSON subcommands
│       ├── scanner-design.md    # Scanner internals
│       └── invariants.md        # Core principles
├── diskwise.cabal
└── README.md
```

## Design Decisions

- **Generic scanner, smart interpreter**: The scanner knows nothing about specific tools — it just finds what's big. All interpretation (what's safe to delete, how to clean up) comes from the analysis engine + the wiki. This keeps the scanner simple and portable across platforms.
- **Wiki over rules**: Instead of a local rules engine, DiskWise uses a shared GitHub wiki. Every instance contributes to a growing knowledge base that benefits all users.
- **Session-aware learning**: The engine sees the full session — actions executed, errors encountered, user decisions and their reasons, actual space freed — not just scan data. This grounds wiki contributions in real experience.
- **Cross-session intelligence**: Skip patterns, command reliability, regrowth detection, and user feedback accumulate across sessions. DiskWise gets smarter with use.
- **Dual backend**: Wiki pages can be stored in a GitHub repo (full directory structure) or a GitHub Gist (flat, simpler setup). The routing is transparent to the rest of the codebase.
- **Gardener separation**: Normal sessions contribute raw knowledge. The gardener (a separate command) shapes the wiki into a well-organized knowledge base, maintaining its own meta wiki for cross-session context.
- **Optimistic concurrency**: Wiki writes use SHA-based conflict detection with exponential backoff retry. Failed writes are saved locally and retried next session.
- **Minimal config**: Set your API key and DiskWise handles the rest. Wiki access is built in.
- **User approval is mandatory**: Nothing is deleted without explicit confirmation. Wiki contributions are previewed before pushing.
