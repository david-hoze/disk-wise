# Wiki Structure

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

## Page Format

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

Pages under `troubleshooting/`, `techniques/`, and `observations/` are freeform. They don't need a fixed template. The only requirement is a `## History` section at the bottom so the provenance of each addition is traceable.

Meta wiki pages (`_meta/`) have no required format. They are the gardener's notes to its future self.

## Wiki Access

The wiki lives in a public GitHub repository. The agent accesses it via the GitHub REST API. No git binary is required. The user configures nothing — the tool handles wiki access out of the box.

**Reading**: The repo is public. The agent reads files via unauthenticated GET requests to the GitHub Contents API. No token needed.

**Writing**: The agent authenticates via a classic GitHub personal access token (PAT) provided in the `DISKWISE_WIKI_TOKEN` environment variable, scoped to write access on the single wiki repo and nothing else. Every agent uses the same token. This is not a secret — the repo is public and every agent is equally trusted. If the token is not set, wiki reads still work but writes will fail with a clear error.

**Conflict handling**: The GitHub Contents API requires the file's current SHA to update it. If the SHA is stale (another agent wrote to the same file concurrently), the PUT will fail. The agent re-fetches the current SHA, re-applies its amendment to the latest content, and retries. It keeps retrying with a short backoff (1s, 2s, 4s, ...) up to 5 attempts. If all retries are exhausted, the contribution is saved to a local pending queue and retried at the start of the next session.

**Commit messages**: Each write includes a descriptive commit message. Normal agents use: `diskwise-agent: add conda environments page`. The gardener uses a distinct prefix: `diskwise-gardener: merge overlapping docker pages`.

## Outcome Tracking on Wiki Pages

Each wiki page carries metadata for outcome tracking:

- `pageVerifyCount`: How many times advice from this page led to a successful cleanup
- `pageFailCount`: How many times advice from this page led to a failure
- `pageLastVerified`: When the page was last verified on a real system

This data is included in analysis prompts so the engine can prioritize reliable advice and flag pages with high failure rates.

## Gist Backend

As an alternative to a full GitHub repository, DiskWise supports storing wiki pages in a GitHub Gist. This is useful for small wikis or users who want a simpler setup without a dedicated repo.

**Configuration**: Set the `DISKWISE_GIST_ID` environment variable or pass `--gist-id GIST_ID` on the command line. When a gist ID is configured, all wiki operations route through the Gist API instead of the GitHub Contents API. The same `DISKWISE_WIKI_TOKEN` PAT is used for authentication (it needs the `gist` scope).

**Path encoding**: Gists are flat (no directories). Wiki paths are encoded by replacing `/` with `--`. For example, `tools/npm.md` becomes `tools--npm.md` in the gist, and `_meta/gardening-log.md` becomes `_meta--gardening-log.md`.

**API mapping**: Reading uses `GET /gists/{id}` which returns all file contents inline. Writing uses `PATCH /gists/{id}` with a `files` object. Gists have no SHA-based concurrency control — last write wins — so the retry-with-backoff logic used by the repo backend is not needed.

**Routing**: The `DiskWise.WikiRouter` module checks `configGistId` and delegates to either `DiskWise.Wiki` (repo backend) or `DiskWise.WikiGist` (gist backend). Pure functions like `matchPages`, `deduplicateContribs`, and `parseMetaComment` are shared between both backends.
