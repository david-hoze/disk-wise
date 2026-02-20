# Learn

After an investigation, Claude reviews the entire session — the scan, its analysis, the user's actions, any errors encountered, any surprises — and drafts wiki contributions (new pages or amendments to existing ones).

The bar for contributing is low. If it might save a future agent five seconds of reasoning or prevent it from making a wrong suggestion, write it down.

The normal CLI **contributes only**. It does not refactor or reorganize the wiki. That responsibility belongs to the gardener.

## Session Context

The learn prompt includes the full session context:

- **Platform**: OS, architecture, and shell (e.g., `mingw64_nt / x86_64 / bash`)
- **Scan output**: Raw filesystem data
- **All session events**: Actions executed (with measured bytes freed vs. estimates), actions failed (with errors), actions skipped (with user's reason), wiki contributions pushed/skipped, user feedback
- **Agent identity**: A privacy-preserving identifier (hash of hostname) for History entries
- **Session history**: Summaries of recent sessions including skip patterns, command reliability stats, and regrowth reports
- **User feedback**: Post-cleanup reports of breakage (highest-value signal)

## What Gets Written Back

Examples of things worth writing back:

- A tool or cache directory the wiki didn't know about yet.
- A safer cleanup method than what was previously documented.
- A caveat discovered the hard way (e.g., "don't delete X if Y is installed").
- Platform-specific paths (e.g., Homebrew on Apple Silicon vs Intel).
- An error encountered during cleanup and how it was resolved.
- A workaround for a permission issue on a specific OS.
- Size corrections when actual freed space differs significantly from wiki estimates.
- Commands that failed on specific platforms.
- Unexpected interactions between tools (e.g., "removing Conda breaks pyenv shims").
- Observations about typical sizes, growth rates, or patterns on real systems.

Examples of things NOT written back:

- User-specific paths or usernames.
- Anything containing secrets, tokens, or personal data.

## Contribution Approval

Each wiki contribution is presented to the user with type, path, summary, and a content preview (first 10 lines). The user can:

- `[y]` Approve and push as-is
- `[n]` Skip (recorded as `ContribSkipped`)
- `[e]` Edit first (recorded as `ContribEdited`)
- `[a]` Approve all remaining contributions without further prompts
- `[s]` Skip all remaining contributions without further prompts

When the user picks `[a]` or `[s]`, all subsequent contributions in the session are automatically approved or skipped (shown as `[auto-approve]` or `[auto-skip]`).

Failed pushes are saved to `~/.diskwise/pending/` and retried at the start of the next session.

## Deduplication

Before presenting contributions, they are deduplicated against existing wiki pages. If a `CreatePage` targets a path that already exists, it is automatically converted to an `AmendPage`. Contributions targeting `_meta/` paths are filtered out (that space belongs to the gardener).
