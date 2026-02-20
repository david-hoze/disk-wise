# Garden

The gardener is a separate command (`diskwise garden`) that improves wiki quality. It is never invoked as part of the normal investigate flow.

## Model

The gardener always uses the strongest available model (`claude-opus-4-6`) at high effort. Wiki gardening requires the strongest reasoning available — it involves judging quality, spotting subtle duplication, making organizational decisions, and writing clear prose. Weaker or faster models are not suitable.

## Flow

1. The gardener fetches the entire wiki, including the meta wiki (`_meta/`).
2. It partitions pages into content pages and meta pages. Meta pages are presented to the engine in a separate section so it understands they are its own notes, not content to refactor.
3. It enters a gardening loop (up to 5 passes): build a prompt with content pages + meta pages + agent identity + outcome history, call the engine, parse the result, push improvements with the `diskwise-gardener:` commit prefix.
4. Between passes, it re-fetches the full wiki tree so it can see its own changes (including any `_meta/` pages it wrote as contributions during the pass).
5. The loop stops when the engine returns `done=true`, produces no contributions, or the pass limit (5) is reached.
6. After the loop completes, the gardener writes a session summary to `_meta/gardening-log.md` recording what each pass accomplished. If the page doesn't exist yet (first garden run), it is created; otherwise it is amended.
7. Each improvement is a separate commit so the history stays granular.

## Types of Improvements

- Merge duplicate or overlapping pages.
- Rewrite unclear explanations.
- Reorganize structure (move content to better locations).
- Split pages that got too long.
- Create new category pages to tie related topics together.
- Fix formatting inconsistencies.
- Add missing `## History` entries.

## What the Gardener Does NOT Do

- Remove useful information.
- Add speculative content not based on existing pages.
- Change the meaning of existing advice.
- Reorganize, merge, or rewrite `_meta/` pages as if they were wiki content. The gardener's system prompt explicitly states: "_meta/ pages are your notes to your future self, not content to refactor."

## Meta Wiki

The meta wiki lives under `_meta/` in the wiki repository. It is the gardener's own knowledge base about the knowledge base. The gardener reads it at the start of every session and writes to it after every pass.

The meta wiki is freeform, but typical content includes:

- **Gardening log**: What was improved, when, and why. A running record of organizational decisions.
- **Structural observations**: Patterns the gardener noticed (e.g., "the haskell/ section has grown large and may need subcategories").
- **Deferred work**: Things the gardener noticed but didn't fix this session, and why.
- **Quality trends**: Whether the wiki is getting better or worse in specific areas.
- **Conventions**: Emerging patterns that should be standardized.

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
