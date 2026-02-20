# Core Concepts

**Wiki**: A GitHub repository of markdown files organized by topic. It contains human-readable knowledge about what lives where on a filesystem, what's safe to delete, what isn't, and how to clean things up. The wiki is the tool's shared memory.

**Meta Wiki**: A section of the wiki (`_meta/`) maintained exclusively by the gardener. It contains observations about the wiki itself — organizational patterns, quality trends, recurring issues, gardening decisions and rationale. The meta wiki is the gardener's own memory across sessions.

**Agent**: A single running instance of DiskWise on a user's machine. Every agent has equal read/write access to the wiki. There is no distinction between contributors and consumers.

**Gardener**: A dedicated command (`diskwise garden`) that reviews and improves wiki quality. The gardener always runs with the strongest available model at high effort, because wiki quality improvement requires deep reasoning about organization, duplication, clarity, and cross-page coherence. The gardener also reads and writes the meta wiki, building up its own understanding of the knowledge base over time. Gardening is never triggered automatically by the normal investigate flow — it is a separate, intentional operation.

**Investigation**: The agent scans the local filesystem, pulls relevant wiki pages, sends both to the analysis engine, presents advice to the user, and pushes any new knowledge back to the wiki.

**Session**: A single run of the investigate flow. The agent tracks everything that happens during a session — findings, actions proposed and taken, skip reasons, space freed, user feedback — and persists a summary for cross-session learning.

**Session History**: A local JSONL file (`~/.diskwise/session-history.jsonl`) that accumulates session summaries over time. The agent reads this at the start of each session to inform the engine about patterns from previous runs.
