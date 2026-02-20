# DiskWise — Behavioral Specification (v4)

## Overview

DiskWise is a disk cleanup assistant backed by a shared wiki. An AI agent scans the user's system, consults a community-maintained knowledge base for context, advises the user, and writes back anything new it learned. A separate **gardener** command reviews and improves wiki quality using the strongest available model, and maintains a meta wiki of its own observations about the knowledge base.

The agent learns from every session — tracking what worked, what failed, what the user skipped, and what regrew — and feeds that experience back into both the wiki and future sessions.

---

## Sections

1. **[Core Concepts](concepts.md)** — Wiki, Meta Wiki, Agent, Gardener, Investigation
2. **[Investigate](investigate.md)** — Scanning, wiki matching, Claude analysis
3. **[Act](act.md)** — Cleanup execution with measurement and validation
4. **[Learn](learn.md)** — Session-aware wiki contributions
5. **[Garden](garden.md)** — Wiki quality improvement and meta wiki
6. **[Session Learning](session-learning.md)** — Cross-session intelligence: skip patterns, regrowth tracking, command reliability, feedback loops
7. **[Wiki Structure](wiki-structure.md)** — Directory layout, page format, wiki access, gist backend
8. **[Claude Access](claude-access.md)** — CLI subprocess, API fallback, model selection
9. **[Batch Mode](batch-mode.md)** — JSON subcommands for scripting
10. **[Scanner Design](scanner-design.md)** — Generic filesystem scanning
11. **[Invariants](invariants.md)** — Core principles and guarantees
