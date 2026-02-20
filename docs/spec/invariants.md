# Invariants

1. **Nothing is deleted from the user's system without explicit confirmation.**
2. **No personal data is ever written to the wiki.**
3. **Every agent has equal access. There are no privileged contributors.**
4. **Agents can create and amend wiki content. The gardener can additionally reorganize, merge, and restructure.** The git history is the safety net. Every version is recoverable.
5. **The wiki is useful on its own, even without the tool.** A human can browse it on GitHub and get value from it.
6. **The tool is useful on its own, even without the wiki.** The engine can still analyze a system with no wiki context. The wiki just makes it better.
7. **The tool requires minimal configuration.** Wiki reads work out of the box (public repo). Wiki writes require the `DISKWISE_WIKI_TOKEN` environment variable (a classic GitHub PAT). Engine access requires the `DISKWISE_API_KEY` environment variable.
8. **The gardener always uses the strongest model.** Wiki quality is a long-term investment. Cutting corners on model quality defeats the purpose.
9. **Normal agents do not refactor the wiki.** They contribute raw knowledge. The gardener shapes it.
10. **The meta wiki belongs to the gardener.** Normal agents do not read from or write to `_meta/`. This is enforced at two levels: `fetchTree` (used by the normal flow) filters out `_meta/` paths, and `offerLearn` defensively drops any contributions targeting `_meta/` before presenting them to the user. The gardener uses `fetchFullTree` which includes `_meta/` paths.
11. **The agent learns from every session.** Skip reasons, space measurements, platform data, user feedback, and command outcomes are all persisted and fed back into future sessions.
12. **User feedback is the highest-priority signal.** If a user reports that something broke, the relevant wiki page must be updated — this takes priority over any other wiki contribution.
