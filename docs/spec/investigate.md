# Investigate

When the user runs the tool:

1. **Previous session follow-up**: Before scanning, the agent loads the most recent session summary. If the previous session cleaned any paths, it measures them again to detect regrowth. It also asks whether anything broke after the last session's cleanup (delayed side effects).

2. **Scan**: The agent scans the local filesystem and gathers raw disk usage data. The scanner is generic — it runs `df`, `du`, and `find` to discover what's large without any tool-specific knowledge baked in. All interpretation is left to Claude and the wiki.

3. **Wiki fetch**: The agent pulls the latest wiki from GitHub (or Gist, depending on configuration).

4. **Match**: The agent matches scan findings to wiki topics by path, tool name, and platform. Unmatched findings are flagged as novel.

5. **Command reliability**: The agent computes per-command success/failure statistics from session history. Commands with 2+ occurrences across sessions get reliability ratings included in the Claude prompt.

6. **Analysis**: The agent sends the scan data, matched wiki pages, novel findings, and command reliability stats to Claude. Claude produces advice grounded in community knowledge.

7. **Act**: The user sees the advice and decides what to act on (see [Act](act.md)).

8. **Learn**: Claude reviews the session and drafts wiki contributions (see [Learn](learn.md)).

9. **Feedback**: If any actions were executed, the agent asks whether anything broke or behaved unexpectedly.

10. **Persist**: The agent summarizes the session and appends it to the local history file for cross-session learning.
