# Batch Mode

In addition to the interactive CLI, DiskWise supports a batch mode that splits the investigation flow into composable subcommands. Each subcommand reads/writes JSON, making it usable by other tools (including Claude Code itself as an outer orchestrator).

**`diskwise scan`**: Runs the system scan and outputs JSON findings to stdout with `scan_output` (raw text) and `findings` (structured list). Redirect stderr for progress messages.

**`diskwise analyze SCAN_FILE`**: Takes the path to a scan output JSON file, fetches wiki pages, calls Claude, and outputs `ClaudeAdvice` JSON (analysis, cleanup_actions, wiki_contributions) to stdout.

**`diskwise cleanup ACTION_JSON`**: Executes a single cleanup action from a JSON string argument. Outputs `{"success": true/false, "message": "..."}`.

**`diskwise contribute CONTRIB_JSON`**: Pushes a single wiki contribution from a JSON string argument. Outputs `{"success": true/false, "message": "..."}`.

**`diskwise garden`**: Runs the gardener. Fetches the full wiki (including `_meta/`), enters the refactoring loop using Claude Opus 4.6, and updates the meta wiki. Progress is written to stderr. This command takes no input — it operates on the wiki as a whole.

This allows an outer agent (e.g., Claude Code) to: (1) run the scan, (2) read the findings, (3) call analyze, (4) present cleanup choices to the user, (5) selectively execute approved actions. Gardening is run separately, on its own schedule.
