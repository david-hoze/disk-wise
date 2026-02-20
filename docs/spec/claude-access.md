# Claude Access

The agent supports two ways to use Claude:

**API key**: The user sets their Anthropic API key. The agent calls the Claude API directly. This is pay-per-use.

**Claude subscription**: The user has a Claude Pro or Team subscription. The agent invokes Claude Code (`claude` CLI) as a subprocess, which authenticates via the user's existing subscription. No API key needed. This is the zero-config option — if the user already has Claude Code installed and logged in, it just works.

The agent tries Claude Code first. If it's not installed or not authenticated, it falls back to the API key. If neither is available, it exits with a clear message explaining the two options.

## CLI Invocation Details

The agent calls Claude Code via `sh -c` to ensure PATH resolution works on all platforms (including MINGW/Windows). The prompt is written to a temporary file in UTF-8 encoding to avoid OS command-line length limits and Unicode encoding issues on Windows. The system prompt and user prompt are combined into a single file, which is piped to `claude --print` via stdin redirect (`< /path/to/file`). The temporary file uses the Windows temp directory (obtained via `getTemporaryDirectory`) for writing, and is converted to a MINGW path (via `toMingwPath`) for the shell command. The `CLAUDECODE` environment variable is unset in the shell command to allow the tool to be tested from within a Claude Code session. The CLI subprocess output is read in binary mode and decoded as UTF-8 explicitly, bypassing the system code page — this prevents non-ASCII characters (em dashes, etc.) from being garbled on Windows/MINGW where the default code page is not UTF-8. After the CLI call completes, the temporary file is cleaned up.

## Model Selection

The normal investigate flow uses whatever model is configured (default: `claude-sonnet-4-20250514`). The gardener always overrides this to use `claude-opus-4-6` via both the CLI `--model` flag and the API model parameter — wiki quality improvement demands the strongest model available. The model override is passed through both code paths (Claude Code CLI subprocess and direct API call) so the gardener always gets Opus regardless of which path succeeds.
