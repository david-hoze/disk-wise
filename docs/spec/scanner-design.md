# Scanner Design

The scanner is intentionally generic. It runs three broad filesystem queries:

1. **`df -h`** — overall disk usage
2. **`du -sh <path>/*/`** and **`du -sh <path>/.*/`** — largest top-level directories (including hidden), sorted by size
3. **`find <path> -maxdepth 5 -type f -size +100M`** — large files

No tool-specific paths are hardcoded. The scanner does not know about npm, Docker, Cabal, or any other tool. All interpretation of what the findings mean, what's safe to delete, and how to clean up is delegated to the analysis engine with the wiki as context.

## Finding Parser

The scan output is parsed into structured `Finding` records. The parser handles:

- `du`-style output: `SIZE\tPATH` (e.g., `1.2G\t/home/user/.cache`)
- `ls -lh` output for individual files
- Human-readable size suffixes: K, M, G, T

Findings below the configured minimum size threshold (`--min-size`, default 50 MB) are filtered out.

Each finding is categorized heuristically by path patterns: `cache`, `log`, `temp`, `build-artifact`, `docker`, `package-manager`, `app-data`, or `other`.

## Platform Support

On Windows/MINGW, the scanner converts Windows paths (e.g., `C:\Users\foo`) to MINGW paths (`/c/Users/foo`) and uses `sh -c` to run all commands through the MINGW shell.

## Measurement Functions

The scanner provides measurement utilities used during cleanup:

- **`measurePathSize`**: Measures a path's size in bytes via `du -sb` (Linux) or `du -sk` (macOS/MINGW).
- **`measureDiskFree`**: Measures available disk space on the root filesystem via `df -B1` (Linux) or `df -k` (macOS/MINGW).
- **`detectPlatform`**: Detects OS (via `uname -s`), architecture (via `uname -m`), and shell (from `$MSYSTEM` or `$SHELL`).

## Dry-Run Validation

Before presenting each cleanup action to the user, the agent validates it:

- Checks that the command binary exists on the system (`findExecutable`)
- Checks that referenced paths exist (`doesPathExist`)
- Warnings are shown inline before the user decides whether to execute
