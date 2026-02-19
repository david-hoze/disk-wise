# DiskWise

**AI-powered disk cleanup that learns your system over time.**

DiskWise uses Claude to investigate your disk usage and discover cleanup patterns. But instead of asking Claude every time, it builds up a **rule store** — a persistent set of approved rules that can fire automatically without any AI calls.

## How It Works

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│   Scanner    │────▶│  Rules Engine │────▶│  Known fixes │
│  (du, find)  │     │  (local eval) │     │  (no AI!)    │
└──────────────┘     └──────┬───────┘     └──────────────┘
                            │
                    Novel items only
                            │
                     ┌──────▼───────┐     ┌──────────────┐
                     │    Claude    │────▶│ Proposed Rule │
                     │  (API call)  │     │   (pending)   │
                     └──────────────┘     └──────┬───────┘
                                                  │
                                          User approves?
                                                  │
                                          ┌───────▼──────┐
                                          │  Rule Store  │
                                          │ (~/.diskwise)│
                                          └──────────────┘
```

### The Learning Loop

1. **First run**: Scanner gathers disk data → Claude analyzes everything → proposes rules
2. **You approve** rules that make sense (e.g., "old GHC versions in ~/.ghcup can be deleted")
3. **Next run**: Rules fire automatically for known patterns. Only *novel* findings go to Claude
4. **Over time**: The tool handles more and more without AI, becoming faster and cheaper

### Example Flow

```
$ diskwise run

── Evaluating existing rules ──

✓ Rule matched: clean-old-ghc-versions
  Delete GHC versions older than 90 days from ~/.ghcup/ghc
  Found 3 item(s)
    · ~/.ghcup/ghc/9.4.7
    · ~/.ghcup/ghc/9.2.5
    · ~/.ghcup/ghc/8.10.7
  Execute action? [y/n] > y
  ✓ Deleted 3 items

── Asking Claude to analyze ──

Claude's Analysis:
─────────────────
I found 2.3GB in ~/.cache/bazel that hasn't been accessed in 6 months...

┌─ Proposed Rule ──────────────────────
│ Name: clean-bazel-cache
│ Description: Remove Bazel cache entries older than 60 days
│ Condition: OlderThanDays "~/.cache/bazel" 60
│ Action: SuggestDelete "~/.cache/bazel" "Stale build cache"
│
│ Claude's reasoning: You have Bazel build artifacts from
│ projects that are no longer active. This is safe to delete
│ as Bazel will re-fetch/rebuild as needed.
└──────────────────────────────────────
  Approve this rule? [y/n/q] > y
  ✓ Rule approved and saved as rule-4
    This pattern will be checked automatically next time.
```

## Installation

```bash
# Clone and build
git clone <repo-url>
cd diskwise
cabal build

# Set your API key
export ANTHROPIC_API_KEY="sk-ant-..."

# Run
cabal run diskwise -- run
```

## Usage

```bash
# Interactive mode (default) — scan, investigate, manage rules
diskwise run

# Apply existing rules only — no AI calls, great for cron
diskwise scan

# List all saved rules
diskwise rules

# Scan specific directories
diskwise run -p /home -p /var

# Use a custom rules file
diskwise run -r ./my-rules.json

# Set minimum file size to report
diskwise run -m 100   # only files > 100MB
```

### Automation with Cron

```cron
# Check rules daily, log results
0 8 * * * diskwise scan >> ~/.diskwise/scan.log 2>&1
```

## Rule Types

### Conditions
| Condition | Description |
|-----------|-------------|
| `PathExists path` | Fires if path exists |
| `PathGlob dir pattern` | Fires if glob matches files |
| `OlderThanDays path n` | Fires if contents are older than n days |
| `LargerThanMB path n` | Fires if path exceeds n MB |
| `AndCondition [...]` | All sub-conditions must match |
| `OrCondition [...]` | Any sub-condition matches |

### Actions
| Action | Description |
|--------|-------------|
| `SuggestDelete path reason` | Suggest removing a path |
| `RunCommand cmd description` | Run an arbitrary shell command |
| `SuggestArchive src dest reason` | Move files to archive location |
| `ReportSize path description` | Report size without acting |

## Project Structure

```
diskwise/
├── app/
│   └── Main.hs              # CLI entry point, arg parsing
├── src/DiskWise/
│   ├── Types.hs             # Core domain types (Rule, Condition, Action)
│   ├── Rules.hs             # Rule engine: load, save, evaluate
│   ├── Claude.hs            # Claude API: investigate, propose rules
│   ├── Scanner.hs           # System scanning (du, find, etc.)
│   └── CLI.hs               # Interactive UI with approval flow
├── diskwise.cabal
└── README.md
```

## Design Decisions

- **Rules are data, not code**: Rules serialize to JSON, so they're inspectable, editable, and portable
- **Claude sees existing rules**: When investigating, Claude knows what rules already exist, so it won't re-propose things you've already handled
- **Hit counts track value**: Each rule tracks how many times it has fired, so you can see which rules are most useful
- **Scan-first architecture**: The scanner always runs first; Claude only sees what the rules didn't catch, keeping API costs low
- **User approval is mandatory**: Claude can never add a rule or execute an action without your explicit approval
