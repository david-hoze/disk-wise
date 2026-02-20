# Engine Access

The agent accesses the analysis engine via API key. The user sets their `DISKWISE_API_KEY` environment variable. The agent calls the API directly. This is pay-per-use.

If no API key is available, the agent exits with a clear message explaining how to set one up.

## Model Selection

The normal investigate flow uses whatever model is configured (default: `claude-sonnet-4-20250514`). The gardener always overrides this to use the strongest available model (`claude-opus-4-6`) via the API model parameter — wiki quality improvement demands the strongest model available.
