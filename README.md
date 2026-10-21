# Credit Policy Tool v2

> Under construction — this is the in-flight overhaul of the Credit Policy Tool. See `../docs/superpowers/specs/2026-04-11-credit-policy-overhaul-design.md` for the full design.

## Quickstart

```bash
# One-time setup (installs pinned R packages)
Rscript setup.R

# Launch on default port 7861
Rscript run.R

# Or a custom port
Rscript run.R 3838
```

The old app at the parent folder still runs unchanged on port 7860 via the old `Rscript run.R`.

## AI Providers (optional)

The app is fully functional without any AI configuration. To enable AI features (segment explanations, scenario narration, export report executive summary), set one of these env var sets before launch:

```bash
# OpenAI
export OPENAI_API_KEY=sk-...

# Azure OpenAI
export AZURE_OPENAI_API_KEY=...
export AZURE_OPENAI_ENDPOINT=https://your-resource.openai.azure.com
export AZURE_OPENAI_DEPLOYMENT=gpt-4o-mini

# Anthropic
export ANTHROPIC_API_KEY=sk-ant-...
```

See `.env.example` for the full list.
