# blockr.dplyr Developer Resources

## Guides

- **[js-block-guide.md](js-block-guide.md)** — How to build a JS-driven block (start here)
- **[blockr-select-spec.md](blockr-select-spec.md)** — BlockrSelect component API
- **[blockr-input-spec.md](blockr-input-spec.md)** — BlockrInput component API
- **[testing-guide.md](testing-guide.md)** — Testing patterns

## Scripts

| Script | Purpose |
|--------|---------|
| `preview-all-blocks.R` | Dock + DAG preview of all 14 blocks |
| `test-ai-discovery.R` | AI discovery — basic prompts, all blocks |
| `test-ai-discovery-v2.R` | AI discovery — harder prompts, verbose |
| `test-ai-basic.R` | Shiny: filter → select → arrange → slice + AI |
| `test-ai-expr.R` | Shiny: mutate → summarize → rename + AI |
| `test-ai-reshape.R` | Shiny: pivot / unite / separate + AI |
