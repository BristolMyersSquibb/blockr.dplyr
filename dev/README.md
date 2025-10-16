# Developer Documentation

This folder contains internal developer documentation for blockr.dplyr. These files are **not** included in the package build (excluded via `.Rbuildignore`).

## What's Here

### [blocks-core-guide.md](blocks-core-guide.md)
**Universal guide to blockr block development (start here)**

Core concepts applicable to all blockr packages. Covers:
- What is a block? (universal concept)
- Block anatomy (UI, Server, Constructor)
- Step-by-step block creation
- Testing and validation
- blockr.core vignette references

**Use this when:**
- New to blockr development
- Understanding block fundamentals
- Learning universal patterns
- Creating blocks for any blockr package

### [ui-guidelines.md](ui-guidelines.md)
**UI development guidelines for dplyr blocks**

Focused guide for building consistent block UIs for data manipulation. Covers:
- Responsive layout system (auto 1-4 column grid)
- Form patterns for filters and selectors
- Table selection components
- Color palette and styling (minimalist gray/white)

**Use this when:**
- Designing block UI
- Implementing responsive layouts
- Creating filter or selection interfaces
- Ensuring consistent styling

## Quick Start for Developers

1. **Read** [blocks-core-guide.md](blocks-core-guide.md) to understand universal block concepts
2. **Review** [ui-guidelines.md](ui-guidelines.md) for UI design patterns
3. **Study** existing blocks in `../R/` directory (filter.R, mutate.R, select.R)
4. **Test** your blocks thoroughly

## Key Principles

- **State Management:** Include ALL constructor parameters in state list
- **Reactive Values:** Use reactiveVal() for all user-modifiable state
- **Expression Building:** Build dplyr expressions that can be piped together

## Related Resources

- **Core concepts:** blockr.core vignettes (`create-block`, `extend-blockr`)
- **Examples:** `../inst/examples/` directory
- **Block code:** `../R/` directory (filter.R, mutate.R, select.R, arrange.R, etc.)

## For AI Assistants

This documentation is optimized for both human developers and AI code assistants:

- **[blocks-core-guide.md](blocks-core-guide.md)** - Universal patterns (reusable across blockr packages)
- **[ui-guidelines.md](ui-guidelines.md)** - UI design patterns for dplyr blocks

Each guide contains:
- Complete patterns and templates
- Step-by-step instructions
- Common pitfalls with solutions
- Quick reference sections

Start with the core guide to understand how blockr blocks work, then review the UI guidelines for implementing consistent interfaces for data manipulation blocks.
