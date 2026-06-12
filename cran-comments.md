# cran-comments

## Submission of blockr.dplyr 0.2.0

This release rewrites the block UIs as JS-driven components and removes
three constructors that were merged into unified blocks (documented in
NEWS.md under breaking changes; serialized boards from 0.1.0 are
migrated on load).

## R CMD check results

0 errors | 0 warnings | 0 notes

Checked locally with `--as-cran` on R 4.5.2 (Linux) and via GitHub
Actions (Linux/macOS/Windows, release and devel).

## Reverse dependencies

There are no CRAN reverse dependencies.
