# blockr.dplyr

<!-- badges: start -->
[![check](https://github.com/BristolMyersSquibb/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.dplyr/actions/workflows/check.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.dplyr/graph/badge.svg?token=VoOPRU65KA)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.dplyr)
<!-- badges: end -->

blockr.dplyr provides interactive blocks for data transformation. Filter, sort, summarize, join, and manipulate data through visual interfaces.

## Overview

blockr.dplyr is part of the blockr ecosystem and provides data transformation blocks.

## Installation

```r
install.packages("blockr.dplyr")
```

Or install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.dplyr")
```

## Getting Started

Create and launch an empty dashboard:

```r
library(blockr.dplyr)
serve(new_board())
```

This opens a visual interface in your web browser. Add blocks using the "+" button, connect them by dragging, and configure each block through its settings. Data transformations update in real-time as you build your workflow.

## Available Blocks

blockr.dplyr provides 16 data transformation blocks:

### Core Operations
- [select](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#select-block): choose which columns to keep
- [rename](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#rename-block): change column names
- [arrange](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#arrange-block): sort rows by column values
- [slice](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#slice-block): select specific rows by position or value

### Filtering
- [filter](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#filter-block): filter by selecting values from dropdowns (simple, no-code)
- [filter expression](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#filter-expression-block): keep rows that meet conditions (advanced, R expressions)

### Creating & Modifying Columns
- [mutate expression](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#mutate-expression-block): create or modify columns using R expressions

### Summarizing
- [summarize](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#summarize-block): calculate statistics using dropdown selections (simple, no-code)
- [summarize expression](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#summarize-expression-block): calculate statistics using R expressions (advanced)

### Combining Data
- [join](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#join-block): combine two datasets based on common columns
- [bind_rows](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#bind-rows-block): stack datasets vertically
- [bind_cols](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#bind-cols-block): combine datasets side-by-side

### Reshaping
- [pivot_longer](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#pivot-longer-block): reshape data from wide to long format
- [pivot_wider](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#pivot-wider-block): reshape data from long to wide format
- [unite](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#unite-block): combine multiple columns into one
- [separate](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#separate-block): split one column into multiple columns

See `vignette("blockr-dplyr-showcase")` for a complete showcase with screenshots and detailed explanations of each block.

## Learn More

The [blockr.dplyr website](https://bristolmyerssquibb.github.io/blockr.dplyr/) includes full documentation and the showcase vignette. For information on the workflow engine, see [blockr.core](https://bristolmyerssquibb.github.io/blockr.core/).
