# blockr.dplyr

<!-- badges: start -->
[![check](https://github.com/BristolMyersSquibb/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.dplyr/actions/workflows/check.yaml)
[![coverage](https://codecov.io/gh/BristolMyersSquibb/blockr.dplyr/graph/badge.svg?token=VoOPRU65KA)](https://app.codecov.io/gh/BristolMyersSquibb/blockr.dplyr)
<!-- badges: end -->

blockr.dplyr provides interactive blocks for data transformation. Filter, sort, summarize, join, and manipulate data through visual interfaces.

## Overview

blockr.dplyr is part of the blockr ecosystem. blockr.core provides the workflow engine, blockr.ui provides the visual interface, and blockr.dplyr provides the data transformation blocks. These three packages work together to create interactive data workflows.

## Installation

You can install blockr.dplyr from CRAN:

```r
install.packages("blockr.dplyr")
```

Or install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("BristolMyersSquibb/blockr.dplyr")
```

## Getting Started

Create and launch an empty dashboard:

```r
library(blockr.core)
library(blockr.ui)
library(blockr.dplyr)
serve(new_dag_board())
```

This opens a visual interface in your web browser. Add blocks using the "+" button, connect them by dragging, and configure each block through its settings. Data transformations update in real-time as you build your workflow.

## Available Blocks

blockr.dplyr provides 13 data transformation blocks:

- [select](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#select-block): choose which columns to keep
- [expression_filter](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#expression-filter-block): keep rows that meet conditions
- [value_filter](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#value-filter-block): filter by selecting values from dropdowns
- [arrange](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#arrange-block): sort rows by column values
- [slice](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#slice-block): select specific rows by position or value
- [mutate](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#mutate-block): create or modify columns
- [rename](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#rename-block): change column names
- [summarize](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#summarize-block): calculate statistics, optionally by groups
- [join](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#join-block): combine two datasets based on common columns
- [pivot_longer](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#pivot-longer-block): reshape data from wide to long format
- [pivot_wider](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#pivot-wider-block): reshape data from long to wide format
- [bind_rows](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#bind-rows-block): stack datasets vertically
- [bind_cols](https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#bind-cols-block): combine datasets side-by-side

See `vignette("blockr-dplyr-showcase")` for a complete showcase with screenshots and detailed explanations of each block.

## Learn More

The [blockr.dplyr website](https://bristolmyerssquibb.github.io/blockr.dplyr/) includes full documentation and the showcase vignette. For information on the broader blockr ecosystem, see [blockr.core](https://bristolmyerssquibb.github.io/blockr.core/) and [blockr.ui](https://bristolmyerssquibb.github.io/blockr.ui/).
