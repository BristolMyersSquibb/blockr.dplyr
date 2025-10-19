# blockr.dplyr

<!-- badges: start -->
[![check](https://github.com/BristolMyersSquibb/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.dplyr/actions/workflows/check.yaml)
<!-- badges: end -->

blockr.dplyr provides interactive blocks for data transformation. Filter, sort, summarize, join, and manipulate data through visual interfaces.

## Overview

blockr.dplyr is part of the blockr ecosystem. blockr.core provides the workflow engine, blockr.ui provides the visual interface, and blockr.dplyr provides the data transformation blocks. These three packages work together to create interactive data workflows.

## Installation

```r
# install.packages("devtools")
devtools::install_github("BristolMyersSquibb/blockr.dplyr")
devtools::install_github("BristolMyersSquibb/blockr.core")
devtools::install_github("BristolMyersSquibb/blockr.ui")
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

blockr.dplyr provides 11 data transformation blocks:

- select: choose which columns to keep
- filter: keep rows that meet conditions
- value_filter: filter by selecting values from dropdowns
- arrange: sort rows by column values
- slice: select specific rows by position or value
- mutate: create or modify columns
- rename: change column names
- summarize: calculate statistics, optionally by groups
- join: combine two datasets based on common columns
- bind_rows: stack datasets vertically
- bind_cols: combine datasets side-by-side

See `vignette("blockr-dplyr-showcase")` for a complete showcase with screenshots and detailed explanations of each block.

## Learn More

The [blockr.dplyr website](https://bristolmyerssquibb.github.io/blockr.dplyr/) includes full documentation and the showcase vignette. For information on the broader blockr ecosystem, see [blockr.core](https://bristolmyerssquibb.github.io/blockr.core/) and [blockr.ui](https://bristolmyerssquibb.github.io/blockr.ui/).
