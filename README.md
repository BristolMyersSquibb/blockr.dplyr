# blockr.dplyr

<!-- badges: start -->
[![check](https://github.com/BristolMyersSquibb/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/BristolMyersSquibb/blockr.dplyr/actions/workflows/check.yaml)
<!-- badges: end -->

**Data wrangling blocks for blockr.core**

`blockr.dplyr` extends [blockr.core](https://github.com/cynkra/blockr.core) with interactive blocks for data transformation, providing visual interfaces for all major dplyr operations. Build data analysis pipelines by connecting blocks in an intuitive drag-and-drop interface.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("BristolMyersSquibb/blockr.dplyr")
```

## Quick Start

```r
library(blockr.dplyr)

# Create and serve a simple filter block
pkgload::load_all(); blockr.core::serve(new_enhanced_filter_block("Petal.Length > 5"),data = list(data = iris))

pkgload::load_all(); blockr.core::serve(new_enhanced_filter_block("Petal.Length > mean(Petal.Length)"),data = list(data = iris))




pkgload::load_all(); blockr.core::serve(new_enhanced_filter_block(),data = list(data = iris))




blockr.core::serve(
  new_mutate_block("Petal.Length.5 = Petal.Length * 5"),
  data = list(data = iris)
)

```

This launches an interactive web interface where you can:
- Configure filter conditions with a visual builder
- See real-time results as you modify conditions
- Add multiple conditions with AND/OR logic

## Individual Blocks

### Select Block - Column Selection

Choose which columns to keep in your dataset with an intuitive table interface.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_select_block(c("Species", "Sepal.Length")),
  data = list(data = iris)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("iris"),
    select_block = new_select_block(c("Species", "Sepal.Length"))
  ),
  links = c(
    select_link = new_link("data_block", "select_block", "data")
  )
)

blockr.core::serve(board)
```


### Filter Block - Row Filtering

Filter rows based on conditions with support for multiple criteria and AND/OR logic.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_filter_block("mpg > 20"),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars"),
    filter_block = new_filter_block("mpg > 20")
  ),
  links = c(
    filter_link = new_link("data_block", "filter_block", "data")
  )
)

blockr.core::serve(board)
```

### Value Filter Block - Visual Row Filtering

Filter rows by selecting specific values from columns - no R expressions needed! Perfect for users who prefer a point-and-click interface over writing filter conditions.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_value_filter_block(
    conditions = list(
      list(column = "Species", values = c("setosa", "versicolor"), mode = "include")
    )
  ),
  data = list(data = iris)
)
```

**Key Features:**
- 🎯 **Visual column selection** - Choose from dropdown
- 🔍 **Dynamic value selection** - See all unique values, select multiple
- ✅ **Include/Exclude modes** - Toggle between positive and negative filtering
- ➕ **Multiple conditions** - Add conditions with AND/OR logic
- 🚫 **No coding required** - Pure point-and-click interface

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("iris"),
    value_filter_block = new_value_filter_block(
      conditions = list(
        list(column = "Species", values = c("setosa"), mode = "include"),
        list(column = "Sepal.Length", values = c(4.9, 5.0), mode = "exclude")
      )
    )
  ),
  links = c(
    filter_link = new_link("data_block", "value_filter_block", "data")
  )
)

blockr.core::serve(board)
```

### Mutate Block - Add/Modify Columns

Create new columns or modify existing ones with support for multiple expressions.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_mutate_block(list(
    power_to_weight = "hp / wt",
    mpg_doubled = "mpg * 2"
  )),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars"),
    mutate_block = new_mutate_block(list(power_to_weight = "hp / wt"))
  ),
  links = c(
    mutate_link = new_link("data_block", "mutate_block", "data")
  )
)

blockr.core::serve(board)
```


### Arrange Block - Sort Data

Sort data by multiple columns with individual ascending/descending controls.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_arrange_block(list(
    list(column = "mpg", direction = "desc"),
    list(column = "cyl", direction = "asc")
  )),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars"),
    arrange_block = new_arrange_block(c("mpg", "cyl"))
  ),
  links = c(
    arrange_link = new_link("data_block", "arrange_block", "data")
  )
)

blockr.core::serve(board)
```

### Summarize Block - Aggregate Data

Calculate summary statistics with support for grouping and multiple expressions.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_summarize_block(
    string = list(
      avg_mpg = "mean(mpg)",
      max_hp = "max(hp)",
      count = "n()"
    ),
    by = c("cyl")
  ),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars"),
    summary_block = new_summarize_block(
      string = list(avg_mpg = "mean(mpg)", count = "n()"),
      by = c("cyl")
    )
  ),
  links = c(
    summary_link = new_link("data_block", "summary_block", "data")
  )
)

blockr.core::serve(board)
```

### Slice Block - Row Selection

Select rows by position, value, or random sampling with comprehensive slice variants.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_slice_block(type = "head", n = 5),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars"),
    slice_block = new_slice_block(type = "max", order_by = "mpg", n = 3)
  ),
  links = c(
    slice_link = new_link("data_block", "slice_block", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Multiple slice types**: head/tail, min/max, sample, custom positions
- **Grouping support**: Use `.by` parameter for group-wise operations
- **Flexible selection**: By position, proportion, or value-based ordering
- **Random sampling**: With optional weighting and replacement

### Rename Block - Column Renaming

Rename columns with an intuitive visual mapping interface.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_rename_block(list(
    miles_per_gallon = "mpg",
    cylinders = "cyl"
  )),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars"),
    rename_block = new_rename_block(list(miles_per_gallon = "mpg"))
  ),
  links = c(
    rename_link = new_link("data_block", "rename_block", "data")
  )
)

blockr.core::serve(board)
```

**Features:**
- **Visual mapping**: Clear new_name ← old_name with arrow indicators
- **Add/remove functionality**: Dynamic interface for rename pairs
- **Column validation**: Dropdown selectors ensure valid column names
- **Duplicate prevention**: Validates against renaming same column twice

### Distinct Block - Remove Duplicates

Remove duplicate rows based on specified columns.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_distinct_block(columns = c("cyl", "gear"), .keep_all = TRUE),
  data = list(data = mtcars)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars"),
    distinct_block = new_distinct_block(columns = c("cyl", "gear"))
  ),
  links = c(
    distinct_link = new_link("data_block", "distinct_block", "data")
  )
)

blockr.core::serve(board)
```

### Join Block - Combine Datasets

Join two datasets with support for all join types and multi-column keys.

**Simple usage:**
```r
library(blockr.dplyr)
blockr.core::serve(
  new_join_block(type = "left_join", by = "name"),
  data = list(
    x = dplyr::band_members,
    y = dplyr::band_instruments
  )
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    members_block = new_dataset_block("band_members"),
    instruments_block = new_dataset_block("band_instruments"),
    join_block = new_join_block(type = "left_join")
  ),
  links = c(
    x_link = new_link("members_block", "join_block", "x"),
    y_link = new_link("instruments_block", "join_block", "y")
  )
)

blockr.core::serve(board)
```

### Bind Rows Block - Combine Vertically

Stack datasets vertically with intelligent column matching.

**Simple usage:**
```r
library(blockr.dplyr)

# Sample datasets
q1_data <- data.frame(region = c("East", "West"), sales = c(100, 200))
q2_data <- data.frame(region = c("North", "South"), sales = c(150, 250))

blockr.core::serve(
  new_bind_rows_block(add_id = TRUE, id_name = "quarter"),
  data = list(x = q1_data, y = q2_data)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    q1_block = new_dataset_block("q1_data"),
    q2_block = new_dataset_block("q2_data"),
    bind_block = new_bind_rows_block(add_id = TRUE)
  ),
  links = c(
    x_link = new_link("q1_block", "bind_block", "x"),
    y_link = new_link("q2_block", "bind_block", "y")
  )
)

blockr.core::serve(board)
```

### Bind Columns Block - Combine Horizontally

Combine datasets side-by-side with duplicate column handling.

**Simple usage:**
```r
library(blockr.dplyr)

# Sample datasets
info <- data.frame(id = 1:3, name = c("A", "B", "C"))
metrics <- data.frame(score = c(85, 92, 78), tier = c("Gold", "Silver", "Bronze"))

blockr.core::serve(
  new_bind_cols_block(),
  data = list(x = info, y = metrics)
)
```

**DAG board integration:**
```r
library(blockr.core)
library(blockr.ui)

board <- blockr.ui::new_dag_board(
  blocks = c(
    info_block = new_dataset_block("info"),
    metrics_block = new_dataset_block("metrics"),
    bind_block = new_bind_cols_block()
  ),
  links = c(
    x_link = new_link("info_block", "bind_block", "x"),
    y_link = new_link("metrics_block", "bind_block", "y")
  )
)

blockr.core::serve(board)
```


## Comprehensive Example

Here's a realistic data analysis pipeline demonstrating multiple blocks working together, organized with stacks for better visual management of complex workflows:

```r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Comprehensive analysis pipeline using built-in R datasets
analysis_board <- blockr.ui::new_dag_board(
  blocks = c(
    # Data sources - using datasets from R's datasets package
    bod_data = new_dataset_block("BOD", package = "datasets"),
    chick_data = new_dataset_block("ChickWeight", package = "datasets"),

    # Prepare BOD data
    bod_select = new_select_block(c("Time", "demand")),
    bod_filter = new_filter_block("demand > 10"),

    # Prepare ChickWeight data
    chick_select = new_select_block(c("Time", "weight", "Diet")),
    chick_filter = new_filter_block("Time <= 10"),

    # Join on shared Time column
    joined_data = new_join_block(type = "inner_join", by = c("Time" = "Time")),

    # Remove any duplicate time entries
    distinct_data = new_distinct_block(columns = c("Time")),

    # Analysis with mtcars
    cars_data = new_dataset_block("mtcars", package = "datasets"),

    # Filter high-performance cars
    fast_cars = new_filter_block("hp > 150"),

    # Add calculated fields
    cars_enhanced = new_mutate_block(list(
      performance = "hp / wt",
      efficiency = "mpg / cyl",
      car_type = "dplyr::case_when(cyl <= 4 ~ 'Economy', cyl <= 6 ~ 'Standard', TRUE ~ 'Performance')"
    )),

    # Sort by multiple columns
    cars_sorted = new_arrange_block(list(
      list(column = "car_type", direction = "asc"),
      list(column = "mpg", direction = "desc")
    )),

    # Summarize by car type
    summary_stats = new_summarize_block(
      string = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        avg_performance = "mean(performance)",
        count = "n()"
      ),
      by = c("car_type", "cyl")
    ),

    # Get top results
    top_cars = new_slice_block(type = "head", n = 5),

    # Analysis with iris dataset
    iris_data = new_dataset_block("iris", package = "datasets"),

    # Rename columns for clarity
    iris_renamed = new_rename_block(renames = list(
      sepal_length = "Sepal.Length",
      sepal_width = "Sepal.Width",
      petal_length = "Petal.Length",
      petal_width = "Petal.Width"
    )),

    # Group by species and summarize
    iris_summary = new_summarize_block(
      string = list(
        avg_sepal_length = "mean(sepal_length)",
        avg_petal_length = "mean(petal_length)",
        count = "n()"
      ),
      by = c("Species")
    ),

    # Combine car and iris summaries
    combined_summaries = new_bind_rows_block(id_column = "dataset"),

    # Get matching subset of original cars for column binding
    cars_original_subset = new_slice_block(type = "head", n = 13),

    # Add metadata to cars data
    cars_with_ids = new_bind_cols_block(
      suffix = c("_enhanced", "_original")
    )
  ),
  links = c(
    # BOD pipeline
    bod_link1 = new_link("bod_data", "bod_select", "data"),
    bod_link2 = new_link("bod_select", "bod_filter", "data"),

    # ChickWeight pipeline
    chick_link1 = new_link("chick_data", "chick_select", "data"),
    chick_link2 = new_link("chick_select", "chick_filter", "data"),

    # Join BOD and ChickWeight on Time
    join_bod = new_link("bod_filter", "joined_data", "x"),
    join_chick = new_link("chick_filter", "joined_data", "y"),

    # Remove duplicates from joined data
    distinct_link = new_link("joined_data", "distinct_data", "data"),

    # Cars analysis pipeline
    filter_cars = new_link("cars_data", "fast_cars", "data"),
    enhance_cars = new_link("fast_cars", "cars_enhanced", "data"),
    sort_cars = new_link("cars_enhanced", "cars_sorted", "data"),
    summarize_cars = new_link("cars_sorted", "summary_stats", "data"),
    slice_top = new_link("summary_stats", "top_cars", "data"),

    # Iris pipeline
    iris_rename_link = new_link("iris_data", "iris_renamed", "data"),
    iris_summary_link = new_link("iris_renamed", "iris_summary", "data"),

    # Combine summaries
    bind_cars_summary = new_link("summary_stats", "combined_summaries", "x"),
    bind_iris_summary = new_link("iris_summary", "combined_summaries", "y"),

    # Prepare matching subset for bind_cols
    cars_subset_link = new_link("cars_data", "cars_original_subset", "data"),

    # Add metadata to cars
    cars_bind_main = new_link("cars_enhanced", "cars_with_ids", "x"),
    cars_bind_meta = new_link("cars_original_subset", "cars_with_ids", "y")
  ),
  stacks = blockr.core::stacks(
    # Organize blocks into logical groups for better pipeline management
    data_sources = blockr.core::new_stack(
      c("bod_data", "chick_data", "cars_data", "iris_data"),
      "Data Sources"
    ),
    bod_analysis = blockr.core::new_stack(
      c("bod_select", "bod_filter"),
      "BOD Analysis"
    ),
    chick_analysis = blockr.core::new_stack(
      c("chick_select", "chick_filter"),
      "ChickWeight Analysis"
    ),
    cars_analysis = blockr.core::new_stack(
      c("fast_cars", "cars_enhanced", "cars_sorted", "summary_stats", "top_cars"),
      "Cars Analysis Pipeline"
    ),
    iris_analysis = blockr.core::new_stack(
      c("iris_renamed", "iris_summary"),
      "Iris Analysis"
    ),
    data_combination = blockr.core::new_stack(
      c("joined_data", "distinct_data", "combined_summaries", "cars_original_subset", "cars_with_ids"),
      "Data Combination Operations"
    )
  )
)

# Launch the comprehensive analysis dashboard
blockr.core::serve(analysis_board)
```

## Learn More

- [blockr.core documentation](https://github.com/cynkra/blockr.core)
- [blockr.ui documentation](https://github.com/cynkra/blockr.ui)
- [dplyr documentation](https://dplyr.tidyverse.org/)