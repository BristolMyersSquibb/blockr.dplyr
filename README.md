
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Data transformation for `blockr.core`

<!-- badges: start -->

[![check](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml)
<!-- badges: end -->

Extending `blockr.core` with blocks for data wrangling, `blockr.dplyr`
wraps several `dplyr` verbs.

## Installation

You can install the development version of blockr.dplyr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.dplyr")
```

## Enhanced Select Block for Visual Column Selection

The `dplyr::select()` block now features an enhanced visual interface
with column cards, search functionality, and detailed column
information:

### Simple select usage (backward compatible)

``` r
library(blockr.dplyr)
# Classic dropdown interface
blockr.core::serve(new_select_block(c("mpg", "cyl"), enhanced = FALSE), data = list(data = mtcars))
```

### Enhanced visual interface (new default)

``` r
library(blockr.dplyr)
# Enhanced interface with column cards and information
blockr.core::serve(new_select_block(c("mpg", "cyl")), data = list(data = mtcars))
```

### Complete data pipeline with enhanced select

``` r
library(blockr.core)
library(blockr.ui)

# Create a dashboard demonstrating the enhanced select capabilities
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

The enhanced select block features: - **Visual column cards** - Show
column type, sample data, and statistics (NA count, unique values) -
**Search functionality** - Type to filter columns by name
(case-insensitive) - **Quick selection controls** - Select All, Select
None, and Invert Selection buttons - **Column information** - Display
data types, sample values, and basic statistics - **Selection
summary** - Clear overview of selected columns with numbering -
**Backward compatibility** - Use `enhanced = FALSE` for classic dropdown
interface

### Testing select functionality

To quickly test the select features:

``` r
library(blockr.dplyr)

# Test the multi-select module directly
run_multi_select_example()

# Test in context with sample data
blockr.core::serve(new_select_block(), data = list(data = mtcars))
```

In the enhanced UI, you can: 1. View column cards with type information
and sample data 2. Use the search box to filter columns by name 3. Click
“Select All” to select all visible columns 4. Click “Select None” to
clear all selections 5. Click “Invert” to toggle selected/unselected
columns 6. Click individual column cards to select/deselect them 7. See
a summary of selected columns at the bottom

The column cards show: - **Column name** with checkbox for selection -
**Data type** and observation count (e.g., “numeric (32 obs)”) -
**Sample values** from the first few non-NA entries - **Statistics**
showing unique value count and NA count

A `dplyr::mutate()` block can be used to add new variables:

``` r
library(blockr.dplyr)
blockr.core::serve(new_mutate_block(), data = list(data = mtcars))
```

## Multi-Dataframe Operations

The package now includes comprehensive support for multi-dataframe
operations with enhanced join and bind blocks, all fully integrated with
the **blockr.ui visual DAG board interface** for interactive pipeline
building.

### Enhanced Join Block with Multi-Column Support

The `dplyr::join()` block has been completely redesigned with advanced
multi-column join capabilities:

``` r
library(blockr.dplyr)
# Enhanced join with natural join (same column names)
blockr.core::serve(
  new_join_block(type = "left_join", by = "name"),
  data = list(x = dplyr::band_members, y = dplyr::band_instruments)
)
```

### Advanced Join Configuration

The enhanced join block supports complex column mappings between
datasets:

``` r
library(blockr.dplyr)

# Create sample datasets with different column names
customers <- data.frame(
  customer_id = 1:3,
  customer_name = c("Alice", "Bob", "Charlie"),
  region = c("East", "West", "North")
)

orders <- data.frame(
  order_id = c(101, 102, 103, 104),
  cust_id = c(1, 2, 1, 3),  # Different column name
  product = c("A", "B", "A", "C"),
  amount = c(100, 200, 150, 300)
)

# Join with different column names using the visual interface
blockr.core::serve(
  new_join_block(type = "left_join"),
  data = list(x = customers, y = orders)
)
```

### Complete Multi-Table Pipeline

Create sophisticated multi-table workflows with the enhanced blocks:

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Sample data
customers <- data.frame(
  id = 1:4,
  name = c("Alice", "Bob", "Charlie", "Diana"),
  region = c("East", "West", "North", "South"),
  tier = c("Gold", "Silver", "Gold", "Bronze")
)

transactions <- data.frame(
  customer_id = c(1, 1, 2, 3, 3, 4),
  product = c("A", "B", "A", "C", "A", "B"),
  amount = c(100, 150, 200, 75, 125, 90),
  month = rep(c("Jan", "Feb", "Mar"), 2)
)

# Create enhanced pipeline with join and analysis
board <- blockr.ui::new_dag_board(
  blocks = list(
    customers_block = new_dataset_block(customers),
    transactions_block = new_dataset_block(transactions),
    join_block = new_join_block(type = "left_join"),  # Will configure id = customer_id in UI
    filter_block = new_filter_block("amount > 100"),
    summary_block = new_summarize_block(
      string = list(
        total_amount = "sum(amount)",
        avg_amount = "mean(amount)",
        transaction_count = "n()"
      ),
      by = c("region", "tier")
    )
  ),
  links = links(
    from = c("customers_block", "transactions_block", "join_block", "filter_block"),
    to = c("join_block", "join_block", "filter_block", "summary_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

### Bind Rows Block for Vertical Combination

Stack datasets vertically with intelligent column matching:

``` r
library(blockr.dplyr)

# Sample datasets with different but overlapping columns
q1_sales <- data.frame(
  region = c("East", "West"),
  product = c("A", "B"),
  revenue = c(1000, 1200),
  quarter = "Q1"
)

q2_sales <- data.frame(
  region = c("North", "South"),
  product = c("A", "C"),
  profit = c(800, 950),  # Different column name
  quarter = "Q2"
)

# Bind rows with ID column to track sources
blockr.core::serve(
  new_bind_rows_block(add_id = TRUE, id_name = "data_source"),
  data = list(x = q1_sales, y = q2_sales)
)
```

### Bind Columns Block for Horizontal Combination

Combine datasets side-by-side with duplicate column handling:

``` r
library(blockr.dplyr)

# Sample datasets with same row count
customer_info <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie"),
  age = c(25, 30, 35)
)

customer_metrics <- data.frame(
  id = 1:3,  # Duplicate column name
  score = c(85, 92, 78),
  tier = c("Gold", "Platinum", "Silver")
)

# Bind columns with custom suffixes for duplicates
blockr.core::serve(
  new_bind_cols_block(suffix = c("_info", "_metrics")),
  data = list(x = customer_info, y = customer_metrics)
)
```

### Complete Multi-Dataframe Workflow

Demonstrate the full power of multi-dataframe operations:

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create comprehensive multi-dataframe pipeline
board <- blockr.ui::new_dag_board(
  blocks = list(
    # Data sources
    current_customers = new_dataset_block(data.frame(
      id = 1:3, name = c("Alice", "Bob", "Charlie"), 
      region = c("East", "West", "North"), active = c(TRUE, TRUE, FALSE)
    )),
    new_customers = new_dataset_block(data.frame(
      id = 4:5, name = c("Diana", "Eve"), 
      region = c("South", "East"), active = c(TRUE, TRUE)
    )),
    customer_details = new_dataset_block(data.frame(
      customer_id = 1:5, tier = c("Gold", "Silver", "Gold", "Bronze", "Silver"),
      signup_date = as.Date(c("2023-01-01", "2023-02-15", "2023-03-10", "2023-12-01", "2023-12-15"))
    )),
    
    # Multi-dataframe operations
    bind_customers = new_bind_rows_block(add_id = TRUE, id_name = "source"),
    join_details = new_join_block(type = "left_join"),  # Configure id = customer_id in UI
    
    # Analysis blocks
    filter_active = new_filter_block("active == TRUE"),
    summary_by_region = new_summarize_block(
      string = list(
        customer_count = "n()",
        gold_customers = "sum(tier == 'Gold')",
        avg_days_since_signup = "mean(as.numeric(Sys.Date() - signup_date))"
      ),
      by = c("region")
    )
  ),
  links = links(
    from = c("current_customers", "new_customers", "bind_customers", "customer_details", "join_details", "filter_active"),
    to = c("bind_customers", "bind_customers", "join_details", "join_details", "filter_active", "summary_by_region")
  )
)

# Launch the comprehensive dashboard
blockr.core::serve(board)
```

The enhanced multi-dataframe blocks feature:

### Visual DAG Board Integration

All multi-dataframe operations work seamlessly with the **blockr.ui
visual DAG board interface**:

- **Drag-and-drop pipeline building**: Visually connect blocks in the
  interactive interface
- **Real-time data flow**: See data flowing through your multi-table
  transformations
- **Interactive block configuration**: Click blocks to configure join
  keys, bind options, etc.
- **Visual feedback**: Immediate visual indication of data structure
  changes
- **Export workflows**: Save and share your multi-dataframe analysis
  pipelines

#### Join Block Features:

- **Advanced join types**: All 6 dplyr join types with clear
  descriptions
- **Multi-column joins**: Support for complex column mappings between
  different datasets  
- **Same-name joins**: Natural joins on common column names
- **Different-name joins**: Visual interface for mapping columns with
  different names
- **Real-time preview**: See join configuration and column mappings
  before applying

#### Bind Rows Block Features:

- **Intelligent column matching**: Automatically aligns columns by name
- **Missing column handling**: Fills missing columns with NA values
- **Source tracking**: Optional ID column to identify which dataset each
  row came from
- **Real-time preview**: Shows column differences and result structure

#### Bind Columns Block Features:

- **Row count validation**: Prevents binding datasets with different row
  counts
- **Duplicate column handling**: Configurable suffixes for columns with
  same names
- **Real-time validation**: Immediate feedback on compatibility and
  conflicts
- **Custom naming**: Full control over how duplicate columns are renamed

## Summarize Example

The `dplyr::summarize()` block supports both simple aggregations and
grouped summaries using the built-in `by` parameter:

### Standalone usage

``` r
library(blockr.dplyr)

# Basic summarization with grouping
blockr.core::serve(
  new_summarize_block(
    string = list(
      mean_mpg = "mean(mpg)",
      max_hp = "max(hp)",
      count = "n()"
    ),
    by = c("cyl", "am")
  ),
  data = list(data = mtcars)
)
```

### Full dashboard example

Create a complete data analysis workflow by connecting multiple blocks:

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard with connected blocks
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    filter_block = new_filter_block("mpg > 20"),
    summary_block = new_summarize_block(
      string = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        count = "n()"
      ),
      by = c("cyl")
    )
  ),
  links = links(
    from = c("data_block", "filter_block"),
    to = c("filter_block", "summary_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

This creates a visual pipeline where: 1. Data is loaded from the mtcars
dataset 2. Rows are filtered to only include cars with mpg \> 20 3.
Results are summarized by cylinder count, calculating average mpg,
average horsepower, and row count

## Mutate Example with Multiple Expressions

The `dplyr::mutate()` block now supports multiple expressions that can
be added/removed dynamically in the UI:

### Full dashboard with mutate block

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard with mutate block showing multiple expressions
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    mutate_block = new_mutate_block(
      string = list(
        mpg_double = "mpg * 2",
        hp_per_100 = "hp / 100",
        power_to_weight = "hp / wt",
        efficiency_score = "mpg / hp * 100"
      )
    )
  ),
  links = links(
    from = c("data_block"),
    to = c("mutate_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

This creates a focused demonstration of the mutate block’s new
capabilities: - **Data loading**: Load the mtcars dataset - **Multiple
mutations**: Create 4 new columns simultaneously in one block

The mutate block UI allows you to: - Add new expressions with the “Add
Expression” button - Remove expressions (keeping at least one) - Edit
column names and expressions with syntax highlighting - See real-time
results as you modify the expressions

## Enhanced Filter Block with Multi-Condition Support

The `dplyr::filter()` block now supports multiple conditions with visual
AND/OR logic builders:

### Simple single condition (classic mode)

``` r
library(blockr.dplyr)
# Single condition interface (backward compatible)
blockr.core::serve(
  new_filter_block("mpg > 20", multi_condition = FALSE),
  data = list(data = mtcars)
)
```

### Multi-condition interface (new default)

``` r
library(blockr.dplyr)
# Multi-condition interface with visual builder
blockr.core::serve(
  new_filter_block("mpg > 20"),
  data = list(data = mtcars)
)
```

### Complete data pipeline with enhanced filter

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard demonstrating the enhanced filter capabilities
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    filter_block = new_filter_block("mpg > 20"),  # Will use multi-condition UI
    mutate_block = new_mutate_block(
      string = list(
        efficiency = "mpg / hp",
        power_ratio = "hp / wt"
      )
    ),
    summary_block = new_summarize_block(
      string = list(
        avg_efficiency = "mean(efficiency)",
        max_power_ratio = "max(power_ratio)",
        count = "n()"
      ),
      by = c("cyl")
    )
  ),
  links = links(
    from = c("data_block", "filter_block", "mutate_block"),
    to = c("filter_block", "mutate_block", "summary_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

The enhanced filter block features: - **Multi-condition interface**:
Add/remove filter conditions with “Add Condition” button - **AND/OR
logic**: Choose between AND (&) or OR (\|) operators between
conditions - **Full autocompletion**: Column names and dplyr functions
suggested as you type - **Real-time validation**: Immediate feedback on
filter syntax and results - **Backward compatibility**: Use
`multi_condition = FALSE` for single-condition mode

### Testing multi-condition functionality

To quickly test the multi-condition features:

``` r
library(blockr.dplyr)

# Test the multi-filter module directly
run_multi_filter_example()

# Test in context with sample data
blockr.core::serve(new_filter_block(), data = list(data = mtcars))
```

In the UI, you can: 1. Start with one condition (e.g., “mpg \> 20”) 2.
Click “Add Condition” to add more (e.g., “cyl == 4”) 3. Choose AND/OR
between conditions using the dropdown 4. Use autocompletion for column
names and functions 5. Remove conditions with the trash icon (keeping at
least one)

## Slice Block for Row Selection

The `slice` block provides comprehensive row selection using all dplyr
slice variants, with support for grouping via the `.by` parameter:

### Select first/last rows

``` r
library(blockr.dplyr)
# Select first 5 rows
pkgload::load_all(); blockr.core::serve(new_slice_block(type = "head", n = 5), data = list(data = mtcars))

# Select last 10% of rows
blockr.core::serve(new_slice_block(type = "tail", prop = 0.1), data = list(data = mtcars))
```

### Select by value

``` r
# Select 3 cars with best fuel economy
blockr.core::serve(
  new_slice_block(type = "max", order_by = "mpg", n = 3),
  data = list(data = mtcars)
)

# Select worst mpg per cylinder group
blockr.core::serve(
  new_slice_block(type = "min", order_by = "mpg", n = 1, by = "cyl"),
  data = list(data = mtcars)
)
```

### Random sampling

``` r
# Random sample of 10 rows
blockr.core::serve(
  new_slice_block(type = "sample", n = 10),
  data = list(data = mtcars)
)

# Weighted sampling with replacement
blockr.core::serve(
  new_slice_block(type = "sample", n = 50, weight_by = "hp", replace = TRUE),
  data = list(data = mtcars)
)
```

### Custom row positions

``` r
# Select specific rows by position
blockr.core::serve(
  new_slice_block(type = "custom", rows = "c(1, 3, 5:10)"),
  data = list(data = mtcars)
)

# Remove first and last rows
blockr.core::serve(
  new_slice_block(type = "custom", rows = "-c(1, n())"),
  data = list(data = mtcars)
)
```

## Rename Block for Column Renaming

The `dplyr::rename()` block provides an intuitive interface for renaming
columns with visual new_name ← old_name mappings:

### Simple rename usage

``` r
library(blockr.dplyr)
# Rename with predefined mappings
blockr.core::serve(
  new_rename_block(list(miles_per_gallon = "mpg", cylinders = "cyl")),
  data = list(data = mtcars)
)
```

### Interactive rename interface

``` r
library(blockr.dplyr)
# Start with empty rename block for interactive column renaming
blockr.core::serve(new_rename_block(), data = list(data = mtcars))
```

### Complete data pipeline with rename

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard with rename in the pipeline
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    rename_block = new_rename_block(list(
      miles_per_gallon = "mpg",
      number_of_cylinders = "cyl",
      gross_horsepower = "hp",
      weight_pounds = "wt"
    )),
    filter_block = new_filter_block("miles_per_gallon > 20"),
    summary_block = new_summarize_block(
      string = list(
        avg_mpg = "mean(miles_per_gallon)",
        avg_hp = "mean(gross_horsepower)",
        count = "n()"
      ),
      by = c("number_of_cylinders")
    )
  ),
  links = links(
    from = c("data_block", "rename_block", "filter_block"),
    to = c("rename_block", "filter_block", "summary_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

The rename block features: - **Visual mapping interface**: Clear
new_name ← old_name with arrow indicators - **Add/remove
functionality**: Dynamic interface to add/remove rename pairs - **Column
validation**: Dropdown selectors ensure old column names exist -
**Duplicate prevention**: Validation prevents renaming the same column
multiple times - **Real-time preview**: See the generated
`dplyr::rename()` code as you work

### Testing rename functionality

To quickly test the rename features:

``` r
library(blockr.dplyr)

# Test the multi-rename module directly
run_multi_rename_example()

# Test in context with sample data
blockr.core::serve(new_rename_block(), data = list(data = mtcars))
```

In the UI, you can: 1. Set new column names in the left text inputs 2.
Select existing columns from dropdowns on the right 3. Add more rename
pairs with “Add Rename” button 4. Remove pairs with trash icons (keeping
at least one) 5. See the arrow (←) indicating the rename direction

## Distinct Block for Removing Duplicates

The `dplyr::distinct()` block allows you to remove duplicate rows from
your data:

### Basic distinct usage

``` r
library(blockr.dplyr)
# Remove duplicates based on specific columns
blockr.core::serve(
  new_distinct_block(columns = c("cyl", "gear"), .keep_all = TRUE),
  data = list(data = mtcars)
)
```

### Interactive distinct interface

``` r
library(blockr.dplyr)
# Start with empty distinct block for interactive duplicate removal
blockr.core::serve(new_distinct_block(), data = list(data = mtcars))
```

### Complete pipeline with distinct

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard with distinct in the pipeline
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    distinct_block = new_distinct_block(
      columns = c("cyl", "gear"),
      .keep_all = TRUE
    ),
    summary_block = new_summarize_block(
      string = list(
        avg_mpg = "mean(mpg)",
        count = "n()"
      ),
      by = c("cyl")
    )
  ),
  links = links(
    from = c("data_block", "distinct_block"),
    to = c("distinct_block", "summary_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

The distinct block features: - **Multi-column selection**: Choose which
columns to check for uniqueness - **Keep all columns option**: Decide
whether to keep all columns or just the selected ones - **Duplicate
count preview**: See how many duplicate rows will be removed before
applying - **Empty selection support**: Leave columns empty to check all
columns for duplicates

### Testing distinct functionality

To quickly test the distinct features:

``` r
library(blockr.dplyr)

# Test the distinct block with example data
run_distinct_example()

# Test in context with your data
blockr.core::serve(new_distinct_block(), data = list(data = mtcars))
```

In the UI, you can: 1. Select columns from the multi-select dropdown to
check for uniqueness 2. Toggle “Keep all columns” to control output
columns 3. See the duplicate count in real-time 4. Leave columns empty
to remove duplicates across all columns

## Enhanced Arrange Block for Multi-Column Sorting

The `dplyr::arrange()` block now supports enhanced multi-column sorting
with individual ASC/DESC controls and visual priority management:

### Simple arrange usage (backward compatible)

``` r
library(blockr.dplyr)
# Classic approach with character vector
blockr.core::serve(
  new_arrange_block(c("mpg", "cyl"), desc = TRUE),
  data = list(data = mtcars)
)
```

### Advanced multi-column interface (new default)

``` r
library(blockr.dplyr)
# Enhanced interface with individual column controls
blockr.core::serve(
  new_arrange_block(list(
    list(column = "mpg", direction = "desc"),
    list(column = "cyl", direction = "asc"),
    list(column = "hp", direction = "desc")
  )),
  data = list(data = mtcars)
)
```

### Complete data pipeline with enhanced arrange

``` r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Create a dashboard demonstrating the enhanced arrange capabilities
board <- blockr.ui::new_dag_board(
  blocks = list(
    data_block = new_dataset_block("mtcars"),
    filter_block = new_filter_block("mpg > 15"),
    arrange_block = new_arrange_block(list(
      list(column = "mpg", direction = "desc"),
      list(column = "cyl", direction = "asc")
    )),
    mutate_block = new_mutate_block(
      string = list(
        efficiency_rank = "row_number()",
        power_class = "case_when(hp > 200 ~ 'High', hp > 100 ~ 'Medium', TRUE ~ 'Low')"
      )
    )
  ),
  links = links(
    from = c("data_block", "filter_block", "arrange_block"),
    to = c("filter_block", "arrange_block", "mutate_block")
  )
)

# Launch the interactive dashboard
blockr.core::serve(board)
```

The enhanced arrange block features: - **Multi-column interface**:
Add/remove sort columns with individual controls - **Individual
ASC/DESC**: Choose ascending or descending for each column
independently - **Visual priority**: Clear 1., 2., 3. indicators showing
sort order priority - **Add/remove controls**: Dynamic interface to
add/remove sort columns - **Backward compatibility**: Accepts both
character vectors and list specifications

### Testing arrange functionality

To quickly test the arrange features:

``` r
library(blockr.dplyr)

# Test the multi-arrange module directly
run_multi_arrange_example()

# Test in context with sample data
blockr.core::serve(new_arrange_block(), data = list(data = mtcars))
```

In the UI, you can: 1. Start with default or provided sort columns 2.
Click “Add Sort Column” to add more sorting criteria 3. Choose ASC/DESC
for each column individually using dropdowns 4. Remove columns with
trash icons (keeping at least one) 5. See priority numbers (1., 2., 3.)
indicating sort order

## Mutate without blockr.ui (core-only)

You can run a `dplyr::mutate()` block with only `blockr.core` and this
package:

``` r
library(blockr.core)
library(blockr.dplyr)

blockr.core::serve(
  new_mutate_block(
    string = list(
      mpg_double = "mpg * 2",
      power_to_weight = "hp / wt"
    )
  ),
  data = list(data = mtcars)
)
```
