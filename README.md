# blockr.dplyr

<!-- badges: start -->
[![check](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml/badge.svg)](https://github.com/cynkra/blockr.dplyr/actions/workflows/check.yaml)
<!-- badges: end -->

**Data wrangling blocks for blockr.core**

`blockr.dplyr` extends [blockr.core](https://github.com/cynkra/blockr.core) with interactive blocks for data transformation, providing visual interfaces for all major dplyr operations. Build data analysis pipelines by connecting blocks in an intuitive drag-and-drop interface.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("cynkra/blockr.dplyr")
```

## Quick Start

```r
library(blockr.dplyr)

# Create and serve a simple filter block
blockr.core::serve(
  new_filter_block("mpg > 20"),
  data = list(data = mtcars)
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

**Features:**
- **Table interface** (default): Compact view with colored type tags, sortable columns
- **Card interface**: Detailed column information with statistics  
- **Search functionality**: Filter columns by name
- **Selected columns on top**: Easy management with numbered badges

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

**Features:**
- **Multi-condition interface**: Add/remove conditions dynamically
- **AND/OR logic**: Choose operators between conditions
- **Autocompletion**: Column names and dplyr functions
- **Real-time validation**: Immediate syntax feedback

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

**Features:**
- **Multi-expression support**: Add/remove expressions with dynamic UI
- **Syntax highlighting**: ACE editor with R syntax highlighting
- **Autocompletion**: Column names and dplyr function suggestions
- **Real-time preview**: See results as you type

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

**Features:**
- **Multi-column sorting**: Priority-based sort order with visual indicators
- **Individual direction control**: ASC/DESC for each column independently
- **Add/remove columns**: Dynamic interface for sort criteria
- **Visual priority**: Clear 1., 2., 3. numbering shows sort order

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

**Features:**
- **Multiple expressions**: Add/remove summary expressions dynamically
- **Grouping support**: Built-in `.by` parameter for group-wise operations
- **dplyr functions**: Access to `n()`, `mean()`, `sum()`, etc.
- **Flexible output**: Name your summary columns

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

**Features:**
- **Multi-column selection**: Choose which columns define uniqueness
- **Keep all option**: Control whether to keep all columns or just selected ones
- **Duplicate count preview**: See impact before applying
- **Flexible usage**: Leave empty to check all columns

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

**Features:**
- **All join types**: left, inner, right, full, semi, anti joins
- **Multi-column joins**: Support for complex column mappings
- **Visual interface**: Configure join keys with dropdown selectors
- **Natural joins**: Automatic matching on same-name columns

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

**Features:**
- **Intelligent matching**: Automatically aligns columns by name
- **Missing column handling**: Fills non-matching columns with NA
- **Source tracking**: Optional ID column to identify row origins
- **Real-time preview**: Shows column alignment before binding

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

**Features:**
- **Row count validation**: Prevents binding incompatible datasets
- **Duplicate handling**: Automatic column name disambiguation
- **Real-time validation**: Immediate compatibility feedback
- **Simple operation**: Straightforward side-by-side combination

## Comprehensive Example

Here's a realistic data analysis pipeline demonstrating multiple blocks working together:

```r
library(blockr.core)
library(blockr.dplyr)
library(blockr.ui)

# Sample business data
sales_data <- data.frame(
  customer_id = c(1, 2, 3, 1, 2),
  product = c("A", "B", "A", "C", "A"),
  amount = c(100, 200, 150, 75, 125),
  date = as.Date(c("2024-01-15", "2024-01-20", "2024-02-10", "2024-02-15", "2024-03-01"))
)

customer_data <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie"),
  region = c("East", "West", "North"),
  tier = c("Gold", "Silver", "Gold")
)

# Comprehensive analysis pipeline
analysis_board <- blockr.ui::new_dag_board(
  blocks = c(
    # Data sources
    sales_raw = new_dataset_block("sales_data"),
    customers_raw = new_dataset_block("customer_data"),
    
    # Data preparation
    sales_clean = new_filter_block("amount > 50"),
    customers_select = new_select_block(c("id", "name", "region", "tier")),
    
    # Join operations  
    sales_enhanced = new_join_block(type = "left_join"),
    
    # Data transformation
    sales_mutated = new_mutate_block(list(
      month = "format(date, '%Y-%m')",
      amount_category = "case_when(amount > 150 ~ 'High', amount > 100 ~ 'Medium', TRUE ~ 'Low')"
    )),
    
    # Remove any duplicates
    sales_distinct = new_distinct_block(columns = c("customer_id", "product", "date")),
    
    # Sort by date and amount
    sales_arranged = new_arrange_block(list(
      list(column = "date", direction = "desc"),
      list(column = "amount", direction = "desc")
    )),
    
    # Analysis summaries
    monthly_summary = new_summarize_block(
      string = list(
        total_sales = "sum(amount)",
        avg_order = "mean(amount)", 
        customer_count = "n_distinct(customer_id)",
        transaction_count = "n()"
      ),
      by = c("month", "region")
    ),
    
    # Top performers
    top_results = new_slice_block(type = "head", n = 10)
  ),
  links = c(
    # Data cleaning (single-input blocks use "data")
    sales_filter = new_link("sales_raw", "sales_clean", "data"),
    customer_select = new_link("customers_raw", "customers_select", "data"),
    
    # Join operation (multi-input block uses "x" and "y") 
    join_sales = new_link("sales_clean", "sales_enhanced", "x"),
    join_customers = new_link("customers_select", "sales_enhanced", "y"),
    
    # Transformation pipeline (single-input blocks use "data")
    enhance = new_link("sales_enhanced", "sales_mutated", "data"),
    distinct = new_link("sales_mutated", "sales_distinct", "data"),
    arrange = new_link("sales_distinct", "sales_arranged", "data"),
    
    # Analysis (single-input blocks use "data")
    summarize = new_link("sales_arranged", "monthly_summary", "data"),
    top_slice = new_link("monthly_summary", "top_results", "data")
  )
)

# Launch the comprehensive analysis dashboard
blockr.core::serve(analysis_board)
```

This pipeline demonstrates:

- **Multi-table integration**: Joining sales and customer data
- **Data cleaning**: Filtering out low-value transactions  
- **Feature engineering**: Adding calculated columns for analysis
- **Data quality**: Removing duplicates and sorting results
- **Business analysis**: Monthly summaries by region with key metrics
- **Result focusing**: Extracting top 10 results for dashboard display

## Features

- ✅ **Complete dplyr coverage**: All major data transformation operations
- ✅ **Visual interfaces**: Intuitive UIs for complex operations  
- ✅ **Multi-expression support**: Add/remove expressions dynamically
- ✅ **Advanced joins**: Multi-column joins with visual configuration
- ✅ **Interactive pipelines**: Drag-and-drop workflow building
- ✅ **Real-time feedback**: See results as you configure blocks
- ✅ **Professional UIs**: Clean, modern interfaces with validation
- ✅ **Full integration**: Works seamlessly with blockr.core and blockr.ui

## Learn More

- [blockr.core documentation](https://github.com/cynkra/blockr.core)
- [blockr.ui documentation](https://github.com/cynkra/blockr.ui) 
- [dplyr documentation](https://dplyr.tidyverse.org/)