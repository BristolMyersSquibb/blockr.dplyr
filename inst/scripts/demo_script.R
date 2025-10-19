# ==============================================================================
# BLOCKR EXTENSIONS TECHNICAL DEMO
# ==============================================================================
# Comprehensive demonstration of blockr.dplyr and blockr.ggplot capabilities
# Run sections individually by copying and pasting code blocks

# Load required libraries
library(blockr.core)
library(blockr.dplyr)
library(blockr.ggplot)
library(blockr.ui)

# ==============================================================================
# DEMO 1: COMPREHENSIVE PIPELINE ARCHITECTURE
# ==============================================================================
# Demonstrates the scale and complexity possible with 20+ interconnected blocks
# Features: stacks organization, multi-table joins, complex transformations, visualizations
# Shows BOTH blockr.dplyr AND blockr.ggplot working together

analysis_board <- blockr.ui::new_dag_board(
  blocks = c(
    # Data sources - using datasets from R's datasets package
    bod_data = new_dataset_block("BOD", package = "datasets"),
    chick_data = new_dataset_block("ChickWeight", package = "datasets"),

    # Prepare BOD data
    bod_select = new_select_block(c("Time", "demand")),
    bod_filter = new_filter_expr_block("demand > 10"),

    # Prepare ChickWeight data
    chick_select = new_select_block(c("Time", "weight", "Diet")),
    chick_filter = new_filter_expr_block("Time <= 10"),

    # Join on shared Time column
    joined_data = new_join_block(type = "inner_join", by = c("Time" = "Time")),

    # Remove any duplicate time entries
    distinct_data = new_distinct_block(columns = c("Time")),

    # Analysis with mtcars
    cars_data = new_dataset_block("mtcars", package = "datasets"),

    # Filter high-performance cars
    fast_cars = new_filter_expr_block("hp > 150"),

    # Add calculated fields - MULTI-EXPRESSION CAPABILITY
    cars_enhanced = new_mutate_block(list(
      performance = "hp / wt",
      efficiency = "mpg / cyl",
      car_type = "dplyr::case_when(cyl <= 4 ~ 'Economy', cyl <= 6 ~ 'Standard', TRUE ~ 'Performance')"
    )),

    # Sort by multiple columns - MULTI-COLUMN SORTING
    cars_sorted = new_arrange_block(list(
      list(column = "car_type", direction = "asc"),
      list(column = "mpg", direction = "desc")
    )),

    # Summarize by car type - MULTI-EXPRESSION + GROUPING
    summary_stats = new_summarize_block(
      string = list(
        avg_mpg = "mean(mpg)",
        avg_hp = "mean(hp)",
        avg_performance = "mean(performance)",
        count = "dplyr::n()"
      ),
      by = c("car_type", "cyl")
    ),

    # Get top results
    top_cars = new_slice_block(type = "head", n = 5),

    # Analysis with iris dataset
    iris_data = new_dataset_block("iris", package = "datasets"),

    # Rename columns for clarity - VISUAL MAPPING INTERFACE
    iris_renamed = new_rename_block(
      renames = list(
        sepal_length = "Sepal.Length",
        sepal_width = "Sepal.Width",
        petal_length = "Petal.Length",
        petal_width = "Petal.Width"
      )
    ),

    # Group by species and summarize
    iris_summary = new_summarize_block(
      string = list(
        avg_sepal_length = "mean(sepal_length)",
        avg_petal_length = "mean(petal_length)",
        count = "dplyr::n()"
      ),
      by = c("Species")
    ),

    # Combine car and iris summaries - VERTICAL BINDING
    combined_summaries = new_bind_rows_block(id_column = "dataset"),

    # Get matching subset of original cars for column binding
    cars_original_subset = new_slice_block(type = "head", n = 13),

    # Add metadata to cars data - HORIZONTAL BINDING
    cars_with_ids = new_bind_cols_block(
      suffix = c("_enhanced", "_original")
    ),

    # VISUALIZATION BLOCKS - blockr.ggplot integration
    # Visualize car performance analysis
    performance_scatter = new_scatter_plot_block(
      x = "performance",
      y = "efficiency",
      color = "car_type",
      size = "hp",
      add_smooth = TRUE
    ),

    # Bar chart of summary statistics
    summary_bar = new_bar_chart_block(
      x = "car_type",
      y = "avg_mpg",
      fill = "cyl"
    ),

    # Box plot of iris measurements
    iris_boxplot = new_boxplot_block(
      x = "Species",
      y = "sepal_length",
      fill = "Species"
    ),

    # Heatmap of joined time-series data
    time_heatmap = new_heatmap_block(
      x = "Time",
      y = "Diet",
      fill = "weight",
      show_values = TRUE
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
    cars_bind_meta = new_link("cars_original_subset", "cars_with_ids", "y"),

    # Visualization links
    perf_scatter_link = new_link("cars_sorted", "performance_scatter", "data"),
    summary_bar_link = new_link("summary_stats", "summary_bar", "data"),
    iris_box_link = new_link("iris_renamed", "iris_boxplot", "data"),
    heatmap_link = new_link("joined_data", "time_heatmap", "data")
  ),
  stacks = blockr.core::stacks(
    # Organize blocks into logical groups for better pipeline management
    bod_analysis = blockr.core::new_stack(
      c("bod_data", "bod_select", "bod_filter"),
      "BOD Analysis"
    ),
    chick_analysis = blockr.core::new_stack(
      c("chick_data", "chick_select", "chick_filter", "time_heatmap"),
      "ChickWeight Analysis"
    ),
    cars_analysis = blockr.core::new_stack(
      c(
        "cars_data",
        "fast_cars",
        "cars_enhanced",
        "cars_sorted",
        "summary_stats",
        "top_cars",
        "performance_scatter",
        "summary_bar"
      ),
      "Cars Analysis Pipeline"
    ),
    iris_analysis = blockr.core::new_stack(
      c("iris_data", "iris_renamed", "iris_summary", "iris_boxplot"),
      "Iris Analysis"
    ),
    data_combination = blockr.core::new_stack(
      c(
        "joined_data",
        "distinct_data",
        "combined_summaries",
        "cars_original_subset",
        "cars_with_ids"
      ),
      "Data Combination Operations"
    )
  )
)

# Launch the comprehensive analysis dashboard
blockr.core::serve(analysis_board)

# ==============================================================================
# DEMO 2: MULTI-EXPRESSION MUTATE BLOCK
# ==============================================================================
# Add multiple calculated fields simultaneously with ACE autocompletion
# Features: multiple expressions, dplyr functions, complex calculations

blockr.core::serve(
  new_mutate_block(list(
    power_to_weight = "hp / wt",
    efficiency = "mpg / cyl",
    performance_class = "dplyr::case_when(hp > 200 ~ 'High', hp > 100 ~ 'Medium', TRUE ~ 'Low')",
    is_v8 = "cyl == 8",
    mpg_squared = "mpg^2"
  )),
  data = list(data = mtcars)
)

# ==============================================================================
# DEMO 3: MULTI-CONDITION FILTER BLOCK
# ==============================================================================
# Complex filtering with AND/OR logic trees
# Features: condition builder interface, multiple criteria, logical operators

cat("=== MULTI-CONDITION FILTER BLOCK ===\n")
cat("Complex filtering with AND/OR logic trees\n\n")

# Note: This shows the interface - actual conditions are configured in UI
blockr.core::serve(
  new_filter_expr_block("mpg > 20 & hp > 100"), # Starting condition
  data = list(data = mtcars)
)

# ==============================================================================
# DEMO 4: VISUAL JOIN CONFIGURATION
# ==============================================================================
# Multi-column key mapping with all join types
# Features: visual column mapping, all join types, conflict resolution

cat("=== VISUAL JOIN CONFIGURATION ===\n")
cat("Multi-column key mapping with all join types\n\n")

blockr.core::serve(
  new_join_block(type = "left_join", by = "name"),
  data = list(
    x = dplyr::band_members,
    y = dplyr::band_instruments
  )
)

# ==============================================================================
# DEMO 5: TABLE-BASED SELECT INTERFACE
# ==============================================================================
# Space-efficient column selection with type information
# Features: DataTable interface, type badges, sortable columns

cat("=== TABLE-BASED SELECT INTERFACE ===\n")
cat("Space-efficient column selection with type information\n\n")

blockr.core::serve(
  new_select_block(c("Species", "Sepal.Length", "Petal.Length")),
  data = list(data = iris)
)

# ==============================================================================
# DEMO 6: UNIFIED .BY PARAMETER SUPPORT
# ==============================================================================
# Integrated grouping across mutate, summarize, and slice blocks
# Features: consistent grouping interface, multi-column grouping

grouped_board <- blockr.ui::new_dag_board(
  blocks = c(
    data_block = new_dataset_block("mtcars"),
    grouped_mutate = new_mutate_block(
      list(avg_mpg_by_cyl = "mean(mpg)"),
      by = c("cyl")
    ),
    grouped_summary = new_summarize_block(
      string = list(
        mean_hp = "mean(hp)",
        count = "dplyr::n()",
        max_mpg = "max(mpg)"
      ),
      by = c("cyl", "gear")
    )
  ),
  links = c(
    mutate_link = new_link("data_block", "grouped_mutate", "data"),
    summary_link = new_link("grouped_mutate", "grouped_summary", "data")
  )
)

blockr.core::serve(grouped_board)

# ==============================================================================
# DEMO 7: BIND OPERATIONS WITH CONFLICT RESOLUTION
# ==============================================================================
# Vertical and horizontal data combination with validation
# Features: conflict resolution, validation messages, flexible binding

# Create sample datasets with intentional conflicts
q1_data <- data.frame(region = c("East", "West"), sales = c(100, 200), id = 1:2)
q2_data <- data.frame(
  region = c("North", "South"),
  sales = c(150, 250),
  id = 3:4
)

# Bind rows example
bind_board <- blockr.ui::new_dag_board(
  blocks = c(
    q1_block = new_dataset_block("q1_data"),
    q2_block = new_dataset_block("q2_data"),
    bind_rows_block = new_bind_rows_block(add_id = TRUE, id_name = "quarter")
  ),
  links = c(
    x_link = new_link("q1_block", "bind_rows_block", "x"),
    y_link = new_link("q2_block", "bind_rows_block", "y")
  )
)

blockr.core::serve(bind_board)
