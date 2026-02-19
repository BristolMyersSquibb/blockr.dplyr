#' @importFrom blockr.core register_blocks
register_dplyr_blocks <- function() {
  register_blocks(
    c(
      "new_select_block",
      "new_join_block",
      "new_arrange_block",
      "new_mutate_expr_block",
      "new_summarize_block",
      "new_summarize_expr_block",
      "new_filter_block",
      "new_filter_expr_block",
      "new_bind_rows_block",
      "new_bind_cols_block",
      "new_rename_block",
      "new_slice_block",
      "new_pivot_longer_block",
      "new_pivot_wider_block",
      "new_unite_block",
      "new_separate_block"
    ),
    name = c(
      "Select Columns",
      "Lookup & Merge",
      "Sort Rows",
      "Calculate Columns",
      "Aggregate Data",
      "Aggregate (Advanced)",
      "Filter Rows",
      "Filter (Advanced)",
      "Stack Tables",
      "Combine Side-by-Side",
      "Rename Columns",
      "Pick Rows",
      "Unpivot (Wide to Long)",
      "Pivot (Long to Wide)",
      "Combine Columns",
      "Split Column"
    ),
    description = c(
      "Choose which columns to keep or remove. Can also remove duplicate rows. (dplyr: select, distinct)",
      "Combine two tables by matching values in a shared column. (dplyr: left_join, inner_join, etc.)",
      "Sort your data by one or more columns, ascending or descending. (dplyr: arrange)",
      "Create new columns or modify existing ones using R formulas. (dplyr: mutate)",
      "Calculate totals, averages, counts, and other statistics. No coding required. (dplyr: summarize)",
      "Create summary statistics using R expressions. Supports grouping. (dplyr: summarize)",
      "Keep only rows that match selected values. No coding required. (dplyr: filter)",
      "Filter rows using R expressions with AND/OR logic. (dplyr: filter)",
      "Stack tables vertically by matching column names. (dplyr: bind_rows)",
      "Place tables next to each other horizontally. Both tables must have the same number of rows. (dplyr: bind_cols)",
      "Change column names to make them more readable. (dplyr: rename)",
      "Select specific rows: first N, last N, random sample, or rows with min/max values. (dplyr: slice)",
      "Convert columns into rows. Useful when column headers contain data values. (tidyr: pivot_longer)",
      "Convert rows into columns. Turn categories into separate columns. (tidyr: pivot_wider)",
      "Merge multiple columns into one by pasting values together. (tidyr: unite)",
      "Split one column into multiple columns using a delimiter. (tidyr: separate)"
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "layout-sidebar-inset-reverse", # select block
      "bezier2", # join block
      "sort-down-alt", # arrange block
      "pencil-square", # mutate_expr block
      "sliders", # summarize block (simple)
      "calculator", # summarize_expr block
      "filter", # filter block (simple)
      "code-slash", # filter_expr block
      "arrows-collapse", # bind_rows block (vertical arrows = rows stacking)
      "arrows-collapse-vertical", # bind_cols block (horizontal arrows = columns joining)
      "tags", # rename block
      "scissors", # slice block
      "arrow-down-up", # pivot_longer block
      "arrow-left-right", # pivot_wider block
      "link", # unite block
      "distribute-horizontal" # separate block
    ),
    arguments = list(
      # select_block:
      structure(
        c(
          columns = "Array of column name strings to select",
          exclude = "Boolean. If true, exclude the listed columns instead of including them",
          distinct = "Boolean. If true, keep only distinct rows after selection"
        ),
        examples = list(columns = list("mpg", "cyl", "hp"), exclude = FALSE, distinct = FALSE)
      ),
      # join_block:
      structure(
        c(
          type = 'Join type: "left_join", "inner_join", "right_join", "full_join", "semi_join", "anti_join"',
          by = "Array of column names to join on"
        ),
        examples = list(type = "left_join", by = list("id"))
      ),
      # arrange_block:
      structure(
        c(
          columns = 'Array of objects, each with "column" (string) and "direction" ("asc" or "desc")'
        ),
        examples = list(columns = list(list(column = "mpg", direction = "desc")))
      ),
      # mutate_expr_block:
      structure(
        c(
          exprs = "Object where keys are new column names and values are R expression strings",
          by = "Array of column names for grouping (optional)"
        ),
        examples = list(exprs = list(mpg_squared = "mpg^2", hp_per_cyl = "hp / cyl"), by = list()),
        prompt = paste(
          "Expressions are R code strings.",
          "For regex in R, use double backslashes: gsub('\\\\(', '', x) not gsub('\\(', '', x).",
          "Column names with spaces or special characters must be backtick-quoted: `PBO N = 334`.",
          "Check option blockr.dplyr.summary_functions for domain-specific helper functions that may be available."
        )
      ),
      # summarize_block:
      structure(
        c(
          summaries = paste0(
            'Object where keys are output column names and values are objects with ',
            '"func" (function name, e.g. "mean", "stats::median", "sum", "min", "max", ',
            '"dplyr::n", "dplyr::n_distinct") and "col" (input column name, empty string for dplyr::n)'
          ),
          by = "Array of column names for grouping (optional)"
        ),
        examples = list(
          summaries = list(
            avg_mpg = list(func = "mean", col = "mpg"),
            count = list(func = "dplyr::n", col = "")
          ),
          by = list("cyl")
        )
      ),
      # summarize_expr_block:
      structure(
        c(
          exprs = "Object where keys are output column names and values are R expression strings. Use across() to apply a function to multiple columns at once, e.g. across(where(is.numeric), mean)",
          by = "Array of column names for grouping (optional)",
          unpack = "Boolean, set true when using across() so its data frame result is unpacked into separate columns (default false). Not needed for scalar expressions like mean(mpg)"
        ),
        examples = list(
          exprs = list(avg_mpg = "mean(mpg)", n = "dplyr::n()"),
          by = list("cyl"),
          unpack = FALSE
        ),
        prompt = "When applying a function across multiple columns, use across(). Set unpack = true with across() so the data frame result is unpacked into separate columns."
      ),
      # filter_block:
      structure(
        c(
          conditions = paste0(
            "List of filter conditions, each with: ",
            "column (string), values (array of strings, even for numbers), ",
            "mode (\"include\" or \"exclude\"), ",
            "operator (\"|\" or \"&\", how this condition connects to the previous one, default \"&\")"
          ),
          preserve_order = "Boolean, whether to preserve selection order"
        ),
        examples = list(
          conditions = list(
            list(column = "Species", values = list("setosa"), mode = "include")
          ),
          preserve_order = FALSE
        ),
        prompt = "The values array must always contain strings, even for numeric columns (e.g. [\"4\"] not [4]). The operator field connects a condition to the previous one."
      ),
      # filter_expr_block:
      structure(
        c(
          exprs = "R expression string for filtering rows"
        ),
        examples = list(exprs = "mpg > 20 & cyl == 4"),
        prompt = paste(
          "The expression is an R expression string, NOT regex.",
          "For regex in R, use double backslashes: gsub('\\\\(', '', x) not gsub('\\(', '', x).",
          "Column names with spaces or special characters must be backtick-quoted: `PBO N = 334`.",
          "Check option blockr.dplyr.summary_functions for domain-specific helper functions that may be available."
        )
      ),
      # bind_rows_block:
      structure(
        c(
          id_name = 'String, optional column name to identify source table (empty for none)'
        ),
        examples = list(id_name = "source")
      ),
      # bind_cols_block:
      NULL,
      # rename_block:
      structure(
        c(
          renames = "Object where keys are new column names and values are current column names"
        ),
        examples = list(renames = list(miles_per_gallon = "mpg", cylinders = "cyl"))
      ),
      # slice_block:
      structure(
        c(
          type = 'Slice type: "head", "tail", "min", "max", "sample"',
          n = "Integer, number of rows to select (ignored when prop is set)",
          prop = "Number between 0 and 1, proportion of rows to select (e.g. 0.05 for 5%). When set, n is ignored",
          order_by = "Column name to order by (required for min/max types)",
          with_ties = "Boolean, whether to include tied values (for min/max types, default true)",
          weight_by = "Column name for weighted sampling (for sample type, optional)",
          replace = "Boolean, whether to sample with replacement (for sample type, default false)",
          by = "Array of column names for grouping (optional)"
        ),
        examples = list(
          type = "head", n = 10L, prop = NULL, order_by = "",
          with_ties = TRUE, weight_by = "", replace = FALSE, by = list()
        ),
        prompt = "Use either n or prop, not both. order_by is required for min and max types."
      ),
      # pivot_longer_block:
      structure(
        c(
          cols = "Array of column names to pivot into longer format",
          names_to = "String, name for the new column containing old column names",
          values_to = "String, name for the new column containing values",
          values_drop_na = "Boolean, if true drop rows with NA values (default false)",
          names_prefix = "String prefix to remove from column names before storing (optional)"
        ),
        examples = list(
          cols = list("col_a", "col_b", "col_c"), names_to = "variable",
          values_to = "value", values_drop_na = FALSE, names_prefix = ""
        )
      ),
      # pivot_wider_block:
      structure(
        c(
          names_from = "Array of column names whose values become new column names",
          values_from = "Array of column names whose values fill the cells",
          id_cols = "Array of column names that uniquely identify each row (optional, defaults to all unspecified columns)",
          values_fill = "Value to fill for missing combinations (e.g. 0). Leave empty to keep NA (optional)",
          names_sep = 'String separator when names_from has multiple columns (default "_")',
          names_prefix = "String prefix to add to all new column names (optional)"
        ),
        examples = list(
          names_from = list("category"), values_from = list("value"),
          id_cols = list(), values_fill = NULL, names_sep = "_", names_prefix = ""
        ),
        prompt = "Leave id_cols empty to auto-detect identifier columns."
      ),
      # unite_block:
      structure(
        c(
          col = "String, name for the new combined column",
          cols = "Array of column names to unite",
          sep = 'String separator between values (default "_")',
          remove = "Boolean, if true remove input columns from output (default true)",
          na.rm = "Boolean, if true remove NA values before uniting (default false)"
        ),
        examples = list(
          col = "full_name", cols = list("first_name", "last_name"),
          sep = " ", remove = TRUE, na.rm = FALSE
        )
      ),
      # separate_block:
      structure(
        c(
          col = "String, column name to separate",
          into = "Array of new column name strings",
          sep = "String separator to split on",
          remove = "Boolean, if true remove input column from output (default true)",
          convert = "Boolean, if true auto-convert column types (default false)",
          extra = '"warn", "drop", or "merge" — how to handle extra pieces (default "warn")',
          fill = '"warn", "right", or "left" — how to handle missing pieces (default "warn")'
        ),
        examples = list(
          col = "full_name", into = list("first", "last"), sep = " ",
          remove = TRUE, convert = FALSE, extra = "warn", fill = "warn"
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
