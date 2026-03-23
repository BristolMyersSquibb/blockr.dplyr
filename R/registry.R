#' @importFrom blockr.core register_blocks
register_dplyr_blocks <- function() {
  register_blocks(
    c(
      "new_select_block",
      "new_join_block",
      "new_arrange_block",
      "new_mutate_block",
      "new_summarize_block",
      "new_filter_block",
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
      "Filter Rows",
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
      paste0(
        "Choose which columns to keep or remove. ",
        "Can also remove duplicate rows. ",
        "(dplyr: select, distinct)"
      ),
      paste0(
        "Combine two tables by matching values ",
        "in a shared column. ",
        "(dplyr: left_join, inner_join, etc.)"
      ),
      paste0(
        "Sort your data by one or more columns, ",
        "ascending or descending. (dplyr: arrange)"
      ),
      paste0(
        "Create new columns or modify existing ",
        "ones using R expressions. (dplyr: mutate)"
      ),
      paste0(
        "Calculate totals, averages, counts, and ",
        "other statistics. Simple mode or R ",
        "expressions. (dplyr: summarize)"
      ),
      paste0(
        "Filter rows by values, numeric comparisons,",
        " or R expressions with AND/OR logic. ",
        "(dplyr: filter)"
      ),
      paste0(
        "Stack tables vertically by matching ",
        "column names. (dplyr: bind_rows)"
      ),
      paste0(
        "Place tables next to each other ",
        "horizontally. Both tables must have the ",
        "same number of rows. (dplyr: bind_cols)"
      ),
      paste0(
        "Change column names to make them more ",
        "readable. (dplyr: rename)"
      ),
      paste0(
        "Select specific rows: first N, last N, ",
        "random sample, or rows with min/max ",
        "values. (dplyr: slice)"
      ),
      paste0(
        "Convert columns into rows. Useful when ",
        "column headers contain data values. ",
        "(tidyr: pivot_longer)"
      ),
      paste0(
        "Convert rows into columns. Turn ",
        "categories into separate columns. ",
        "(tidyr: pivot_wider)"
      ),
      paste0(
        "Merge multiple columns into one by ",
        "pasting values together. (tidyr: unite)"
      ),
      paste0(
        "Split one column into multiple columns ",
        "using a delimiter. (tidyr: separate)"
      )
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
      "transform"
    ),
    icon = c(
      "layout-sidebar-inset-reverse", # select block
      "bezier2", # join block
      "sort-down-alt", # arrange block
      "pencil-square", # mutate block
      "sliders", # summarize block
      "filter", # filter block
      "arrows-collapse", # bind_rows: rows stacking
      "arrows-collapse-vertical", # bind_cols
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
          state = paste0(
            "Object with: columns (array of ",
            "column name strings), ",
            "exclude (boolean), ",
            "distinct (boolean)"
          )
        ),
        examples = list(
          state = list(
            columns = list("mpg", "cyl", "hp"),
            exclude = FALSE,
            distinct = FALSE
          )
        )
      ),
      # join_block:
      structure(
        c(
          state = paste0(
            "Object with: type (join type ",
            "string), keys (array of ",
            "{xCol, op, yCol}), ",
            "exprs (array of R expression ",
            "strings), suffix_x, suffix_y"
          )
        ),
        examples = list(
          state = list(
            type = "left_join",
            keys = list(list(
              xCol = "id", op = "==",
              yCol = "id"
            )),
            exprs = list(),
            suffix_x = ".x",
            suffix_y = ".y"
          )
        )
      ),
      # arrange_block:
      structure(
        c(
          state = paste0(
            "Object with: columns (array of ",
            "{column, direction} where ",
            "direction is \"asc\" or \"desc\")"
          )
        ),
        examples = list(
          state = list(columns = list(
            list(column = "mpg", direction = "desc")
          ))
        )
      ),
      # mutate_block:
      structure(
        c(
          state = paste0(
            "Object with: rows (array of ",
            "objects, each with 'name' ",
            "(new column name) and 'expr' ",
            "(R expression string)), and ",
            "optional by (array of grouping ",
            "column names)"
          )
        ),
        examples = list(
          state = list(
            rows = list(
              list(
                name = "mpg_squared",
                expr = "mpg^2"
              ),
              list(
                name = "hp_per_cyl",
                expr = "hp / cyl"
              )
            )
          )
        ),
        prompt = paste(
          "Expressions are R code strings.",
          "Each row creates or modifies",
          "a column. Column names with",
          "spaces or special characters",
          "must be backtick-quoted.",
          "\n\nR coding rules: always use",
          "the base pipe |> (never %>%).",
          "Namespace-prefix all functions",
          "except base and stats."
        )
      ),
      # summarize_block:
      structure(
        c(
          state = paste0(
            "Object with: summaries (array ",
            "of summary objects) and by ",
            "(array of grouping column ",
            "names). Summary types: ",
            "'simple' (name, func like ",
            "'mean'/'sd'/'sum'/'n'/",
            "'n_distinct', col) or 'expr' ",
            "(name, expr as R expression ",
            "string)"
          )
        ),
        examples = list(
          state = list(
            summaries = list(
              list(
                type = "simple",
                name = "avg_mpg",
                func = "mean",
                col = "mpg"
              ),
              list(
                type = "simple",
                name = "count",
                func = "n",
                col = ""
              )
            ),
            by = list("cyl")
          )
        ),
        prompt = paste(
          "Simple mode uses predefined",
          "functions (mean, median, sd,",
          "min, max, sum, n, n_distinct,",
          "first, last). Expression mode",
          "allows arbitrary R code.",
          "The 'n' function does not need",
          "a column argument."
        )
      ),
      # filter_block:
      structure(
        c(
          state = paste0(
            "Object with: conditions ",
            "(array of condition objects) ",
            "and operator ",
            "(\"&\" or \"|\"). ",
            "Condition types: ",
            "\"values\" (column, values ",
            "array of strings, mode ",
            "\"include\"/\"exclude\"), ",
            "\"numeric\" (column, op like ",
            "\">\"/\">=\"/\"<\"/\"<=\", ",
            "value as number), ",
            "\"expr\" (expr as R ",
            "expression string)"
          )
        ),
        examples = list(
          state = list(
            conditions = list(
              list(
                type = "values",
                column = "Species",
                values = list("setosa"),
                mode = "include"
              )
            ),
            operator = "&"
          )
        ),
        prompt = paste(
          "The state contains the full",
          "filter configuration as a",
          "single object. Conditions can",
          "be: type 'values' (column +",
          "values array + mode",
          "'include'/'exclude'), type",
          "'numeric' (column + op like",
          "'>', '>=', '<', '<=' + numeric",
          "value), or type 'expr' (free R",
          "expression string). Values",
          "must be strings even for",
          "numeric columns.",
          "\n\nIMPORTANT -- include vs",
          "exclude mode: ALWAYS prefer",
          "mode 'include' with the",
          "matching values. Only use mode",
          "'exclude' when the user",
          "explicitly says 'exclude',",
          "'remove', 'drop', or 'not'.",
          "\n\nFor numeric comparisons,",
          "prefer type 'numeric' over",
          "type 'values'.",
          "\n\nR coding rules for expr",
          "type: always use the base",
          "pipe |> (never %>%).",
          "Namespace-prefix all functions",
          "except base and stats."
        )
      ),
      # bind_rows_block:
      structure(
        c(
          state = paste0(
            "Object with: id_name ",
            "(optional string for ",
            ".id column name)"
          )
        ),
        examples = list(
          state = list(id_name = "source")
        )
      ),
      # bind_cols_block:
      NULL,
      # rename_block:
      structure(
        c(
          state = paste0(
            "Object with: renames (object",
            " where keys are new names,",
            " values are old names)"
          )
        ),
        examples = list(
          state = list(renames = list(
            miles_per_gallon = "mpg",
            cylinders = "cyl"
          ))
        )
      ),
      # slice_block:
      structure(
        c(
          state = paste0(
            "Object with: type ",
            "('head'/'tail'/'min'/'max'/",
            "'sample'), n (integer), ",
            "prop (number 0-1, alternative",
            " to n), order_by (column for",
            " min/max), with_ties ",
            "(boolean), weight_by (column",
            " for sample), replace ",
            "(boolean), by (array of ",
            "grouping columns)"
          )
        ),
        examples = list(
          state = list(
            type = "head", n = 10L,
            prop = NULL, order_by = "",
            with_ties = TRUE,
            weight_by = "",
            replace = FALSE,
            by = list()
          )
        ),
        prompt = paste(
          "Use either n or prop,",
          "not both. order_by is",
          "required for min and",
          "max types."
        )
      ),
      # pivot_longer_block:
      structure(
        c(
          state = paste0(
            "Object with: cols (array of ",
            "column names to pivot), ",
            "names_to (string), ",
            "values_to (string), ",
            "values_drop_na (boolean), ",
            "names_prefix (string)"
          )
        ),
        examples = list(
          state = list(
            cols = list("col_a", "col_b"),
            names_to = "variable",
            values_to = "value",
            values_drop_na = FALSE,
            names_prefix = ""
          )
        )
      ),
      # pivot_wider_block:
      structure(
        c(
          state = paste0(
            "Object with: names_from ",
            "(array), values_from (array),",
            " id_cols (array, optional), ",
            "values_fill (value or null), ",
            "names_sep (string), ",
            "names_prefix (string)"
          )
        ),
        examples = list(
          state = list(
            names_from = list("category"),
            values_from = list("value"),
            id_cols = list(),
            values_fill = NULL,
            names_sep = "_",
            names_prefix = ""
          )
        )
      ),
      # unite_block:
      structure(
        c(
          state = paste0(
            "Object with: col (new column",
            " name), cols (array of ",
            "columns to unite), ",
            "sep (separator string), ",
            "remove (boolean), ",
            "na_rm (boolean)"
          )
        ),
        examples = list(
          state = list(
            col = "full_name",
            cols = list(
              "first_name", "last_name"
            ),
            sep = " ", remove = TRUE,
            na_rm = FALSE
          )
        )
      ),
      # separate_block:
      structure(
        c(
          state = paste0(
            "Object with: col (source ",
            "column), into (array of new ",
            "column names), sep (separator",
            " string), remove (boolean), ",
            "convert (boolean)"
          )
        ),
        examples = list(
          state = list(
            col = "full_name",
            into = list("first", "last"),
            sep = " ", remove = TRUE,
            convert = FALSE
          )
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
