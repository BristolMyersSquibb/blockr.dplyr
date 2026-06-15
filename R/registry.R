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
      "Join",
      "Arrange Rows",
      "Mutate Columns",
      "Summarize",
      "Filter Rows",
      "Bind Rows",
      "Bind Columns",
      "Rename",
      "Slice Rows",
      "Pivot Longer",
      "Pivot Wider",
      "Unite Columns",
      "Separate Column"
    ),
    description = c(
      paste0(
        "Choose, reorder, or exclude columns. ",
        "Optionally remove duplicate rows. ",
        "(dplyr: select, distinct)"
      ),
      paste0(
        "Combine two tables by matching values ",
        "in shared columns. Left, inner, right, ",
        "full, anti, semi joins. (dplyr: *_join)"
      ),
      paste0(
        "Sort rows by one or more columns, ",
        "ascending or descending. (dplyr: arrange)"
      ),
      paste0(
        "Add new columns or transform existing ",
        "ones using R expressions. (dplyr: mutate)"
      ),
      paste0(
        "Calculate totals, averages, counts, and ",
        "other statistics, optionally by group. ",
        "(dplyr: summarize)"
      ),
      paste0(
        "Keep or remove rows by values, ",
        "comparisons, or expressions. ",
        "(dplyr: filter)"
      ),
      paste0(
        "Stack tables vertically by matching ",
        "column names. (dplyr: bind_rows)"
      ),
      paste0(
        "Place tables side-by-side horizontally. ",
        "Both must have the same number of rows. ",
        "(dplyr: bind_cols)"
      ),
      paste0(
        "Change column names without modifying ",
        "data. (dplyr: rename)"
      ),
      paste0(
        "Pick rows by position, random sample, ",
        "or min/max of a column. (dplyr: slice)"
      ),
      paste0(
        "Reshape wide data to long \u2014 gather ",
        "columns into rows. (tidyr: pivot_longer)"
      ),
      paste0(
        "Reshape long data to wide \u2014 spread ",
        "rows into columns. (tidyr: pivot_wider)"
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
          columns = paste0(
            "Array of column name strings ",
            "to keep (or to remove, when ",
            "exclude=true)"
          ),
          exclude = paste0(
            "Boolean -- when true, drop the ",
            "listed columns instead of ",
            "keeping them"
          ),
          distinct = paste0(
            "Boolean -- when true, ",
            "deduplicate rows after ",
            "selecting"
          )
        ),
        examples = list(
          columns = list("mpg", "cyl", "hp"),
          exclude = FALSE,
          distinct = FALSE
        ),
        prompt = paste(
          "Set columns to keep (or set",
          "exclude=true to remove them",
          "instead). Set distinct=true",
          "to deduplicate rows."
        )
      ),
      # join_block:
      structure(
        c(
          type = paste0(
            "Join type string: left_join, ",
            "inner_join, right_join, ",
            "full_join, anti_join, or ",
            "semi_join"
          ),
          keys = paste0(
            "Array of {xCol, op, yCol} ",
            "objects defining the column ",
            "pairs to match"
          ),
          exprs = paste0(
            "Array of R expression strings ",
            "(optional, advanced join ",
            "predicates)"
          ),
          suffix_x = paste0(
            "Suffix added to overlapping ",
            "column names from the left ",
            "table (default '.x')"
          ),
          suffix_y = paste0(
            "Suffix added to overlapping ",
            "column names from the right ",
            "table (default '.y')"
          )
        ),
        examples = list(
          type = "left_join",
          keys = list(list(
            xCol = "id", op = "==",
            yCol = "id"
          )),
          exprs = list(),
          suffix_x = ".x",
          suffix_y = ".y"
        ),
        prompt = paste(
          "keys define column pairs to match (xCol op yCol).",
          "op is usually '==' but can be '>=', '>', '<=', '<'",
          "for non-equi joins."
        )
      ),
      # arrange_block:
      structure(
        c(
          columns = paste0(
            "Array of {column, direction} ",
            "objects where direction is ",
            "\"asc\" or \"desc\". Rows are ",
            "sorted by the columns in order."
          )
        ),
        examples = list(
          columns = list(
            list(column = "mpg", direction = "desc")
          )
        )
      ),
      # mutate_block:
      structure(
        c(
          mutations = paste0(
            "Array of objects, each with ",
            "'name' (new column name) and ",
            "'expr' (R expression string)"
          ),
          by = paste0(
            "Optional array of grouping ",
            "column names (group the ",
            "operation, e.g. mean per group)"
          )
        ),
        examples = list(
          mutations = list(
            list(
              name = "mpg_squared",
              expr = "mpg^2"
            ),
            list(
              name = "hp_per_cyl",
              expr = "hp / cyl"
            )
          ),
          by = list()
        ),
        prompt = paste(
          "Group with the 'by' array (e.g. mean per group) -- do",
          "NOT write group_by() inside expressions. Backtick-quote",
          "column names with spaces or special characters.",
          "\n\nExample -- mean mpg per cylinder: mutations:",
          "[{name:'mean_mpg', expr:'mean(mpg)'}], by:['cyl'].",
          "\n\nR rules: do NOT prefix base R (mean, sum, sd, abs,",
          "ifelse, is.na, ...); DO prefix dplyr/tidyr (dplyr::n(),",
          "dplyr::lag(), dplyr::case_when(), dplyr::if_else(), ...).",
          "Use the base pipe |> (never %>%)."
        )
      ),
      # summarize_block:
      structure(
        c(
          summaries = paste0(
            "Array of summary objects. ",
            "Types: 'simple' (name, func ",
            "like 'mean'/'sd'/'sum'/'n'/",
            "'n_distinct', col) or 'expr' ",
            "(name, expr as R expression ",
            "string)"
          ),
          by = paste0(
            "Array of grouping column names ",
            "-- this is how you group (do ",
            "NOT write group_by() in exprs)"
          )
        ),
        examples = list(
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
        ),
        prompt = paste(
          "Prefer simple mode (type='simple', func, col) over expr",
          "mode when a predefined function exists; for func='n'",
          "set col=''.",
          "\n\nGroup with the 'by' array -- do NOT write group_by()",
          "in expressions.",
          "\n\nExample -- mean mpg + count per cylinder: summaries:",
          "[{type:'simple', name:'avg_mpg', func:'mean', col:'mpg'},",
          "{type:'simple', name:'count', func:'n', col:''}],",
          "by:['cyl'].",
          "\n\nR rules (expr mode): do NOT prefix base R; DO prefix",
          "dplyr (dplyr::n(), dplyr::n_distinct()). Base pipe |> only."
        )
      ),
      # filter_block:
      structure(
        c(
          conditions = paste0(
            "Array of condition objects. ",
            "Types: \"values\" (column, ",
            "values array of strings, mode ",
            "\"include\"/\"exclude\"), ",
            "\"numeric\" (column, op like ",
            "\">\"/\">=\"/\"<\"/\"<=\", ",
            "value as number), ",
            "\"expr\" (expr as R ",
            "expression string)"
          ),
          operator = paste0(
            "How to combine conditions: ",
            "\"&\" for AND (all match), ",
            "\"|\" for OR (any match)"
          ),
          preserve_order = paste0(
            "Boolean -- when true, keep the ",
            "original row order rather than ",
            "reordering"
          )
        ),
        examples = list(
          conditions = list(
            list(
              type = "values",
              column = "Species",
              values = list("setosa"),
              mode = "include"
            )
          ),
          operator = "&",
          preserve_order = FALSE
        ),
        prompt = paste(
          "Combine conditions with 'operator': '&' (AND, all",
          "match) / '|' (OR, any match). Condition values are",
          "always strings, even for numeric columns.",
          "\n\nIMPORTANT -- include vs exclude: ALWAYS prefer mode",
          "'include'. Use 'exclude' only when the user says",
          "'exclude', 'remove', 'drop', or 'not'.",
          "\n\nIMPORTANT -- equality on a NUMERIC column (e.g.",
          "Year == 2024) uses type='values' with stringified",
          "values (values=['2024']), NOT type='numeric'.",
          "type='numeric' supports only '>', '>=', '<', '<=';",
          "op='==' renders a blank UI control.",
          "\n\nChoose the type: 'values' for categorical and",
          "exact-match numeric; 'numeric' for inequalities; 'expr'",
          "only when the others can't express the rule. Prefer",
          "values+mode='exclude' over expr+grepl to drop a known",
          "set of names (removable chips beat a regex); reach for",
          "expr only for genuinely substring/regex/multi-column",
          "rules.",
          "\n\nIMPORTANT -- one filter_block holds MANY conditions",
          "(conditions:[c1,c2,c3] + operator). Do NOT chain",
          "separate filter_blocks to AND predicates on the same",
          "data; chain only to put a transform (e.g. summarize)",
          "between filtering steps.",
          "\n\nR rules (expr type): do NOT prefix base R (grepl,",
          "is.na, ...); DO prefix dplyr/tidyr. Base pipe |> only."
        )
      ),
      # bind_rows_block:
      structure(
        c(
          id_name = paste0(
            "Optional string -- name of an ",
            ".id column identifying which ",
            "input each row came from"
          )
        ),
        examples = list(id_name = "source")
      ),
      # bind_cols_block:
      structure(
        character(),
        prompt = paste(
          "Place multiple tables side-by-side horizontally, matching row",
          "positions. All inputs must have the same number of rows - if",
          "they don't, use join_block on a shared key instead. Takes no",
          "constructor parameters; the inputs come from multi-input",
          "wiring on the board."
        )
      ),
      # rename_block:
      structure(
        c(
          renames = paste0(
            "Object mapping new names to old",
            " names: {new_name: 'old_name', ",
            "...}. Keys are the desired new ",
            "column names, values are the ",
            "existing column names."
          )
        ),
        examples = list(
          renames = list(
            miles_per_gallon = "mpg",
            cylinders = "cyl"
          )
        )
      ),
      # slice_block:
      structure(
        c(
          type = paste0(
            "One of 'head' (first n rows), ",
            "'tail' (last n), 'min'/'max' ",
            "(rows with smallest/largest ",
            "order_by values), 'sample' ",
            "(random sample)"
          ),
          n = "Integer row count",
          prop = paste0(
            "Number 0-1, proportion of rows ",
            "(alternative to n -- not both)"
          ),
          order_by = "Column to rank by, for min/max",
          with_ties = paste0(
            "Boolean -- whether tied rows at ",
            "the cutoff are all kept (min/max)"
          ),
          weight_by = "Column of weights, for sampling",
          replace = "Boolean -- sample with replacement",
          by = "Array of grouping columns (slice within each group)"
        ),
        examples = list(
          type = "head", n = 10L,
          prop = NULL, order_by = "",
          with_ties = TRUE,
          weight_by = "",
          replace = FALSE,
          by = list()
        ),
        prompt = paste(
          "Use n for a count or prop (0-1) for a proportion --",
          "not both. order_by is required for type 'min'/'max'.",
          "\n\nIMPORTANT -- with_ties defaults to TRUE for",
          "min/max, so slice(type='max', n=10) can return MORE",
          "than 10 rows on ties; set with_ties=FALSE for exactly",
          "N.",
          "\n\nFor 'top N by a column' prefer this block",
          "(type='max', order_by=COL, n=N) over arrange +",
          "slice('head'); type='min' for bottom-N. Put grouping",
          "columns in 'by' to slice within each group."
        )
      ),
      # pivot_longer_block:
      structure(
        c(
          cols = "Array of column names to pivot to long form",
          names_to = "String -- name of the new column holding the pivoted names",
          values_to = "String -- name of the new column holding the pivoted values",
          values_drop_na = "Boolean -- drop rows with NA values after pivoting",
          names_prefix = "String -- regex prefix stripped from the pivoted names"
        ),
        examples = list(
          cols = list("col_a", "col_b"),
          names_to = "variable",
          values_to = "value",
          values_drop_na = FALSE,
          names_prefix = ""
        )
      ),
      # pivot_wider_block:
      structure(
        c(
          names_from = "Array of column(s) whose values become the new column names",
          values_from = "Array of column(s) whose values fill the new columns",
          id_cols = "Array (optional) of columns that uniquely identify each output row",
          values_fill = "Value (or null) used to fill missing cells",
          names_sep = "String separator joining multiple names_from / prefix parts",
          names_prefix = "String prepended to every new column name",
          values_fn = paste0(
            "String (or null) aggregation ",
            "function (e.g. 'mean', 'sum', ",
            "'first') for duplicate id+name ",
            "combinations"
          )
        ),
        examples = list(
          names_from = list("category"),
          values_from = list("value"),
          id_cols = list(),
          values_fill = NULL,
          names_sep = "_",
          names_prefix = "",
          values_fn = NULL
        ),
        prompt = paste(
          "Set values_fn (e.g., 'mean',",
          "'sum', 'first') when data has",
          "duplicate id+name combinations.",
          "Without values_fn, duplicates",
          "will cause an error."
        )
      ),
      # unite_block:
      structure(
        c(
          col = "New column name for the united result",
          cols = "Array of columns to unite",
          sep = "Separator string placed between values",
          remove = "Boolean -- remove the input columns after uniting",
          na_rm = "Boolean -- drop NA values before uniting"
        ),
        examples = list(
          col = "full_name",
          cols = list(
            "first_name", "last_name"
          ),
          sep = " ", remove = TRUE,
          na_rm = FALSE
        )
      ),
      # separate_block:
      structure(
        c(
          col = "Source column to split",
          into = "Array of new column names to split into",
          sep = "Separator string (or regex) to split on",
          remove = "Boolean -- remove the source column after splitting",
          convert = "Boolean -- auto-convert the new columns to numeric/logical types"
        ),
        examples = list(
          col = "full_name",
          into = list("first", "last"),
          sep = " ", remove = TRUE,
          convert = FALSE
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
