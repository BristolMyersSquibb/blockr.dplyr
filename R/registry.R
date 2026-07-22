#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_string arg_number arg_integer arg_boolean arg_enum arg_array arg_object
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
    guidance = c(
      # select_block:
      paste(
        "Set columns to keep (or set",
        "exclude=true to remove them",
        "instead). Set distinct=true",
        "to deduplicate rows."
      ),
      # join_block:
      paste(
        "keys define column pairs to match (xCol op yCol).",
        "op is usually '==' but can be '>=', '>', '<=', '<'",
        "for non-equi joins."
      ),
      # arrange_block:
      "",
      # mutate_block:
      paste(
        "Group with the 'by' array (e.g. mean per group) -- do",
        "NOT write group_by() inside expressions. Backtick-quote",
        "column names with spaces or special characters.",
        "\n\nExample -- mean mpg per cylinder: mutations:",
        "[{name:'mean_mpg', expr:'mean(mpg)'}], by:['cyl'].",
        "\n\nR rules: do NOT prefix base R (mean, sum, sd, abs,",
        "ifelse, is.na, ...); DO prefix dplyr/tidyr (dplyr::n(),",
        "dplyr::lag(), dplyr::case_when(), dplyr::if_else(), ...).",
        "Use the base pipe |> (never %>%)."
      ),
      # summarize_block:
      paste(
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
      ),
      # filter_block:
      paste(
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
      ),
      # bind_rows_block:
      "",
      # bind_cols_block:
      paste(
        "Place multiple tables side-by-side horizontally, matching row",
        "positions. All inputs must have the same number of rows - if",
        "they don't, use join_block on a shared key instead. Takes no",
        "constructor parameters; the inputs come from multi-input",
        "wiring on the board."
      ),
      # rename_block:
      "",
      # slice_block:
      paste(
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
      ),
      # pivot_longer_block:
      "",
      # pivot_wider_block:
      paste(
        "Set values_fn (e.g., 'mean',",
        "'sum', 'first') when data has",
        "duplicate id+name combinations.",
        "Without values_fn, duplicates",
        "will cause an error."
      ),
      # unite_block:
      "",
      # separate_block:
      ""
    ),
    arguments = list(
      # select_block:
      new_arg_specs(
        columns = new_arg_spec(
          paste0(
            "Array of column name strings ",
            "to keep (or to remove, when ",
            "exclude=true)"
          ),
          example = list("mpg", "cyl", "hp"),
          type = arg_array(arg_string())
        ),
        exclude = new_arg_spec(
          paste0(
            "Boolean -- when true, drop the ",
            "listed columns instead of ",
            "keeping them"
          ),
          example = FALSE,
          type = arg_boolean()
        ),
        distinct = new_arg_spec(
          paste0(
            "Boolean -- when true, ",
            "deduplicate rows after ",
            "selecting"
          ),
          example = FALSE,
          type = arg_boolean()
        )
      ),
      # join_block:
      new_arg_specs(
        type = new_arg_spec(
          paste0(
            "Join type string: left_join, ",
            "inner_join, right_join, ",
            "full_join, anti_join, or ",
            "semi_join"
          ),
          example = "left_join",
          type = arg_enum(
            c("left_join", "inner_join", "right_join",
              "full_join", "anti_join", "semi_join")
          )
        ),
        keys = new_arg_spec(
          paste0(
            "Array of {xCol, op, yCol} ",
            "objects defining the column ",
            "pairs to match"
          ),
          example = list(list(xCol = "id", op = "==", yCol = "id")),
          type = arg_array(
            arg_object(
              xCol = arg_string(),
              op = arg_enum(c("==", ">=", ">", "<=", "<")),
              yCol = arg_string()
            )
          )
        ),
        exprs = new_arg_spec(
          paste0(
            "Array of R expression strings ",
            "(optional, advanced join ",
            "predicates)"
          ),
          example = list(),
          type = arg_array(arg_string())
        ),
        suffix_x = new_arg_spec(
          paste0(
            "Suffix added to overlapping ",
            "column names from the left ",
            "table (default '.x')"
          ),
          example = ".x",
          type = arg_string()
        ),
        suffix_y = new_arg_spec(
          paste0(
            "Suffix added to overlapping ",
            "column names from the right ",
            "table (default '.y')"
          ),
          example = ".y",
          type = arg_string()
        )
      ),
      # arrange_block:
      new_arg_specs(
        columns = new_arg_spec(
          paste0(
            "Array of {column, direction} ",
            "objects where direction is ",
            "\"asc\" or \"desc\". Rows are ",
            "sorted by the columns in order."
          ),
          example = list(list(column = "mpg", direction = "desc")),
          type = arg_array(
            arg_object(
              column = arg_string(),
              direction = arg_enum(c("asc", "desc"))
            )
          )
        )
      ),
      # mutate_block:
      new_arg_specs(
        mutations = new_arg_spec(
          paste0(
            "Array of objects, each with ",
            "'name' (new column name) and ",
            "'expr' (R expression string)"
          ),
          example = list(
            list(name = "mpg_squared", expr = "mpg^2"),
            list(name = "hp_per_cyl", expr = "hp / cyl")
          ),
          type = arg_array(
            arg_object(name = arg_string(), expr = arg_string())
          )
        ),
        by = new_arg_spec(
          paste0(
            "Optional array of grouping ",
            "column names (group the ",
            "operation, e.g. mean per group)"
          ),
          example = list(),
          type = arg_array(arg_string())
        )
      ),
      # summarize_block:
      new_arg_specs(
        summaries = new_arg_spec(
          paste0(
            "Array of summary objects. ",
            "Types: 'simple' (name, func ",
            "like 'mean'/'sd'/'sum'/'n'/",
            "'n_distinct', col) or 'expr' ",
            "(name, expr as R expression ",
            "string)"
          ),
          example = list(
            list(type = "simple", name = "avg_mpg", func = "mean", col = "mpg"),
            list(type = "simple", name = "count", func = "n", col = "")
          ),
          type = arg_array(
            arg_object(
              type = arg_enum(c("simple", "expr")),
              name = arg_string(),
              func = arg_string(required = FALSE),
              col = arg_string(required = FALSE),
              expr = arg_string(required = FALSE)
            )
          )
        ),
        by = new_arg_spec(
          paste0(
            "Array of grouping column names ",
            "-- this is how you group (do ",
            "NOT write group_by() in exprs)"
          ),
          example = list("cyl"),
          type = arg_array(arg_string())
        )
      ),
      # filter_block:
      new_arg_specs(
        conditions = new_arg_spec(
          paste0(
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
          example = list(
            list(
              type = "values",
              column = "Species",
              values = list("setosa"),
              mode = "include"
            )
          ),
          type = arg_array(
            arg_object(
              type = arg_enum(c("values", "numeric", "expr")),
              column = arg_string(required = FALSE),
              values = arg_array(arg_string(), required = FALSE),
              mode = arg_enum(c("include", "exclude"), required = FALSE),
              op = arg_enum(c(">", ">=", "<", "<="), required = FALSE),
              value = arg_number(required = FALSE),
              expr = arg_string(required = FALSE)
            )
          )
        ),
        operator = new_arg_spec(
          paste0(
            "How to combine conditions: ",
            "\"&\" for AND (all match), ",
            "\"|\" for OR (any match)"
          ),
          example = "&",
          type = arg_enum(c("&", "|"))
        ),
        preserve_order = new_arg_spec(
          paste0(
            "Boolean -- when true, keep the ",
            "original row order rather than ",
            "reordering"
          ),
          example = FALSE,
          type = arg_boolean()
        )
      ),
      # bind_rows_block:
      new_arg_specs(
        id_name = new_arg_spec(
          paste0(
            "Optional string -- name of an ",
            ".id column identifying which ",
            "input each row came from"
          ),
          example = "source",
          type = arg_string()
        )
      ),
      # bind_cols_block:
      new_arg_specs(),
      # rename_block:
      new_arg_specs(
        # A data-keyed map (arbitrary new->old name pairs). The JSON-Schema
        # subset has no open-ended object, so `type` is left unset and the
        # consumer infers a key->value map from the worked example.
        renames = new_arg_spec(
          paste0(
            "Object mapping new names to old",
            " names: {new_name: 'old_name', ",
            "...}. Keys are the desired new ",
            "column names, values are the ",
            "existing column names."
          ),
          example = list(miles_per_gallon = "mpg", cylinders = "cyl")
        )
      ),
      # slice_block:
      new_arg_specs(
        type = new_arg_spec(
          paste0(
            "One of 'head' (first n rows), ",
            "'tail' (last n), 'min'/'max' ",
            "(rows with smallest/largest ",
            "order_by values), 'sample' ",
            "(random sample)"
          ),
          example = "head",
          type = arg_enum(c("head", "tail", "min", "max", "sample"))
        ),
        n = new_arg_spec(
          "Integer row count",
          example = 10L,
          type = arg_integer()
        ),
        prop = new_arg_spec(
          paste0(
            "Number 0-1, proportion of rows ",
            "(alternative to n -- not both)"
          ),
          example = NULL,
          type = arg_number()
        ),
        order_by = new_arg_spec(
          "Column to rank by, for min/max",
          example = "",
          type = arg_string()
        ),
        with_ties = new_arg_spec(
          paste0(
            "Boolean -- whether tied rows at ",
            "the cutoff are all kept (min/max)"
          ),
          example = TRUE,
          type = arg_boolean()
        ),
        weight_by = new_arg_spec(
          "Column of weights, for sampling",
          example = "",
          type = arg_string()
        ),
        replace = new_arg_spec(
          "Boolean -- sample with replacement",
          example = FALSE,
          type = arg_boolean()
        ),
        by = new_arg_spec(
          "Array of grouping columns (slice within each group)",
          example = list(),
          type = arg_array(arg_string())
        )
      ),
      # pivot_longer_block:
      new_arg_specs(
        cols = new_arg_spec(
          "Array of column names to pivot to long form",
          example = list("col_a", "col_b"),
          type = arg_array(arg_string())
        ),
        names_to = new_arg_spec(
          "String -- name of the new column holding the pivoted names",
          example = "variable",
          type = arg_string()
        ),
        values_to = new_arg_spec(
          "String -- name of the new column holding the pivoted values",
          example = "value",
          type = arg_string()
        ),
        values_drop_na = new_arg_spec(
          "Boolean -- drop rows with NA values after pivoting",
          example = FALSE,
          type = arg_boolean()
        ),
        names_prefix = new_arg_spec(
          "String -- regex prefix stripped from the pivoted names",
          example = "",
          type = arg_string()
        )
      ),
      # pivot_wider_block:
      new_arg_specs(
        names_from = new_arg_spec(
          "Array of column(s) whose values become the new column names",
          example = list("category"),
          type = arg_array(arg_string())
        ),
        values_from = new_arg_spec(
          "Array of column(s) whose values fill the new columns",
          example = list("value"),
          type = arg_array(arg_string())
        ),
        id_cols = new_arg_spec(
          "Array (optional) of columns that uniquely identify each output row",
          example = list(),
          type = arg_array(arg_string())
        ),
        # Scalar of unknown type (number/string/NULL) -- left untyped.
        values_fill = new_arg_spec(
          "Value (or null) used to fill missing cells",
          example = NULL
        ),
        names_sep = new_arg_spec(
          "String separator joining multiple names_from / prefix parts",
          example = "_",
          type = arg_string()
        ),
        names_prefix = new_arg_spec(
          "String prepended to every new column name",
          example = "",
          type = arg_string()
        ),
        values_fn = new_arg_spec(
          paste0(
            "String (or null) aggregation ",
            "function (e.g. 'mean', 'sum', ",
            "'first') for duplicate id+name ",
            "combinations"
          ),
          example = NULL,
          type = arg_string()
        )
      ),
      # unite_block:
      new_arg_specs(
        col = new_arg_spec(
          "New column name for the united result",
          example = "full_name",
          type = arg_string()
        ),
        cols = new_arg_spec(
          "Array of columns to unite",
          example = list("first_name", "last_name"),
          type = arg_array(arg_string())
        ),
        sep = new_arg_spec(
          "Separator string placed between values",
          example = " ",
          type = arg_string()
        ),
        remove = new_arg_spec(
          "Boolean -- remove the input columns after uniting",
          example = TRUE,
          type = arg_boolean()
        ),
        na_rm = new_arg_spec(
          "Boolean -- drop NA values before uniting",
          example = FALSE,
          type = arg_boolean()
        )
      ),
      # separate_block:
      new_arg_specs(
        col = new_arg_spec(
          "Source column to split",
          example = "full_name",
          type = arg_string()
        ),
        into = new_arg_spec(
          "Array of new column names to split into",
          example = list("first", "last"),
          type = arg_array(arg_string())
        ),
        sep = new_arg_spec(
          "Separator string (or regex) to split on",
          example = " ",
          type = arg_string()
        ),
        remove = new_arg_spec(
          "Boolean -- remove the source column after splitting",
          example = TRUE,
          type = arg_boolean()
        ),
        convert = new_arg_spec(
          "Boolean -- auto-convert the new columns to numeric/logical types",
          example = FALSE,
          type = arg_boolean()
        )
      )
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
