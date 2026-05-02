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
        ),
        prompt = paste(
          "Set type to the join function:",
          "left_join, inner_join,",
          "right_join, full_join,",
          "anti_join, or semi_join.",
          "Keys define column pairs to",
          "match (xCol op yCol). Op is",
          "usually '==' but can be",
          "'>=', '>', '<=', '<' for",
          "non-equi joins."
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
        ),
        prompt = paste(
          "Each entry has column (name)",
          "and direction ('asc' or",
          "'desc'). Rows are sorted by",
          "columns in order."
        )
      ),
      # mutate_block:
      structure(
        c(
          state = paste0(
            "Object with: mutations (array ",
            "of objects, each with 'name' ",
            "(new column name) and 'expr' ",
            "(R expression string)), and ",
            "optional by (array of grouping ",
            "column names)"
          )
        ),
        examples = list(
          state = list(
            mutations = list(
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
          "Each mutation creates or modifies",
          "a column. The 'name' field is the",
          "new column name, 'expr' is an R",
          "expression. Column names with",
          "spaces or special characters must",
          "be backtick-quoted.",
          "\n\nTo group the operation (e.g.,",
          "mean per group), put grouping",
          "columns in the 'by' array -- do",
          "NOT write group_by() inside",
          "expressions.",
          "\n\nExample: mean mpg per cylinder:",
          "mutations: [{name: 'mean_mpg',",
          "expr: 'mean(mpg)'}], by: ['cyl']",
          "\n\nR coding rules for expressions:",
          "Do NOT prefix base R functions",
          "(mean, sum, sd, min, max, median,",
          "abs, sqrt, log, round, paste,",
          "ifelse, is.na, etc.).",
          "DO prefix dplyr/tidyr functions:",
          "dplyr::n(), dplyr::n_distinct(),",
          "dplyr::first(), dplyr::last(),",
          "dplyr::lag(), dplyr::lead(),",
          "dplyr::case_when(),",
          "dplyr::if_else().",
          "Use the base pipe |> (never %>%)."
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
          "Two modes for each summary:",
          "- Simple: type='simple', func",
          "(mean/median/sd/min/max/sum/n/",
          "n_distinct/first/last), col",
          "(column name). For func='n',",
          "set col to empty string.",
          "- Expression: type='expr',",
          "name, expr (R code string).",
          "\n\nPut grouping columns in 'by'",
          "-- this is how you group. Do NOT",
          "write group_by() in expressions.",
          "\n\nExample: mean mpg + count",
          "per cylinder:",
          "summaries: [{type:'simple',",
          "name:'avg_mpg', func:'mean',",
          "col:'mpg'}, {type:'simple',",
          "name:'count', func:'n',",
          "col:''}], by: ['cyl']",
          "\n\nPrefer simple mode over expr",
          "mode when a predefined function",
          "exists.",
          "\n\nR coding rules for expr mode:",
          "Do NOT prefix base R functions",
          "(mean, sum, sd, etc.).",
          "DO prefix dplyr functions:",
          "dplyr::n(), dplyr::n_distinct().",
          "Use the base pipe |> (never %>%)."
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
          "Conditions can be: type",
          "'values' (column + values",
          "array + mode 'include'/",
          "'exclude'), type 'numeric'",
          "(column + op one of '>', '>=',",
          "'<', '<=' + numeric value;",
          "EQUALITY IS NOT SUPPORTED HERE),",
          "or type 'expr' (free R",
          "expression string). Values",
          "must be strings even for",
          "numeric columns.",
          "\n\nThe 'operator' field combines",
          "multiple conditions: '&' for",
          "AND (all must match), '|' for",
          "OR (any can match).",
          "\n\nIMPORTANT -- include vs",
          "exclude mode: ALWAYS prefer",
          "mode 'include' with the",
          "matching values. Only use mode",
          "'exclude' when the user",
          "explicitly says 'exclude',",
          "'remove', 'drop', or 'not'.",
          "\n\nIMPORTANT -- equality on a",
          "NUMERIC column (e.g. Year ==",
          "2024) uses type='values' with",
          "values=['2024'] (stringified),",
          "NOT type='numeric' with op='=='.",
          "The 'numeric' type only supports",
          "the comparison ops listed above;",
          "passing op='==' results in a",
          "block whose UI control renders",
          "blank even though the underlying",
          "filter may run.",
          "\n\nPrefer type 'values' for",
          "categorical filtering AND for",
          "exact-match on numeric columns,",
          "type 'numeric' for inequality",
          "comparisons. Only use type",
          "'expr' when the other types",
          "cannot express the condition.",
          "\n\nIMPORTANT -- prefer",
          "values+mode='exclude' over",
          "expr+grepl for dropping a known",
          "set of names. The values UI",
          "renders entries as removable",
          "chips and is much easier to",
          "maintain than a regex. Reach",
          "for expr only when the rule is",
          "genuinely substring/regex/",
          "free-form (e.g. 'starts with",
          "Q', or a multi-column",
          "expression).",
          "\n\nIMPORTANT -- a single",
          "filter_block can hold MANY",
          "conditions joined by 'operator'",
          "('&' for AND, '|' for OR). Do",
          "NOT chain multiple filter_blocks",
          "to AND together independent",
          "predicates on the same upstream",
          "data — one block with",
          "conditions: [c1, c2, c3] +",
          "operator: '&' is clearer,",
          "faster, and surfaces all the",
          "rules in one UI. Chain a second",
          "filter_block only when you",
          "deliberately want a transform",
          "(e.g. summarize) between the two",
          "filtering steps.",
          "\n\nR coding rules for expr type:",
          "Do NOT prefix base R functions",
          "(mean, sum, grepl, is.na, etc.).",
          "DO prefix dplyr/tidyr functions.",
          "Use the base pipe |> (never %>%)."
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
        ),
        prompt = paste(
          "Renames is an object mapping",
          "new names to old names:",
          "{new_name: 'old_name', ...}.",
          "Keys are the desired new column",
          "names, values are the existing",
          "column names to rename."
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
          "Types: 'head' (first n rows),",
          "'tail' (last n), 'min' (rows",
          "with smallest order_by values),",
          "'max' (largest order_by values),",
          "'sample' (random sample).",
          "\n\nUse n for count or prop",
          "(0-1) for proportion -- not both.",
          "order_by is required for min/max.",
          "with_ties: include tied rows",
          "(min/max only).",
          "weight_by: weighted sampling",
          "(sample only).",
          "\n\nPut grouping columns in 'by'",
          "to slice within each group."
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
            "names_prefix (string), ",
            "values_fn (string or null, ",
            "e.g. 'mean', 'sum', 'first')"
          )
        ),
        examples = list(
          state = list(
            names_from = list("category"),
            values_from = list("value"),
            id_cols = list(),
            values_fill = NULL,
            names_sep = "_",
            names_prefix = "",
            values_fn = NULL
          )
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
