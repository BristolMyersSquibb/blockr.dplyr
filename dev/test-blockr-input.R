library(shiny)
library(htmltools)

# Run from blockr.dplyr/dev/
js_path <- normalizePath("../inst/assets/js/blockr-input.js")
css_path <- normalizePath("../inst/assets/css/blockr-input.css")

ui <- fluidPage(
  tags$head(
    includeCSS(css_path),
    includeScript(js_path),
    tags$style(HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; padding: 30px; background: #f5f6f8; }
      .demo-card {
        background: #fff;
        border: 1px solid #e5e7eb;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 20px;
        max-width: 600px;
      }
      .demo-card h3 { margin-top: 0; font-size: 0.95rem; color: #374151; }
      .demo-card p { font-size: 0.8rem; color: #6b7280; margin: 4px 0 12px; }
      .demo-row {
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 6px 0;
        border-bottom: 1px solid #f3f4f6;
      }
      .demo-label { font-size: 0.8rem; color: #6b7280; min-width: 80px; }
      .demo-input-wrap { flex: 1; }
      #log { font-family: monospace; font-size: 0.8rem; background: #1f2937; color: #a5f3fc;
              padding: 12px; border-radius: 8px; max-height: 200px; overflow-y: auto;
              white-space: pre-wrap; max-width: 600px; }
    "))
  ),

  h2("BlockrInput Component Demo"),

  # --- Demo 1: Filter expression (transparent, row-style) ---
  div(class = "demo-card",
    h3("Filter expression (transparent, inside a row)"),
    p("Type a column name or function. Try: 'm' for mean/min/max, 'S' for Sepal columns."),
    div(class = "demo-row",
      span(class = "demo-label", "Expression"),
      div(class = "demo-input-wrap", id = "demo-filter-expr")
    )
  ),

  # --- Demo 2: Standalone bordered ---
  div(class = "demo-card",
    h3("Mutate expression (bordered, standalone)"),
    p("Press Enter to confirm. Functions insert with cursor between parens."),
    div(id = "demo-mutate-expr")
  ),

  # --- Demo 3: Summarize expression (different categories) ---
  div(class = "demo-card",
    h3("Summarize expression (aggregate-focused categories)"),
    p("Only aggregate + arithmetic categories. Try 'me' for mean/median."),
    div(class = "demo-row",
      span(class = "demo-label", "Expression"),
      div(class = "demo-input-wrap", id = "demo-summarize-expr")
    )
  ),

  # --- Demo 4: Dynamic column update ---
  div(class = "demo-card",
    h3("Dynamic column update"),
    p("Click the button to switch from iris to mtcars columns."),
    div(class = "demo-row",
      span(class = "demo-label", "Expression"),
      div(class = "demo-input-wrap", id = "demo-dynamic-expr")
    ),
    actionButton("update_cols", "Switch to mtcars columns", style = "margin-top: 10px;")
  ),

  # --- Demo 5: Pre-filled expression ---
  div(class = "demo-card",
    h3("Pre-filled expression"),
    p("Starts with a value already set."),
    div(class = "demo-row",
      span(class = "demo-label", "Expression"),
      div(class = "demo-input-wrap", id = "demo-prefilled")
    )
  ),

  # --- Log ---
  div(class = "demo-card",
    h3("Event Log"),
    div(id = "log")
  ),

  tags$script(HTML("
    function log(msg) {
      var el = document.getElementById('log');
      el.textContent += new Date().toLocaleTimeString() + '  ' + msg + '\\n';
      el.scrollTop = el.scrollHeight;
    }

    var irisCols = ['Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species'];
    var mtcarsCols = ['mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb'];

    var filterCats = {
      arithmetic: ['abs','sign','ceiling','floor','round','trunc','log','log2','log10','exp','sqrt'],
      aggregate: ['mean','sum','min','max'],
      offset: ['lead','lag','cumsum','cumprod','cummin','cummax'],
      logical: ['if_else','case_when'],
      string: ['str_c','paste','paste0','str_sub','str_to_lower','str_to_upper'],
      ranking: ['row_number','min_rank','dense_rank','percent_rank','ntile']
    };

    var summarizeCats = {
      aggregate: ['mean','median','sd','sum','min','max','n','n_distinct','first','last'],
      arithmetic: ['abs','sign','ceiling','floor','round','trunc','log','log2','log10','exp','sqrt']
    };

    // Demo 1: Filter expression, transparent
    var filterExpr = BlockrInput.create(document.getElementById('demo-filter-expr'), {
      columns: irisCols,
      categories: filterCats,
      placeholder: 'R expression...',
      onChange: function() { log('filter onChange: ' + filterExpr.getValue()); },
      onConfirm: function(v) { log('filter CONFIRM: ' + v); }
    });

    // Demo 2: Mutate expression, bordered
    var mutateExpr = BlockrInput.create(document.getElementById('demo-mutate-expr'), {
      columns: irisCols,
      categories: filterCats,
      placeholder: 'e.g. round(Sepal.Length * 2)',
      onChange: function() {},
      onConfirm: function(v) { log('mutate CONFIRM: ' + v); }
    });
    mutateExpr.el.classList.add('blockr-input--bordered');

    // Demo 3: Summarize expression
    var sumExpr = BlockrInput.create(document.getElementById('demo-summarize-expr'), {
      columns: irisCols,
      categories: summarizeCats,
      placeholder: 'e.g. mean(Sepal.Length)',
      onChange: function() {},
      onConfirm: function(v) { log('summarize CONFIRM: ' + v); }
    });

    // Demo 4: Dynamic columns
    var dynExpr = BlockrInput.create(document.getElementById('demo-dynamic-expr'), {
      columns: irisCols,
      categories: filterCats,
      placeholder: 'Type a column name...',
      onChange: function() {},
      onConfirm: function(v) { log('dynamic CONFIRM: ' + v); }
    });

    $(document).on('click', '#update_cols', function() {
      log('--- switching to mtcars columns ---');
      dynExpr.setColumns(mtcarsCols);
    });

    // Demo 5: Pre-filled
    var preExpr = BlockrInput.create(document.getElementById('demo-prefilled'), {
      value: 'mean(Sepal.Length, na.rm = TRUE)',
      columns: irisCols,
      categories: filterCats,
      placeholder: 'R expression...',
      onChange: function() { log('prefilled onChange: ' + preExpr.getValue()); },
      onConfirm: function(v) { log('prefilled CONFIRM: ' + v); }
    });
  "))
)

server <- function(input, output, session) {}

shinyApp(ui, server, options = list(port = 7861, host = "0.0.0.0"))
