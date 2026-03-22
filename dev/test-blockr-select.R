library(shiny)
library(htmltools)

# Run from blockr.dplyr/dev/
js_path <- normalizePath("../inst/assets/js/blockr-select.js")
css_path <- normalizePath("../inst/assets/css/blockr-select.css")

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
        max-width: 500px;
      }
      .demo-card h3 { margin-top: 0; font-size: 0.95rem; color: #374151; }
      .demo-row {
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 6px 0;
        border-bottom: 1px solid #f3f4f6;
      }
      .demo-label { font-size: 0.8rem; color: #6b7280; min-width: 80px; }
      .demo-select-wrap { flex: 1; }
      #log { font-family: monospace; font-size: 0.8rem; background: #1f2937; color: #a5f3fc;
              padding: 12px; border-radius: 8px; max-height: 200px; overflow-y: auto;
              white-space: pre-wrap; max-width: 500px; }
    "))
  ),

  h2("BlockrSelect Component Demo"),

  # --- Demo 1: Single select (transparent, like inside a row) ---
  div(class = "demo-card",
    h3("Single Select (transparent, row-style)"),
    div(class = "demo-row",
      span(class = "demo-label", "Column"),
      div(class = "demo-select-wrap", id = "demo-single")
    )
  ),

  # --- Demo 2: Single select (bordered, standalone) ---
  div(class = "demo-card",
    h3("Single Select (bordered, standalone)"),
    div(id = "demo-single-bordered")
  ),

  # --- Demo 3: Multi select with remove + drag (transparent) ---
  div(class = "demo-card",
    h3("Multi Select with drag reorder (transparent, row-style)"),
    div(class = "demo-row",
      span(class = "demo-label", "Values"),
      div(class = "demo-select-wrap", id = "demo-multi")
    )
  ),

  # --- Demo 4: Multi select bordered (like group-by) ---
  div(class = "demo-card",
    h3("Multi Select (bordered, standalone — like group-by)"),
    div(id = "demo-multi-bordered")
  ),

  # --- Demo 5: Multi select with many items (two rows) ---
  div(class = "demo-card",
    h3("Multi Select — many items (wrapping to two rows)"),
    div(id = "demo-multi-many")
  ),

  # --- Demo 6: Dynamic options update ---
  div(class = "demo-card",
    h3("Dynamic Options Update"),
    div(class = "demo-row",
      span(class = "demo-label", "Dataset"),
      div(class = "demo-select-wrap", id = "demo-dataset")
    ),
    div(class = "demo-row",
      span(class = "demo-label", "Column"),
      div(class = "demo-select-wrap", id = "demo-dynamic-col")
    ),
    actionButton("update_btn", "Switch to mtcars columns", style = "margin-top: 10px;")
  ),

  # --- Log ---
  div(class = "demo-card",
    h3("Event Log"),
    div(id = "log")
  ),

  # --- Init script ---
  tags$script(HTML("
    function log(msg) {
      var el = document.getElementById('log');
      el.textContent += new Date().toLocaleTimeString() + '  ' + msg + '\\n';
      el.scrollTop = el.scrollHeight;
    }

    // Demo 1: Single select, transparent
    var single = BlockrSelect.single(document.getElementById('demo-single'), {
      options: ['Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species'],
      selected: 'Sepal.Length',
      placeholder: 'Column...',
      onChange: function(v) { log('single: ' + v); }
    });

    // Demo 2: Single select, bordered
    var singleB = BlockrSelect.single(document.getElementById('demo-single-bordered'), {
      options: ['mean', 'sum', 'min', 'max', 'sd', 'median', 'n', 'n_distinct'],
      selected: 'mean',
      placeholder: 'Function...',
      onChange: function(v) { log('single-bordered: ' + v); }
    });
    singleB.el.classList.add('blockr-select--bordered');

    // Demo 3: Multi select, transparent
    var multi = BlockrSelect.multi(document.getElementById('demo-multi'), {
      options: ['setosa', 'versicolor', 'virginica', 'unknown_a', 'unknown_b'],
      selected: ['setosa', 'versicolor'],
      placeholder: 'Select values...',
      reorderable: true,
      onChange: function(v) { log('multi: [' + v.join(', ') + ']'); }
    });

    // Demo 4: Multi select, bordered
    var multiB = BlockrSelect.multi(document.getElementById('demo-multi-bordered'), {
      options: ['Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species'],
      selected: [],
      placeholder: 'Select grouping columns...',
      reorderable: true,
      onChange: function(v) { log('multi-bordered: [' + v.join(', ') + ']'); }
    });
    multiB.el.classList.add('blockr-select--bordered');

    // Demo 5: Multi with many items
    var allCols = ['mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb'];
    var multiMany = BlockrSelect.multi(document.getElementById('demo-multi-many'), {
      options: allCols,
      selected: ['mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'],
      placeholder: 'Select columns...',
      reorderable: true,
      onChange: function(v) { log('multi-many: [' + v.join(', ') + ']'); }
    });
    multiMany.el.classList.add('blockr-select--bordered');

    // Demo 6: Dynamic update
    var dynCol = BlockrSelect.single(document.getElementById('demo-dynamic-col'), {
      options: ['Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species'],
      selected: 'Sepal.Length',
      placeholder: 'Column...',
      onChange: function(v) { log('dynamic-col: ' + v); }
    });

    // Listen for button click via Shiny
    $(document).on('click', '#update_btn', function() {
      log('--- updating options to mtcars columns ---');
      dynCol.setOptions(
        ['mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear', 'carb'],
        'mpg'
      );
    });
  "))
)

server <- function(input, output, session) {
  # Server is minimal — all logic is client-side for this demo
}

shinyApp(ui, server, options = list(port = 7860, host = "0.0.0.0"))
