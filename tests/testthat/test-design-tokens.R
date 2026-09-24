# blockr.dplyr styles itself from blockr.ui's design system and nothing else:
# every colour, size and radius in inst/css reads a meaning token that
# blockr.ui defines, with no fallback, and no colour is written out.

ui_token_file <- function() {
  system.file("assets", "css", "blockr-tokens.css", package = "blockr.ui",
              mustWork = TRUE)
}

strip_css_comments <- function(x) {
  gsub("(?s)/\\*.*?\\*/", "", x, perl = TRUE)
}

# The names blockr.ui declares, split into meaning tokens and the rest. Legacy
# aliases sit after the "legacy (aliases)" heading; palette tokens are
# `--blockr-<hue>-<step>`.
ui_tokens <- function() {
  raw <- paste(readLines(ui_token_file(), warn = FALSE), collapse = "\n")
  cut <- regexpr("legacy (aliases)", raw, fixed = TRUE)
  declared <- function(txt) {
    unique(regmatches(txt, gregexpr("--blockr-[a-z0-9-]+(?=\\s*:)", txt,
                                    perl = TRUE))[[1L]])
  }
  all <- declared(strip_css_comments(raw))
  legacy <- if (cut > 0) declared(strip_css_comments(substring(raw, cut))) else
    character()
  palette <- grep("^--blockr-(grey|blue|red|amber|green|accent)-[0-9]+$", all,
                  value = TRUE)
  list(meaning = setdiff(all, c(legacy, palette)), legacy = legacy,
       palette = palette)
}

dplyr_css <- function() {
  files <- dir(system.file("css", package = "blockr.dplyr", mustWork = TRUE),
               pattern = "\\.css$", full.names = TRUE)
  stats::setNames(
    lapply(files, function(f) {
      strip_css_comments(paste(readLines(f, warn = FALSE), collapse = "\n"))
    }),
    basename(files)
  )
}

test_that("every token read is a blockr.ui meaning token", {
  tok <- ui_tokens()
  expect_gt(length(tok$meaning), 0L)
  offending <- unlist(lapply(names(dplyr_css()), function(f) {
    reads <- regmatches(dplyr_css()[[f]],
                        gregexpr("var\\((--blockr-[a-z0-9-]+)",
                                 dplyr_css()[[f]]))[[1L]]
    reads <- unique(sub("^var\\(", "", reads))
    bad <- setdiff(reads, tok$meaning)
    if (length(bad)) paste0(f, ": ", bad)
  }))
  expect_identical(offending, NULL)
})

test_that("no token read carries a fallback", {
  offending <- unlist(lapply(names(dplyr_css()), function(f) {
    hits <- regmatches(dplyr_css()[[f]],
                       gregexpr("var\\(--blockr-[a-z0-9-]+\\s*,",
                                dplyr_css()[[f]]))[[1L]]
    if (length(hits)) paste0(f, ": ", hits)
  }))
  expect_identical(offending, NULL)
})

test_that("no colour is written out", {
  offending <- unlist(lapply(names(dplyr_css()), function(f) {
    hits <- regmatches(dplyr_css()[[f]],
                       gregexpr("#[0-9a-fA-F]{3,8}\\b|rgba?\\([^)]*\\)",
                                dplyr_css()[[f]]))[[1L]]
    if (length(hits)) paste0(f, ": ", hits)
  }))
  expect_identical(offending, NULL)
})

test_that("a block brings blockr.ui's theme with it", {
  ui <- js_block_ui("filter", shared_deps = c("select", "input"))("x")
  deps <- vapply(htmltools::findDependencies(ui), `[[`, character(1L), "name")
  expect_true("blockr-theme" %in% deps)
  expect_lt(match("blockr-theme", deps), match("blockr-blocks-css", deps))
})
