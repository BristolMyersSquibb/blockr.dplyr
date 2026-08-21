# Half of every block is JavaScript, so half of its tests are too (tests/js/).
# Running them from testthat as well means `devtools::test()` and CI see a red
# light when a block's restore contract breaks, instead of a JS suite quietly
# rotting next to a `npm test` nobody thinks to run.
#
# `npm install` first (devDependency: happy-dom); without it this skips.
test_that("the blocks keep their restore contract (node --test)", {

  skip_on_cran()

  node <- Sys.which("node")
  skip_if(!nzchar(node), "node is not installed")

  js_dir <- testthat::test_path("..", "js")
  skip_if(!dir.exists(js_dir), "tests/js is not present")

  modules <- file.path(js_dir, "..", "..", "node_modules", "happy-dom")
  skip_if(!dir.exists(modules), "happy-dom is not installed (npm install)")

  files <- list.files(js_dir, pattern = "[.]test[.]js$", full.names = TRUE)
  skip_if(!length(files), "no JS tests found")

  out <- suppressWarnings(
    system2(node, c("--test", shQuote(files)), stdout = TRUE, stderr = TRUE)
  )
  status <- attr(out, "status")

  expect_true(
    is.null(status) || identical(status, 0L),
    info = paste(out, collapse = "\n")
  )
})
