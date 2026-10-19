# Blocks in inst/blocks/<name>/ follow the block contract shared with blockx
# (blockr.ui::new_js_block()): block.json declares the state, and
# examples.json pairs states with the dplyr code they give. blockx checks its
# own plans against the same examples, so a change to an expression builder
# that changes the code has to update examples.json.

block_dirs <- list.dirs(
  system.file("blocks", package = "blockr.dplyr"),
  full.names = FALSE, recursive = FALSE
)

# An example's input, by the name of a data set in dplyr or datasets.
example_data <- function(name) {
  tryCatch(
    getExportedValue("dplyr", name),
    error = function(e) getExportedValue("datasets", name)
  )
}

block_ctor <- function(name) {
  get(paste0("new_", gsub("-", "_", name), "_block"),
      envir = asNamespace("blockr.dplyr"))
}

test_that("there are contract blocks", {
  expect_true(all(c("select", "filter", "join") %in% block_dirs))
})

for (name in block_dirs) {
  local({
    nm <- name
    manifest <- blockr.ui::js_block_manifest(nm, "blockr.dplyr")
    ctor <- block_ctor(nm)

    test_that(paste(nm, "constructor matches its schema"), {
      args <- formals(ctor)
      args <- args[names(args) != "..."]
      expect_identical(names(args), names(manifest$state$properties))
      defaults <- lapply(args, eval)
      # the constructor's defaults are the schema's
      expect_identical(
        blockr.ui::coerce_state(defaults, manifest$state),
        blockr.ui::coerce_state(list(), manifest$state)
      )
    })

    test_that(paste(nm, "gives the code of its examples"), {
      path <- system.file("blocks", nm, "examples.json",
                          package = "blockr.dplyr")
      examples <- jsonlite::read_json(path, simplifyVector = FALSE)
      slots <- unlist(manifest$inputs)
      for (ex in examples) {
        # called by name: blockr.core records the constructor from the call
        blk <- eval(
          as.call(c(as.name(paste0("new_", nm, "_block")), ex$state)),
          asNamespace("blockr.dplyr")
        )
        shiny::testServer(
          blk$expr_server,
          args = stats::setNames(
            lapply(slots, function(i) shiny::reactive(NULL)), slots
          ),
          {
            e <- session$returned$expr()
            e <- eval(call("bquote", e,
                           lapply(stats::setNames(nm = slots), as.name)))
            # as text: the builders inline constants such as c("a", "b")
            expect_identical(
              deparse1(e, width.cutoff = 500L),
              deparse1(str2lang(ex$code), width.cutoff = 500L)
            )
            # and the code runs on the example's inputs
            env <- list2env(lapply(ex$inputs, example_data))
            expect_s3_class(eval(str2lang(ex$code), env), "data.frame")
          }
        )
      }
    })
  })
}
