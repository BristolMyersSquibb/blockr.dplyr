# Drive dev/select-playground.html through headless Chrome and check what
# happy-dom cannot: where the panel lands, that it flips near the bottom,
# that it follows scroll, that the single-line fit hides tags, and that a
# tag can be dragged. Screenshots go to `out`, one per case and scheme.
#
#   python3 -m http.server 3841 --bind 127.0.0.1   # from /workspace
#   Rscript dev/select-playground-check.R \
#     "http://127.0.0.1:3841/_worktrees/dplyr-select/dev/select-playground.html?tokens=/blockr.ui/inst/assets/css" \
#     /tmp/select-rewrite
#
# Every check prints "ok" or "FAIL" with the numbers; the script exits
# non-zero if any failed.

args <- commandArgs(trailingOnly = TRUE)
url <- args[[1]]
out <- if (length(args) > 1) args[[2]] else tempdir()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(chromote)

failed <- 0L
check <- function(name, cond, detail = "") {
  ok <- isTRUE(cond)
  if (!ok) failed <<- failed + 1L
  cat(sprintf("%-4s %s%s\n", if (ok) "ok" else "FAIL", name,
              if (nzchar(detail)) paste0("  [", detail, "]") else ""))
}

run_scheme <- function(dark) {
  b <- ChromoteSession$new(width = 900, height = 700)
  on.exit(b$close(), add = TRUE)
  tag <- if (dark) "dark" else "light"
  # Register the wait before navigating: a fast load fires the event before
  # a wait registered afterwards would see it.
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0(url, if (dark) "&dark=1" else ""), wait_ = FALSE)
  b$wait_for(loaded)
  Sys.sleep(0.3)

  js <- function(expr) {
    r <- b$Runtime$evaluate(expr, returnByValue = TRUE, awaitPromise = TRUE)
    if (!is.null(r$exceptionDetails)) stop(r$exceptionDetails$exception$description)
    r$result$value
  }
  # The viewport as it is, not the whole document: the cases near the bottom
  # are about where the panel lands in a scrolled window.
  shot <- function(name) {
    f <- file.path(out, sprintf("%s-%s.png", name, tag))
    png <- b$Page$captureScreenshot(format = "png", captureBeyondViewport = FALSE)$data
    writeBin(jsonlite::base64_dec(png), f)
    cat("     screenshot", f, "\n")
  }
  rect <- function(sel) {
    js(sprintf("(() => { const r = document.querySelector(%s).getBoundingClientRect();
      return { top: r.top, bottom: r.bottom, left: r.left, width: r.width, height: r.height }; })()",
      jsonlite::toJSON(sel, auto_unbox = TRUE)))
  }
  click <- function(sel) {
    js(sprintf("document.querySelector(%s).click()", jsonlite::toJSON(sel, auto_unbox = TRUE)))
    Sys.sleep(0.15)
  }
  near <- function(a, b, tol = 1) abs(a - b) <= tol
  dropdown <- function(id) sprintf("#%s .blockr-select__search", id)
  panel_of <- function(id) {
    js(sprintf("'#' + document.querySelector('#%s .blockr-select__search').getAttribute('aria-controls')", id))
  }
  # Closed panels stay in <body>, hidden; the open one is the one shown.
  open_panel <- '.blockr-select__dropdown[style*="display: block"]'

  # A single under its control: 4px gap, same left and width.
  click("#single .blockr-select__control")
  ctl <- rect("#single .blockr-select")
  dd <- rect(panel_of("single"))
  check(paste("single: panel under the control", tag),
        near(dd$top, ctl$bottom + 4) && near(dd$left, ctl$left) && near(dd$width, ctl$width),
        sprintf("gap %.1f dx %.1f dw %.1f", dd$top - ctl$bottom, dd$left - ctl$left, dd$width - ctl$width))
  check(paste("single: highlight on the pick", tag),
        js("document.querySelector('.blockr-select__option--highlighted')?.dataset.value") == "CHG")
  shot("single-open")

  # Follows window scroll while open.
  js("window.scrollBy(0, 30)"); Sys.sleep(0.1)
  ctl2 <- rect("#single .blockr-select"); dd2 <- rect(panel_of("single"))
  check(paste("single: follows window scroll", tag), near(dd2$top, ctl2$bottom + 4),
        sprintf("gap %.1f", dd2$top - ctl2$bottom))
  js("window.scrollTo(0, 0)")
  js("document.body.click()"); Sys.sleep(0.1)
  check(paste("single: outside click closes", tag),
        !js("document.querySelector('#single .blockr-select').classList.contains('blockr-select--open')"))

  # Bordered single with a placeholder.
  click("#bordered .blockr-select__control")
  shot("bordered-open")
  js("document.body.click()")

  # Inside a scrolling box: the panel follows the box's scroll.
  click("#inscroll .blockr-select__control")
  dd <- rect(panel_of("inscroll"))
  js("document.getElementById('scroller').scrollTop = 40"); Sys.sleep(0.15)
  dd2 <- rect(panel_of("inscroll"))
  check(paste("scroll box: panel follows", tag), near(dd2$top, dd$top - 40),
        sprintf("moved %.1f", dd2$top - dd$top))
  js("document.getElementById('scroller').scrollTop = 0")
  js("document.body.click()")

  # Near the bottom: flips above.
  js("document.getElementById('bottom').scrollIntoView(); window.scrollBy(0, -80)")
  Sys.sleep(0.1)
  click("#single-bottom .blockr-select__control")
  ctl <- rect("#single-bottom .blockr-select")
  dd <- rect(panel_of("single-bottom"))
  above <- js("document.querySelector('#single-bottom .blockr-select').classList.contains('blockr-select--above')")
  check(paste("bottom single: flips above", tag),
        above && near(dd$bottom, ctl$top - 4),
        sprintf("above=%s bottom-gap %.1f", above, ctl$top - dd$bottom))
  shot("single-flipped")
  js("document.body.click()")

  # A menu from a word: content-sized, left on the word.
  click("#word-bottom")
  w <- rect("#word-bottom")
  dd <- rect(open_panel)
  check(paste("bottom menu: flips above, left on the word", tag),
        near(dd$bottom, w$top - 4) && near(dd$left, w$left) && dd$width >= 190 && dd$width <= 320,
        sprintf("bottom-gap %.1f dx %.1f width %.0f", w$top - dd$bottom, dd$left - w$left, dd$width))
  check(paste("menu: title and filter box", tag),
        js("!!document.querySelector('.blockr-select__menu-title') && !document.querySelector('.blockr-select__search--menu').classList.contains('blockr-select__search--offscreen')"))
  shot("menu-flipped")
  js("document.body.click()"); Sys.sleep(0.1)
  js("window.scrollTo(0, 0)"); Sys.sleep(0.1)

  click("#word-multi")
  shot("menu-multi")
  check(paste("multi menu: tags in the head", tag),
        js("document.querySelectorAll('.blockr-select__dropdown .blockr-select__tag').length") == 2)
  js("document.body.click()"); Sys.sleep(0.1)

  # Single-line: some tags hidden behind the chip; the chip expands.
  hidden <- js("document.querySelectorAll('#line .blockr-select__tag--hidden').length")
  chip <- js("document.querySelector('#line .blockr-select__more')?.textContent || ''")
  check(paste("single-line: tags hidden behind the chip", tag),
        hidden > 0 && chip == paste0("+", hidden), sprintf("hidden %d chip %s", hidden, chip))
  line <- rect("#line .blockr-select__control")
  check(paste("single-line: one row", tag), line$height < 50, sprintf("height %.0f", line$height))
  shot("single-line")
  click("#line .blockr-select__more")
  check(paste("single-line: chip expands", tag),
        js("document.querySelector('#line .blockr-select').classList.contains('blockr-select--expanded')") &&
          js("document.querySelectorAll('#line .blockr-select__tag--hidden').length") == 0)
  shot("single-line-expanded")
  js("document.body.click()"); Sys.sleep(0.1)
  check(paste("single-line: collapses on an outside click", tag),
        js("document.querySelectorAll('#line .blockr-select__tag--hidden').length") == hidden)

  # Multi: open, panel follows a tag row being added.
  click("#multi .blockr-select__control")
  shot("multi-open")
  dd <- rect(panel_of("multi"))
  click(sprintf("%s .blockr-select__option", panel_of("multi")))
  Sys.sleep(0.2)
  ctl <- rect("#multi .blockr-select"); dd2 <- rect(panel_of("multi"))
  check(paste("multi: stays open after a pick, panel follows the control", tag),
        js("document.querySelector('#multi .blockr-select').classList.contains('blockr-select--open')") &&
          near(dd2$top, ctl$bottom + 4), sprintf("gap %.1f", dd2$top - ctl$bottom))
  js("document.body.click()"); Sys.sleep(0.1)

  # Drag the first tag onto the third with real mouse events: headless
  # Chromium runs native drag and drop off dispatchMouseEvent.
  before <- unlist(js("[...document.querySelectorAll('#multi .blockr-select__tag')].map(t => t.dataset.value)"))
  from <- rect("#multi .blockr-select__tag:nth-child(1)")
  to <- rect("#multi .blockr-select__tag:nth-child(3)")
  x0 <- from$left + 10; y0 <- from$top + 10
  # Aim at the right half of the target, away from its edges: the drop
  # indicator is an 8px margin, which shifts the tags under the pointer, and
  # a pointer that ends up over the gap between tags drops nowhere. The last
  # small move after a pause makes a dragover fire at the settled layout.
  x1 <- to$left + to$width * 0.75; y1 <- to$top + 10
  b$Input$dispatchMouseEvent(type = "mousePressed", x = x0, y = y0, button = "left", clickCount = 1)
  for (i in 1:8) {
    b$Input$dispatchMouseEvent(type = "mouseMoved", x = x0 + i * (x1 - x0) / 8, y = y1, button = "left")
    Sys.sleep(0.03)
  }
  Sys.sleep(0.1)
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = x1 + 1, y = y1, button = "left")
  Sys.sleep(0.1)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = x1 + 1, y = y1, button = "left")
  Sys.sleep(0.2)
  after <- unlist(js("[...document.querySelectorAll('#multi .blockr-select__tag')].map(t => t.dataset.value)"))
  reported <- js("(window.__last && window.__last.multi) || null")
  check(paste("multi: drag moves the tag and reports the order", tag),
        !identical(after, before) && setequal(after, before) && identical(unlist(reported), after),
        paste(before[1:3], collapse = ",") %.% " -> " %.% paste(after[1:3], collapse = ","))
  shot("multi-reordered")

  errors <- js("window.__errors || []")
  check(paste("no console errors", tag), length(errors) == 0, paste(unlist(errors), collapse = "; "))
}

`%.%` <- function(a, b) paste0(a, b)

run_scheme(dark = FALSE)
run_scheme(dark = TRUE)

cat(if (failed) sprintf("\n%d check(s) FAILED\n", failed) else "\nall checks passed\n")
quit(status = if (failed) 1L else 0L)
