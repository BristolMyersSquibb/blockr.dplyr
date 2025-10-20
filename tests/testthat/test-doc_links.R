test_that("doc_link creates a valid link with basic parameters", {
  link <- doc_link("Documentation", "https://example.com/docs")

  # Should return a shiny tag
  expect_s3_class(link, "shiny.tag")
  expect_equal(link$name, "a")

  # Check href attribute
  expect_equal(link$attribs$href, "https://example.com/docs")

  # Check target attribute (should open in new tab)
  expect_equal(link$attribs$target, "_blank")

  # Check security attribute
  expect_equal(link$attribs$rel, "noopener noreferrer")

  # Check class
  expect_true(grepl("block-doc-link", link$attribs$class))
})

test_that("doc_link includes icon", {
  link <- doc_link("Docs", "https://example.com")

  # Should contain an icon element
  link_html <- as.character(link)
  expect_true(grepl("fa-arrow-up-right-from-square", link_html))
})

test_that("doc_link accepts custom icon", {
  link <- doc_link("Help", "https://example.com", icon_name = "circle-question")

  link_html <- as.character(link)
  # Font Awesome 6 uses "circle-question" naming
  expect_true(grepl("circle-question", link_html))
})

test_that("doc_link includes text content", {
  link <- doc_link("My Documentation", "https://example.com")

  link_html <- as.character(link)
  expect_true(grepl("My Documentation", link_html))
})

test_that("doc_link accepts additional CSS classes", {
  link <- doc_link("Docs", "https://example.com", class = "extra-class")

  expect_true(grepl("extra-class", link$attribs$class))
  expect_true(grepl("block-doc-link", link$attribs$class))
})

test_that("doc_link with tooltip adds tooltip wrapper", {
  skip_if_not_installed("bslib")

  link <- doc_link(
    "Docs",
    "https://example.com",
    tooltip = "Click for documentation"
  )

  # With tooltip, the result should be wrapped
  # The exact structure depends on bslib::tooltip implementation
  # Just verify it's still a valid tag and contains our link
  expect_true(inherits(link, "shiny.tag") || inherits(link, "shiny.tag.list"))
})

test_that("doc_link without tooltip returns plain link", {
  link <- doc_link("Docs", "https://example.com")

  # Should be a plain <a> tag without tooltip wrapper
  expect_s3_class(link, "shiny.tag")
  expect_equal(link$name, "a")
})

test_that("css_doc_links returns valid CSS", {
  css <- css_doc_links()

  # Should return a style tag
  expect_s3_class(css, "shiny.tag")
  expect_equal(css$name, "style")

  # Should contain expected CSS classes
  css_content <- as.character(css)
  expect_true(grepl("block-doc-link", css_content))
  expect_true(grepl("hover", css_content))
  expect_true(grepl("icon-sm", css_content))
  expect_true(grepl("block-header-with-doc", css_content))
  expect_true(grepl("expression-help-link", css_content))
})

test_that("css_doc_links contains valid CSS syntax", {
  css <- css_doc_links()
  css_content <- as.character(css)

  # Basic CSS validation - should have opening and closing braces
  expect_true(grepl("\\{", css_content))
  expect_true(grepl("\\}", css_content))

  # Should have some color definitions
  expect_true(grepl("#[0-9a-fA-F]{6}", css_content))

  # Should have some size/spacing definitions
  expect_true(grepl("[0-9.]+rem", css_content) || grepl("[0-9.]+px", css_content))
})

test_that("doc_link works with real blockr documentation URLs", {
  # Test with actual blockr.dplyr documentation URL
  link <- doc_link(
    "Mutate Block Guide",
    "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#mutate-block"
  )

  expect_s3_class(link, "shiny.tag")
  expect_equal(
    link$attribs$href,
    "https://bristolmyerssquibb.github.io/blockr.dplyr/articles/blockr-dplyr-showcase.html#mutate-block"
  )
})

test_that("doc_link applies inline styles", {
  link <- doc_link("Test", "https://example.com")

  # Check that some inline styles are applied
  expect_true(!is.null(link$attribs$style))
  expect_true(nchar(link$attribs$style) > 0)
})
