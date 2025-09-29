# Test app for enhanced filter block
# Supports both simple and complex expression testing via environment variable

# Load development version
pkgload::load_all()

library(blockr.core)

# Get expression from environment variable for flexible testing
# Default to simple expression for backwards compatibility
test_expr <- Sys.getenv("TEST_EXPRESSION", "Petal.Length > 5")

# Test with configurable initial filter expression
blockr.core::serve(
  new_enhanced_filter_block(test_expr),
  data = list(data = iris)
)