test_that("plot_pwr creates a ggplot object", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")

  # Create test data
  test_data <- data.frame(
    n = rep(1:5, each = 3),
    p.value = runif(15, 0.5, 0.9),
    sd = rep(c(1, 2, 3), 5)
  )

  colpal <- c("#00BFC4", "#F8766D", "#7CAE00")

  # Test basic functionality with grouping only
  p <- plot_pwr(test_data, colpal, group = "sd", facet = NULL)

  # Check that output is a ggplot object
  expect_true(inherits(p, "ggplot"))

  # Check that necessary plot components exist
  expect_true("GeomPoint" %in% sapply(p$layers, function(x) class(x$geom)[1]))
  expect_true("GeomLine" %in% sapply(p$layers, function(x) class(x$geom)[1]))
  expect_true("GeomHline" %in% sapply(p$layers, function(x) class(x$geom)[1]))

  # Test with faceting
  p2 <- plot_pwr(test_data, colpal, group = "sd", facet = "sd")
  expect_true(inherits(p2, "ggplot"))
  expect_true(!is.null(p2$facet))
})

test_that("plot_pwr validates inputs", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")

  colpal <- c("#00BFC4", "#F8766D", "#7CAE00")

  # Test with non-data.frame input
  expect_error(plot_pwr("not a data frame", colpal), "'data' must be a data frame")

  # Test with missing columns
  bad_data <- data.frame(a = 1:5, b = 1:5)
  expect_error(plot_pwr(bad_data, colpal), "'data' must contain columns")

  # Test with insufficient colors
  test_data <- data.frame(
    n = rep(1:5, each = 3),
    p.value = runif(15, 0.5, 0.9),
    sd = rep(c(1, 2, 3), 5)
  )
  expect_error(plot_pwr(test_data, c("#00BFC4"), group = "sd"),
               "'colpal' must be a character vector")

  # Test with invalid group column
  expect_error(plot_pwr(test_data, colpal, group = "nonexistent"),
               "'nonexistent' column not found in data")

  # Test with invalid facet column
  expect_error(plot_pwr(test_data, colpal, facet = "nonexistent"),
               "'nonexistent' column not found in data")
})

test_that("plot_pwr works with custom column names", {
  # Skip if ggplot2 is not available
  skip_if_not_installed("ggplot2")

  # Create test data with non-standard column names
  test_data <- data.frame(
    sample_size = rep(1:5, each = 2),
    power = runif(10, 0.5, 0.9),
    std_dev = rep(c(1, 2), 5)
  )

  colpal <- c("#00BFC4", "#F8766D")

  # Test with custom column names
  p <- plot_pwr(test_data, colpal,
                n = "sample_size",
                p.value = "power",
                group = "std_dev")

  # Check that output is a ggplot object
  expect_true(inherits(p, "ggplot"))

  # Test with no grouping
  p2 <- plot_pwr(test_data, colpal,
                 n = "sample_size",
                 p.value = "power",
                 group = NULL)

  expect_true(inherits(p2, "ggplot"))
})
