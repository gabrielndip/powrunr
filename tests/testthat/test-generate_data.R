test_that("generate_data produces correct output format", {
  # Test normal distribution
  normal_data <- generate_data("normal", n = 10, mean = 5, sd = 2)
  expect_length(normal_data, 10)
  expect_type(normal_data, "double")

  # Test poisson distribution
  poisson_data <- generate_data("poisson", n = 15, mean = 3)
  expect_length(poisson_data, 15)
  expect_type(poisson_data, "integer")

  # Test logistic distribution (binomial)
  logistic_data <- generate_data("logistic", n = 20, size = 1, prob = 0.7)
  expect_length(logistic_data, 20)
  expect_type(logistic_data, "integer")
  expect_true(all(logistic_data %in% c(0, 1)))

  # Test negative binomial distribution
  negbin_data <- generate_data("negbin", n = 25, mean = 4, size = 2)
  expect_length(negbin_data, 25)
  expect_type(negbin_data, "double")
})

test_that("generate_data validates inputs correctly", {
  # Test invalid distribution
  expect_error(generate_data("invalid_dist", 10, 5, 2), "Invalid distribution specified")

  # Test missing parameters for normal
  expect_error(generate_data("normal", 10, mean = 5), "Both 'mean' and 'sd' must be provided")

  # Test missing parameters for poisson
  expect_error(generate_data("poisson", 10), "'mean' must be provided")

  # Test missing parameters for logistic
  expect_error(generate_data("logistic", 10), "Both 'size' and 'prob' must be provided")

  # Test missing parameters for negative binomial
  expect_error(generate_data("negbin", 10, mean = 5), "Both 'mean' and 'size' must be provided")
})
