test_that("mock_df creates correct structure", {
  # Test normal distribution
  normal_data <- mock_df("normal", ntrts = 3, n = 5, mean = 10, sd = 2, max_trt_eff = 1)

  # Check structure
  expect_true(is.data.frame(normal_data))
  expect_equal(names(normal_data), c("treatment", "y"))
  expect_equal(nrow(normal_data), 3 * 5) # 3 treatments, 5 observations each

  # Check treatment levels
  expect_setequal(unique(normal_data$treatment), c("control", "trt_1", "trt_2"))

  # Check counts per treatment
  treatment_counts <- table(normal_data$treatment)
  expect_equal(treatment_counts[["control"]], 5)
  expect_equal(treatment_counts[["trt_1"]], 5)
  expect_equal(treatment_counts[["trt_2"]], 5)
})

test_that("mock_df works with different distributions", {
  # Test with Poisson
  poisson_data <- mock_df("poisson", ntrts = 2, n = 3, mean = 5)
  expect_equal(nrow(poisson_data), 2 * 3)
  expect_setequal(unique(poisson_data$treatment), c("control", "trt_1"))

  # Test with logistic
  logistic_data <- mock_df("logistic", ntrts = 4, n = 2, size = 1, prob = 0.6)
  expect_equal(nrow(logistic_data), 4 * 2)
  expect_setequal(unique(logistic_data$treatment), c("control", "trt_1", "trt_2", "trt_3"))
  expect_true(all(logistic_data$y %in% c(0, 1)))

  # Test with negative binomial
  negbin_data <- mock_df("negbin", ntrts = 3, n = 4, mean = 3, size = 2)
  expect_equal(nrow(negbin_data), 3 * 4)
  expect_setequal(unique(negbin_data$treatment), c("control", "trt_1", "trt_2"))
})

test_that("mock_df handles invalid inputs", {
  # Invalid number of treatments
  expect_error(mock_df("normal", ntrts = 1, n = 5, mean = 10, sd = 2), "must be an integer >= 2")

  # Invalid sample size
  expect_error(mock_df("normal", ntrts = 3, n = 0, mean = 10, sd = 2), "must be a positive integer")

  # This should work fine with default max_trt_eff
  expect_silent(mock_df("normal", ntrts = 2, n = 3, mean = 10, sd = 2))
})
