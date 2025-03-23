#' Generate data (random variable) from a given distribution
#'
#' The parameters of the expected random variable are used to draw a random
#' sample from the given probability distribution.
#'
#' @param distribution The probability distribution.
#' @param n The number of values to draw from the probability distribution.
#' @param mean The mean (central tendency).
#' @param sd The standard deviation (variability).
#' @param size The target number of successful trials.
#' @param prob The probability of success in each independent and identical
#'   trial.
#'
#' @return A vector of random deviates whose length is `n`
#' @export
#'
#' @examples
#' generate_data("normal", 6, 2.5, 1.3)
#' generate_data("poisson", 6, 2.5)
#' generate_data("logistic", 6, prob = 0.5, size = 1)
#' generate_data("negbin", 6, 2.5, size = 1)
generate_data <- function(distribution, n, mean = NULL,
                          sd = NULL, size = NULL, prob = NULL) {
  # Input validation with informative error messages
  if (!is.character(distribution) || length(distribution) != 1) {
    stop("'distribution' must be a single character string", call. = FALSE)
  }

  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n %% 1 != 0) {
    stop("'n' must be a positive integer", call. = FALSE)
  }

  # Generate random data based on the specified distribution
  switch(distribution,
    "normal" = {
      if (is.null(mean) || is.null(sd)) {
        stop(
          "Both 'mean' and 'sd' must be provided for normal distribution",
          call. = FALSE
        )
      }
      stats::rnorm(n, mean = mean, sd = sd)
    },
    "poisson" = {
      if (is.null(mean)) {
        stop(
          "'mean' must be provided for poisson distribution",
          call. = FALSE
        )
      }
      stats::rpois(n, lambda = max(mean, 1))
    },
    "logistic" = {
      if (is.null(size) || is.null(prob)) {
        stop(
          "Both 'size' and 'prob' must be provided for logistic distribution",
          call. = FALSE
        )
      }
      stats::rbinom(n, size = size, prob = prob)
    },
    "negbin" = {
      if (is.null(mean) || is.null(size)) {
        stop(
          "Both 'mean' and 'size' must be provided for negative binomial distribution",
          call. = FALSE
        )
      }
      stats::rnbinom(n, size = size, mu = mean)
    },
    # Default case if an invalid distribution is specified
    stop(
      "Invalid distribution specified. Valid options are: 'normal', 'poisson', 'logistic', 'negbin'",
      call. = FALSE
    )
  )
}
