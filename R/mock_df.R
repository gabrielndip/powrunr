#' Generate data frame with treatment and response random variable
#'
#' Creates a data frame with a treatment factor and response variable for
#' use in simulation studies. The function generates data for a control group and
#' multiple treatment groups with specified effects.
#'
#' @param distribution The distribution from which to generate the random
#'   variable. Options are "normal", "poisson", "logistic", or "negbin".
#' @param ntrts The number of treatments (including control).
#' @param n The number of observations per treatment group.
#' @param mean The mean of the response random variable for the control group.
#' @param sd The standard deviation of the response random variable (for normal distribution).
#' @param max_trt_eff The maximum treatment effect (difference from control).
#' @param size The target number of successful trials (for negative binomial and logistic).
#' @param prob Probability of success (for negative binomial and logistic).
#'
#' @return A data frame with columns 'treatment' and 'y'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#'
#' @examples
#' mock_df("normal", 3, 3, 3, 1.5, 0.4)
#' mock_df("poisson", 3, 3, 1.5)
#' mock_df("logistic", 3, 3, prob = 0.5, size = 1)
#' mock_df("negbin", 3, 3, 1.5, size = 1.5)
mock_df <- function(distribution = "normal",
                    ntrts,
                    n,
                    mean = NULL,
                    sd = NULL,
                    max_trt_eff = 0,
                    size = NULL,
                    prob = NULL) {

  # Input validation
  if (!is.numeric(ntrts) || ntrts < 2 || ntrts %% 1 != 0) {
    stop("'ntrts' must be an integer >= 2 (including control group)", call. = FALSE)
  }

  if (!is.numeric(n) || n <= 0 || n %% 1 != 0) {
    stop("'n' must be a positive integer", call. = FALSE)
  }

  # One treatment group is the control, so we have ntrts-1 treatment groups
  n_treatment_groups <- ntrts - 1

  # Generate control group
  control <- tibble::tibble(
    treatment = rep("control", n),
    y = generate_data(distribution, n, mean, sd, size, prob)
  )

  # Generate treatment labels (excluding control)
  treatments <- paste0("trt_", seq_len(n_treatment_groups))

  # Calculate treatment effects that scale linearly from min to max effect
  trt_effects <- seq(
    max_trt_eff / n_treatment_groups,  # Minimum effect
    max_trt_eff,                      # Maximum effect
    length.out = n_treatment_groups
  )

  # For logistic/negbin, adjust probability for treatment groups
  if (distribution %in% c("logistic", "negbin") && !is.null(prob)) {
    # Scale probabilities for treatment groups (increase by a small amount)
    prob_increment <- (0.9 - prob) / n_treatment_groups
    probs <- prob + (1:n_treatment_groups) * prob_increment
    probs <- pmin(probs, 0.9)  # Cap at 0.9 to keep reasonable
  } else {
    # If not using probability-based distributions, create dummy values
    probs <- rep(prob, n_treatment_groups)
  }

  # Generate data for each treatment group
  treatment_data <- lapply(1:n_treatment_groups, function(i) {
    # For mean-based distributions, subtract the treatment effect
    effect_mean <- if (!is.null(mean)) mean - trt_effects[i] else NULL

    tibble::tibble(
      treatment = treatments[i],
      y = generate_data(distribution, n,
                        mean = effect_mean,
                        sd = sd,
                        size = size,
                        prob = probs[i])
    )
  })

  # Combine control and treatment groups
  output <- dplyr::bind_rows(control, treatment_data)

  return(output)
}
