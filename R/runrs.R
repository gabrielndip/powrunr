#' ANOVA-based power simulation
#'
#' Runs simulations to estimate the statistical power of ANOVA tests for
#' detecting differences between treatment groups. The function generates
#' data from a specified distribution, performs ANOVA analysis, and returns
#' the results of post-hoc tests.
#'
#' @inheritParams mock_df
#' @param num_sim Number of simulations to perform.
#'
#' @return A data frame of contrasts with p-values from post-hoc tests.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple example with 5 simulations
#' anova_runr(num_sim = 5, n = 10, mean = 100, sd = 15, max_trt_eff = 20, ntrts = 5)
#'
#' # Calculate power by summarizing results
#' sims <- anova_runr(num_sim = 20, n = 10, mean = 100, sd = 15, max_trt_eff = 20, ntrts = 3)
#' power <- mean(sims$p.value < 0.05, na.rm = TRUE)
#' }
anova_runr <- function(num_sim,
                       n,
                       mean,
                       sd,
                       max_trt_eff,
                       ntrts,
                       distribution = "normal",
                       prob = NULL,
                       size = NULL) {
  # Input validation
  if (!is.numeric(num_sim) || num_sim <= 0 || num_sim %% 1 != 0) {
    stop("'num_sim' must be a positive integer", call. = FALSE)
  }

  # Function to run a single simulation iteration
  run_simulation <- function() {
    # Generate data using mock_df
    data <- mock_df(
      distribution = distribution,
      ntrts = ntrts,
      n = n,
      mean = mean,
      sd = sd,
      max_trt_eff = max_trt_eff,
      size = size,
      prob = prob
    )

    # Fit ANOVA model
    model <- tryCatch(
      {
        stats::aov(y ~ treatment, data = data)
      },
      error = function(e) {
        warning("ANOVA model fitting failed: ", e$message, call. = FALSE)
        return(NULL)
      }
    )

    # If model fitting failed, return NA results
    if (is.null(model)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        df = NA_real_,
        t.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        sd = sd,
        max_trt_eff = max_trt_eff
      ))
    }

    # Perform post-hoc tests using emmeans
    posthoc <- tryCatch(
      {
        emmeans::emmeans(model, specs = "treatment") |>
          emmeans::contrast(method = "trt.vs.ctrl")
      },
      error = function(e) {
        warning("Post-hoc test failed: ", e$message, call. = FALSE)
        return(NULL)
      }
    )

    # If post-hoc tests failed, return NA results
    if (is.null(posthoc)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        df = NA_real_,
        t.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        sd = sd,
        max_trt_eff = max_trt_eff
      ))
    }

    # Convert emmeans results to data frame and add metadata
    result <- as.data.frame(posthoc) |>
      tibble::as_tibble() |>
      dplyr::mutate(n = n, sd = sd, max_trt_eff = max_trt_eff)

    return(result)
  }

  # Run the simulation num_sim times and combine results
  results <- lapply(seq_len(num_sim), function(x) run_simulation())
  output <- dplyr::bind_rows(results)

  return(output)
}


#-----------------------------------------------------------------------------------------


#' Dunnett-based power simulation
#'
#' Runs simulations to estimate the statistical power of Dunnett's test for
#' comparing multiple treatment groups to a control. The function generates
#' data from a specified distribution, performs ANOVA analysis, and returns
#' the results of Dunnett's post-hoc tests.
#'
#' @inheritParams mock_df
#' @param num_sim Number of simulations to perform.
#'
#' @return A data frame of contrasts with p-values from Dunnett's tests.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple example with 5 simulations
#' dunnett_runr(num_sim = 5, n = 10, mean = 100, sd = 15, max_trt_eff = 20, ntrts = 5)
#'
#' # Calculate power by summarizing results
#' sims <- dunnett_runr(num_sim = 20, n = 10, mean = 100, sd = 15, max_trt_eff = 20, ntrts = 3)
#' power <- mean(sims$p.value < 0.05, na.rm = TRUE)
#' }
dunnett_runr <- function(num_sim,
                         n,
                         mean,
                         sd,
                         max_trt_eff,
                         ntrts,
                         distribution = "normal",
                         prob = NULL,
                         size = NULL) {
  # Input validation
  if (!is.numeric(num_sim) || num_sim <= 0 || num_sim %% 1 != 0) {
    stop("'num_sim' must be a positive integer", call. = FALSE)
  }

  # Function to run a single simulation iteration
  run_simulation <- function() {
    # Generate data using mock_df
    data <- mock_df(
      distribution = distribution,
      ntrts = ntrts,
      n = n,
      mean = mean,
      sd = sd,
      max_trt_eff = max_trt_eff,
      size = size,
      prob = prob
    )

    # Fit ANOVA model
    model <- tryCatch(
      {
        stats::aov(y ~ treatment, data = data)
      },
      error = function(e) {
        warning("ANOVA model fitting failed: ", e$message, call. = FALSE)
        return(NULL)
      }
    )

    # If model fitting failed, return NA results
    if (is.null(model)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        df = NA_real_,
        t.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        sd = sd,
        max_trt_eff = max_trt_eff
      ))
    }

    # Perform Dunnett's test using emmeans
    posthoc <- tryCatch(
      {
        emmeans::emmeans(model, specs = "treatment") |>
          emmeans::contrast(method = "dunnett")
      },
      error = function(e) {
        warning("Dunnett's test failed: ", e$message, call. = FALSE)
        return(NULL)
      }
    )

    # If post-hoc tests failed, return NA results
    if (is.null(posthoc)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        df = NA_real_,
        t.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        sd = sd,
        max_trt_eff = max_trt_eff
      ))
    }

    # Convert emmeans results to data frame and add metadata
    result <- as.data.frame(posthoc) |>
      tibble::as_tibble() |>
      dplyr::mutate(n = n, sd = sd, max_trt_eff = max_trt_eff)

    return(result)
  }

  # Run the simulation num_sim times and combine results
  results <- lapply(seq_len(num_sim), function(x) run_simulation())
  output <- dplyr::bind_rows(results)

  return(output)
}


#-----------------------------------------------------------------------------------------


#' Negative binomial regression power simulation
#'
#' Runs simulations to estimate the statistical power of negative binomial regression
#' for detecting differences between treatment groups with count data. The function
#' generates data from a negative binomial distribution, performs regression analysis,
#' and returns the results of post-hoc tests.
#'
#' @inheritParams mock_df
#' @param num_sim Number of simulations to perform.
#'
#' @return A data frame of contrasts with p-values from post-hoc tests.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple example with 5 simulations
#' nb_runr(num_sim = 5, n = 10, mean = 5, sd = 1, max_trt_eff = 2,
#'        ntrts = 3, size = 2, distribution = "negbin")
#'
#' # Calculate power by summarizing results
#' sims <- nb_runr(num_sim = 20, n = 10, mean = 5, max_trt_eff = 2,
#'                ntrts = 3, size = 2, distribution = "negbin")
#' power <- mean(sims$p.value < 0.05, na.rm = TRUE)
#' }
nb_runr <- function(num_sim,
                    n,
                    mean,
                    sd = NULL,
                    max_trt_eff,
                    ntrts,
                    distribution = "negbin",
                    size = 2,
                    prob = NULL) {

  # Input validation
  if (!is.numeric(num_sim) || num_sim <= 0 || num_sim %% 1 != 0) {
    stop("'num_sim' must be a positive integer", call. = FALSE)
  }

  if (distribution != "negbin") {
    warning("nb_runr is designed for negative binomial data. Forcing distribution = 'negbin'.",
            call. = FALSE)
    distribution <- "negbin"
  }

  # Function to run a single simulation iteration
  run_simulation <- function() {
    # Generate data using mock_df
    data <- mock_df(distribution = distribution,
                    ntrts = ntrts,
                    n = n,
                    mean = mean,
                    sd = sd,
                    max_trt_eff = max_trt_eff,
                    size = size,
                    prob = prob)

    # Fit negative binomial model with improved parameters for convergence
    model <- tryCatch({
      # Use control parameters available in glm.nb directly instead of glm.control
      MASS::glm.nb(y ~ treatment, data = data, maxit = 50)
    }, error = function(e) {
      warning("Negative binomial model fitting failed: ", e$message, call. = FALSE)
      return(NULL)
    }, warning = function(w) {
      # Capture warnings but continue with the result
      warning("Warning in model fitting: ", w$message, call. = FALSE)
      return(NULL)
    })

    # If model fitting failed, return NA results
    if (is.null(model)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        z.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        sd = if(is.null(sd)) NA_real_ else sd,
        max_trt_eff = max_trt_eff,
        size = size
      ))
    }

    # Perform post-hoc tests using emmeans
    posthoc <- tryCatch({
      emmeans::emmeans(model, specs = "treatment") |>
        emmeans::contrast(method = "trt.vs.ctrl")
    }, error = function(e) {
      warning("Post-hoc test failed: ", e$message, call. = FALSE)
      return(NULL)
    })

    # If post-hoc tests failed, return NA results
    if (is.null(posthoc)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        z.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        sd = if(is.null(sd)) NA_real_ else sd,
        max_trt_eff = max_trt_eff,
        size = size
      ))
    }

    # Convert emmeans results to data frame and add metadata
    result <- as.data.frame(posthoc) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        n = n,
        sd = if(is.null(sd)) NA_real_ else sd,
        max_trt_eff = max_trt_eff,
        size = size
      )

    return(result)
  }

  # Run the simulation num_sim times and combine results
  results <- lapply(seq_len(num_sim), function(x) run_simulation())
  output <- dplyr::bind_rows(results)

  return(output)
}


#-----------------------------------------------------------------------------------------


#' Poisson regression power simulation
#'
#' Runs simulations to estimate the statistical power of Poisson regression
#' for detecting differences between treatment groups with count data. The function
#' generates data from a Poisson distribution, performs regression analysis,
#' and returns the results of post-hoc tests.
#'
#' @inheritParams mock_df
#' @param num_sim Number of simulations to perform.
#'
#' @return A data frame of contrasts with p-values from post-hoc tests.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple example with 5 simulations
#' pois_runr(
#'   num_sim = 5, n = 10, mean = 5, max_trt_eff = 2,
#'   ntrts = 3, distribution = "poisson"
#' )
#'
#' # Calculate power by summarizing results
#' sims <- pois_runr(
#'   num_sim = 20, n = 10, mean = 5, max_trt_eff = 2,
#'   ntrts = 3, distribution = "poisson"
#' )
#' power <- mean(sims$p.value < 0.05, na.rm = TRUE)
#' }
pois_runr <- function(num_sim,
                      n,
                      mean,
                      sd = NULL,
                      max_trt_eff,
                      ntrts,
                      distribution = "poisson",
                      size = NULL,
                      prob = NULL) {
  # Input validation
  if (!is.numeric(num_sim) || num_sim <= 0 || num_sim %% 1 != 0) {
    stop("'num_sim' must be a positive integer", call. = FALSE)
  }

  if (distribution != "poisson") {
    warning("pois_runr is designed for Poisson data. Forcing distribution = 'poisson'.",
      call. = FALSE
    )
    distribution <- "poisson"
  }

  # Function to run a single simulation iteration
  run_simulation <- function() {
    # Generate data using mock_df
    data <- mock_df(
      distribution = distribution,
      ntrts = ntrts,
      n = n,
      mean = mean,
      sd = sd,
      max_trt_eff = max_trt_eff,
      size = size,
      prob = prob
    )

    # Fit Poisson model
    model <- tryCatch(
      {
        stats::glm(y ~ treatment, family = stats::poisson(link = "log"), data = data)
      },
      error = function(e) {
        warning("Poisson model fitting failed: ", e$message, call. = FALSE)
        return(NULL)
      }
    )

    # If model fitting failed, return NA results
    if (is.null(model)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        z.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        sd = if (is.null(sd)) NA_real_ else sd,
        max_trt_eff = max_trt_eff
      ))
    }

    # Perform post-hoc tests using emmeans
    posthoc <- tryCatch(
      {
        emmeans::emmeans(model, specs = "treatment") |>
          emmeans::contrast(method = "trt.vs.ctrl")
      },
      error = function(e) {
        warning("Post-hoc test failed: ", e$message, call. = FALSE)
        return(NULL)
      }
    )

    # If post-hoc tests failed, return NA results
    if (is.null(posthoc)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        z.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        sd = if (is.null(sd)) NA_real_ else sd,
        max_trt_eff = max_trt_eff
      ))
    }

    # Convert emmeans results to data frame and add metadata
    result <- as.data.frame(posthoc) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        n = n,
        sd = if (is.null(sd)) NA_real_ else sd,
        max_trt_eff = max_trt_eff
      )

    return(result)
  }

  # Run the simulation num_sim times and combine results
  results <- lapply(seq_len(num_sim), function(x) run_simulation())
  output <- dplyr::bind_rows(results)

  return(output)
}


#-----------------------------------------------------------------------------------------

#' Logistic regression power simulation
#'
#' Runs simulations to estimate the statistical power of logistic regression
#' for detecting differences between treatment groups with binary data. The function
#' generates data from a binomial distribution, performs logistic regression analysis,
#' and returns the results of post-hoc tests.
#'
#' @inheritParams mock_df
#' @param num_sim Number of simulations to perform.
#'
#' @return A data frame of contrasts with p-values from post-hoc tests.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple example with 5 simulations
#' logit_runr(
#'   num_sim = 5, n = 20, ntrts = 3, distribution = "logistic",
#'   size = 1, prob = 0.7, max_trt_eff = 0.2
#' )
#'
#' # Calculate power by summarizing results
#' sims <- logit_runr(
#'   num_sim = 20, n = 20, ntrts = 3, distribution = "logistic",
#'   size = 1, prob = 0.7, max_trt_eff = 0.2
#' )
#' power <- mean(sims$p.value < 0.05, na.rm = TRUE)
#' }
logit_runr <- function(num_sim,
                       n,
                       mean = NULL,
                       sd = NULL,
                       max_trt_eff,
                       ntrts,
                       distribution = "logistic",
                       size = 1,
                       prob = 0.5) {
  # Input validation
  if (!is.numeric(num_sim) || num_sim <= 0 || num_sim %% 1 != 0) {
    stop("'num_sim' must be a positive integer", call. = FALSE)
  }

  if (distribution != "logistic") {
    warning("logit_runr is designed for binary data. Forcing distribution = 'logistic'.",
      call. = FALSE
    )
    distribution <- "logistic"
  }

  if (is.null(prob)) {
    stop("'prob' must be provided for logistic regression", call. = FALSE)
  }

  if (is.null(size)) {
    warning("Setting 'size' to 1 for binary outcome in logistic regression", call. = FALSE)
    size <- 1
  } else if (size != 1) {
    warning("logit_runr is typically used with binary outcomes. Setting 'size' to 1.", call. = FALSE)
    size <- 1
  }

  # Function to run a single simulation iteration
  run_simulation <- function() {
    # Generate data using mock_df
    data <- mock_df(
      distribution = distribution,
      ntrts = ntrts,
      n = n,
      mean = mean,
      sd = sd,
      max_trt_eff = max_trt_eff,
      size = size,
      prob = prob
    )

    # Fit logistic model
    model <- tryCatch(
      {
        stats::glm(y ~ treatment, family = stats::binomial(link = "logit"), data = data)
      },
      error = function(e) {
        warning("Logistic model fitting failed: ", e$message, call. = FALSE)
        return(NULL)
      }
    )

    # If model fitting failed, return NA results
    if (is.null(model)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        z.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        prob = prob,
        max_trt_eff = max_trt_eff
      ))
    }

    # Perform post-hoc tests using emmeans
    posthoc <- tryCatch(
      {
        emmeans::emmeans(model, specs = "treatment") |>
          emmeans::contrast(method = "trt.vs.ctrl")
      },
      error = function(e) {
        warning("Post-hoc test failed: ", e$message, call. = FALSE)
        return(NULL)
      }
    )

    # If post-hoc tests failed, return NA results
    if (is.null(posthoc)) {
      return(data.frame(
        contrast = NA_character_,
        estimate = NA_real_,
        SE = NA_real_,
        z.ratio = NA_real_,
        p.value = NA_real_,
        n = n,
        prob = prob,
        max_trt_eff = max_trt_eff
      ))
    }

    # Convert emmeans results to data frame and add metadata
    result <- as.data.frame(posthoc) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        n = n,
        prob = prob,
        max_trt_eff = max_trt_eff
      )

    return(result)
  }

  # Run the simulation num_sim times and combine results
  results <- lapply(seq_len(num_sim), function(x) run_simulation())
  output <- dplyr::bind_rows(results)

  return(output)
}
