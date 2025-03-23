# powrunr <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/gabrielndip/powrunr/workflows/R-CMD-check/badge.svg)](https://github.com/gabrielndip/powrunr/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The goal of `powrunr` is to provide a flexible, simulation-based approach to power analysis for various statistical tests. This package helps researchers:

- Determine appropriate sample sizes for experiments
- Estimate statistical power for different effect sizes and variances
- Compare power across multiple statistical methods
- Visualize power curves to inform experimental design decisions

Unlike analytical power calculations, simulation-based approaches can handle complex designs and provide more realistic estimates for a variety of statistical methods.

## Installation

You can install the development version of powrunr from GitHub:

```r
# Install the remotes package if you haven't already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install powrunr from GitHub
remotes::install_github("gabrielndip/powrunr")
```

## Package Architecture

The package consists of three layers of functions:

1. **Data Generation Layer**:
   - `generate_data()`: Generate random data from various distributions
   - `mock_df()`: Create data frames with treatment and control groups

2. **Simulation Layer**:
   - `anova_runr()`: ANOVA-based power analysis
   - `dunnett_runr()`: Dunnett's test power analysis
   - `nb_runr()`: Negative binomial regression power analysis
   - `pois_runr()`: Poisson regression power analysis
   - `logit_runr()`: Logistic regression power analysis

3. **Visualization Layer**:
   - `plot_pwr()`: Create power curves with flexible grouping and faceting options

## Basic Example

This is a basic example of power analysis for an ANOVA design:

```r
library(powrunr)
library(dplyr)

# Set parameters
mean_value <- 100       # Control group mean
sd_value <- 15          # Standard deviation
max_effect <- 20        # Maximum treatment effect
sample_sizes <- 5:15    # Sample sizes to test
num_sim <- 500          # Number of simulations

# Create scenarios
scenarios <- data.frame(n = sample_sizes)

# Run simulations
set.seed(123)  # For reproducibility
results <- lapply(scenarios$n, function(n_val) {
  anova_runr(
    num_sim = num_sim,
    n = n_val,
    mean = mean_value,
    sd = sd_value,
    max_trt_eff = max_effect,
    ntrts = 3
  )
})

# Calculate power
power_summary <- do.call(rbind, results) %>%
  group_by(n) %>%
  summarize(p.value = mean(p.value < 0.05, na.rm = TRUE))

# Define color palette
colpal <- c("#00BFC4")

# Plot power curve
plot_pwr(power_summary, colpal, group = NULL)
```

## Comprehensive Guide

For a more detailed guide on using `powrunr` for different types of analyses, please see the vignette:

```r
vignette("powrunr-guide", package = "powrunr")
```

The vignette includes examples for:
- ANOVA and Dunnett's test for continuous data
- Poisson regression for count data
- Negative binomial regression for overdispersed count data
- Logistic regression for binary data

## Best Practices

When using `powrunr` for power analysis, consider these recommendations:

1. **Use sufficient simulations**: At least 500-1000 for stable estimates
2. **Set a seed for reproducibility**: Use `set.seed()` before running simulations
3. **Consider practical constraints**: Balance statistical power with available resources
4. **Be conservative with variability estimates**: When in doubt, use larger estimates of SD
5. **Interpret results cautiously**: Remember that simulations involve random variation

## Contributing

Contributions to `powrunr` are welcome! Please feel free to submit issues or pull requests on the [GitHub repository](https://github.com/gabrielndip/powrunr).

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE.md) file for details.
