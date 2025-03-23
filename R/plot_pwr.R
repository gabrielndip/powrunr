#' Plot simulated data for power against sample size, n
#'
#' Creates a visualization of statistical power versus sample size, with options
#' for grouping and faceting. This function helps researchers determine adequate
#' sample sizes for their experimental designs.
#'
#' @param data A dataframe with columns for sample size, power estimates, and grouping variables.
#' @param colpal Character vector of hexadecimal color values for the plot.
#' @param n The name of the sample size column in `data`.
#' @param p.value The name of the power estimate column in `data`.
#' @param group The name of the grouping variable column in `data` (used for coloring).
#' @param facet The name of the faceting variable in `data`. Set to NULL for no faceting.
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom stats as.formula
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' # Create example data
#' data <- data.frame(
#'   n = rep(1:10, each = 3),
#'   p.value = runif(30, 0.5, 0.9),
#'   sd = rep(1:3, 10)
#' )
#' colpal <- c("#00BFC4", "#F8766D", "#7CAE00")
#'
#' # Generate power plot with grouping but no faceting
#' plot_pwr(data, colpal, group = "sd", facet = NULL)
#'
#' # Generate power plot with both grouping and faceting by the same variable
#' plot_pwr(data, colpal, group = "sd", facet = "sd")
plot_pwr <- function(data, colpal, n = "n", p.value = "p.value",
                     group = "sd", facet = NULL) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }

  if (!all(c(n, p.value) %in% names(data))) {
    stop("'data' must contain columns for n and p.value", call. = FALSE)
  }

  if (!is.null(group) && !group %in% names(data)) {
    stop(paste0("'", group, "' column not found in data"), call. = FALSE)
  }

  if (!is.null(facet) && !facet %in% names(data)) {
    stop(paste0("'", facet, "' column not found in data"), call. = FALSE)
  }

  if (!is.null(group)) {
    if (!is.character(colpal) || length(colpal) < length(unique(data[[group]]))) {
      stop("'colpal' must be a character vector with at least as many colors as unique group values",
           call. = FALSE)
    }
  }

  # Convert column names to symbols for non-standard evaluation
  n_sym <- rlang::sym(n)
  p.value_sym <- rlang::sym(p.value)

  # Create base plot
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = as.factor(!!n_sym), y = !!p.value_sym)
  )

  # Add grouping if specified
  if (!is.null(group)) {
    group_sym <- rlang::sym(group)
    p <- p + ggplot2::aes(colour = as.factor(!!group_sym),
                          group = as.factor(!!group_sym))
    p <- p + ggplot2::scale_color_manual(values = colpal, name = group)
  }

  # Add common elements
  p <- p +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0.8) +  # Common power threshold
    ggplot2::theme_bw() +
    ggplot2::xlab("Group Size") +
    ggplot2::ylab("Statistical Power")

  # Add faceting if specified
  if (!is.null(facet)) {
    facet_sym <- rlang::sym(facet)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet_sym))
  }

  return(p)
}
