% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_pwr.R
\name{plot_pwr}
\alias{plot_pwr}
\title{Plot simulated data for power against sample size, n}
\usage{
plot_pwr(
  data,
  colpal,
  n = "n",
  p.value = "p.value",
  group = "sd",
  facet = NULL
)
}
\arguments{
\item{data}{A dataframe with columns for sample size, power estimates, and grouping variables.}

\item{colpal}{Character vector of hexadecimal color values for the plot.}

\item{n}{The name of the sample size column in \code{data}.}

\item{p.value}{The name of the power estimate column in \code{data}.}

\item{group}{The name of the grouping variable column in \code{data} (used for coloring).}

\item{facet}{The name of the faceting variable in \code{data}. Set to NULL for no faceting.}
}
\value{
A ggplot object
}
\description{
Creates a visualization of statistical power versus sample size, with options
for grouping and faceting. This function helps researchers determine adequate
sample sizes for their experimental designs.
}
\examples{
# Create example data
data <- data.frame(
  n = rep(1:10, each = 3),
  p.value = runif(30, 0.5, 0.9),
  sd = rep(1:3, 10)
)
colpal <- c("#00BFC4", "#F8766D", "#7CAE00")

# Generate power plot with grouping but no faceting
plot_pwr(data, colpal, group = "sd", facet = NULL)

# Generate power plot with both grouping and faceting by the same variable
plot_pwr(data, colpal, group = "sd", facet = "sd")
}
