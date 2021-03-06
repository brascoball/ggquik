% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_add_ons.R
\name{add_baseline}
\alias{add_baseline}
\title{Add a baseline to a plot.}
\usage{
add_baseline(gg, direction, intercept)
}
\arguments{
\item{gg}{The ggplot object on which to add a baseline}

\item{direction}{The direction on which to add the baseline: \code{x}, or \code{y}}

\item{intercept}{The location on which to add the line (usually a dimension factor or a measure)}
}
\description{
Add a horizontal or vertical baseline to a plot. This adds the baseline
under most layers.
}
