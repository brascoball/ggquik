#' Red Hat branded colors.
#'
#' A data frame containing the official color name and their hex code
#' for use with \url{ggplot2}. Based on
#' \url{http://brand.redhat.com/elements/color/}
#'
#' @docType data
#'
#' @usage data(rh_colors)
#'
#' @format A data frame with 72 rows and 3 variables:
#'  \describe{
#'       \item{name}{color name}
#'       \item{color}{hexidecimal number}
#'       \item{type}{primary, qualitative, product, gray, or sequential color}
#'  }
#'
#' @keywords datasets
#'
#' @source internal
#'
#' @examples
#' data(rh_colors)
#'
#' @rdname hattyr-data-rh_colors
"rh_colors"
NULL

#' Red Hat plot colors.
#'
#' A list containing a standard set of plot colors from
#' the Red Hat color palette.
#'
#' @docType data
#'
#' @usage data(plot_colors)
#'
#' @format A data frame with 4 rows and 1 variable:
#'  \describe{
#'       \item{positive}{A positive color (currently Green)}
#'       \item{negative}{A negative color (currently Gold)}
#'       \item{grid.color}{A gray to color the grid}
#'       \item{background.fill}{A gray to color plot text}
#'       \item{text.dark}{A dark gray to color plot text}
#'       \item{text.light}{A light color (white) to color plot text}
#'  }
#'
#' @keywords datasets
#'
#' @source internal
#'
#' @examples
#' data(plot_colors)
#'
#' @rdname hattyr-data-plot_colors
"plot_colors"
