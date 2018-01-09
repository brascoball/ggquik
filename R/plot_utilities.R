#' Set Overpass as the quik family font
#'
#' This is a function will download the Overpass font from
#' \url{https://fonts.google.com/} if necessary, then set it as 
#' the font using showtext.
#'
#' @import sysfonts
#' @import showtext
#'
#' @export
set_quik_family = function() {
  if (!("Overpass" %in% font_families())) font_add_google("Overpass", "Overpass")
  showtext_auto()
  return("Overpass")
}


#' Replace ggplot scale breaks with only limits
#'
#' This is a function containing a function that will take
#' the given breaks for a continuous scale and returns only
#' the limits based on the data. Currently only used as a
#' sub function for \code{\link{quik_bullets}}
#'
#'
#' @export
use_limits = function() {
  function(x) c(min(x)+diff(range(x))*0.05/0.6,
                max(x)-diff(range(x))*0.05/0.6)
}


#' Flip Levels of a factor
#'
#' Sometimes plots require levels to be reversed. This function will flip the order
#' of factors.
#'
#' @param data_frame_column The column in your data frame to reverse the levels
#'
#' @usage flip_levels(data_frame_column)
#'
#' @return A column to be added to a dataframe
#'
#' @export
flip_levels = function(data_frame_column) {
  return(factor(data_frame_column, levels = rev(levels(data_frame_column))))
}


#' Expand yearmon or yearqtr into a factor
#'
#' This will add in any missing dates with zeroes, then change the yearmon/yearqtr
#' dimension into a factor so that it can be used in order like all other
#' factor dimensions.
#'
#' @importFrom data.table data.table := CJ
#'
#' @param df The data frame to update the dimension
#' @param dimension The column containing the values to compare across (e.g. quarters, types)
#' @param measure The data frame column with the numerical values to plot
#' @param facet_by The data frame column with the facet data (if necessary).
#' @param type The zoo type, either \code{"yearmon"} or \code{"yearqtr"}.
#'
#' @usage factor_zoo(df, dimension, measure, facet_by, type)
#'
#' @export
factor_zoo = function(df, dimension, measure, facet_by, type) {
  dt <- data.table(df)
  zoo_period <- ifelse(type == 'yearmon', 1/12, 1/4)
  zoo_format <- ifelse(type == 'yearmon', "%b %y", "FY%yQ%q")
  all_zoo = seq(min(dt[, get(dimension)]), max(dt[, get(dimension)]), zoo_period)
  if(!is.null(facet_by)) {
    grid <- CJ(all_zoo, dt[, get(facet_by)], unique = TRUE)
    names(grid) <- c(dimension, facet_by)
    dt <- merge(grid, dt, all.x = TRUE)
    dt[is.na(get(measure)), (measure) := 0]
  }
  df <- data.frame(dt)
  df[, dimension] <- factor(format(df[, dimension], zoo_format), levels = format(all_zoo, zoo_format))
  return(df)
}


#' Set the decimal for the plot text
#'
#' @param measure The df column to be plotted
#' @param measure_unit A string. Can be \code{\%}, \code{K}, or \code{M}
#'
#' @usage set_decimal(measure, measure_unit)
#'
#' @export
set_decimal = function(measure, measure_unit) {
  measure_decimal = 0
  if (mean(measure) < 10) {
    if (is.null(measure_unit)) { measure_decimal = 1 }
    else if (measure_unit != '%') { measure_decimal = 1 }
  }
  return(measure_decimal)
}

