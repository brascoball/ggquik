#' Determine quik ggplot2 fill colors
#'
#' This is a function containing which determines
#' the fill colors to be used in the plot based
#' on the type of colors desired. This is a
#' sub function for \code{\link{quik_bars}} and
#' \code{\link{quik_bullets}}.
#'
#' @param group_column The data frame column containing the levels
#' @param palette_type A string. Allowed values are \code{"diverging"},
#' \code{"sequential"}, and \code{"qualitative"}.
#' @param color_names A string. If \code{palette_type} is
#' \code{"sequential"}, what colors should be used for the bars (i.e.
#' \code{"gray"}, \code{"red"}, \code{"dark red"}, \code{"blue"},
#' \code{"dark blue"}, \code{"light blue"}, \code{"gold"},
#' \code{"green"}, \code{"purple"}, \code{"teal"}).
#' If \code{palette_type} is \code{"manual"},
#' what colors should be used (e.g. \code{c("Purple 5", "Blue 2")})
#'
#' @export
set_group_colors = function(group_column, palette_type, color_names = NULL) {
  plot_colors <- ggquik::plot_colors
  if(!(class(group_column) %in% c('factor', 'data.frame'))) group_column <- as.factor(group_column)
  if (is.null(color_names)) {
    if (palette_type == 'sequential') color_names = 'Teal'
    if (palette_type == 'manual') stop("Please provide values for color_names")
  }
  if (length(group_column) == 0) {
    gr_nums = 1
  } else {
    gr_nums <- length(levels(group_column))
  }
  if (palette_type == 'diverging') {
    if (gr_nums <= 2) fill.colors <-  c(plot_colors$positive, plot_colors$negative)
    else fill.colors <- c(plot_colors$positive, rev(redhat_colors(paste("Gray", 4:(1+gr_nums)))), plot_colors$negative)
  } else if (palette_type == 'sequential') {
    fill.colors <- redhat_colors(color_names, partial = TRUE)
    fill.colors <- fill.colors[order(fill.colors)]
    if (length(fill.colors) < gr_nums) stop("Not enough colors to build bars")
  } else if (palette_type == 'qualitative') {
    fill.colors <- ggquik::rh_colors[ggquik::rh_colors$type == 'qualitative', 'color'][1:gr_nums]
  } else if (palette_type == 'manual') {
    fill.colors <- redhat_colors(color_names)
  } else {
    stop("palette type must be 'diverging', 'sequential', 'qualitiative', or 'manual'")
  }
  return(fill.colors)
}

#' Format the labels for a measure on a quik plot.
#'
#' This will round the data to the measure unit, add a prefix, and add a suffix
#'
#' @param measure The column name of the measure
#' @param currency A string, usually \code{$}
#' @param measure_unit A string. Can be \code{\%}, \code{K}, or \code{M}
#' @param measure_decimal An integer. The number of decimal places to show.
#' @param parenthesis A logical. Should parenthesis be added?
#'
#' @usage format_label(measure, currency, measure_unit,
#'                        measure_decimal, parenthesis)
#'
#' @export
format_label = function(measure, currency = NULL, measure_unit = NULL,
                           measure_decimal = 0, parenthesis = FALSE) {
  if (is.null(measure_unit))  {
    mult = 1
  } else if (measure_unit == "%") {
    mult = 100
  } else if (measure_unit == "K") {
    mult = 1/1000
  } else if (measure_unit == "M") {
    mult = 1/1000000
  }
  measure_label <- ifelse(is.na(measure), "NA", paste0(round(measure*mult, measure_decimal), measure_unit))
  if (!is.null(currency)) measure_label <- paste0(currency, measure_label)
  if (parenthesis) measure_label <- paste0("(", measure_label, ")")
  return(measure_label)
}

#' Prep a data frame for ggquik plotting
#'
#' Sometimes plots need to modify data frame information in
#' order to represent it in a visual format. \code{quik_prepare} takes
#' a data frame, and optionally:
#' \itemize{
#'  \item{Creates a column of formatted percentages}
#'  \item{Sums data across group members}
#'  \item{Sets the positions for text that will be placed in a bar chart}
#' }
#'
#' @import ggplot2
#' @importFrom data.table data.table := CJ setorderv setkeyv
#'
#' @param df The data frame containing plot data
#' @param dimension The column containing the values to compare across (e.g. quarters, types)
#' @param measure The data frame column with the numerical values to plot
#' @param plot_type A string. The type of plot (\code{"bar"} or \code{"line"}).
#' @param groups The column containing the different groups of bars, lines, or points.
#' @param facet_by The data frame column with the facet data (if necessary).
#' @param background A logical. Is a background total necessary?
#' @param currency A string, usually \code{$}
#' @param measure_unit A string. Can be \code{\%}, \code{K}, or \code{M}
#' @param measure_decimal An integer. The number of decimal places to show.
#' @param sum_label Will there be a special group label? Sum from which column?
#' @param text_cutoff A number. If text values below a certain number should not be included.
#'
#' @usage quik_prepare(df, dimension, measure, plot_type, groups, 
#'                        facet_by, background,
#'                        currency, measure_unit, measure_decimal,
#'                        sum_label, text_cutoff)
#'
#' @return Returns a new data frame with additional columns based on selections
#'
#' @export
quik_prepare = function(df, dimension, measure, plot_type, groups = NULL,
                        facet_by = NULL, background = FALSE,
                           currency = NULL, measure_unit = NULL, measure_decimal = 0,
                           sum_label = NULL, text_cutoff = NULL) {
  position_text <- alt_label <- NULL
  if (substr(class(df[, dimension]), 1, 4) == 'year') df <- factor_zoo(df, dimension = dimension,
                                                            measure = measure, facet_by = facet_by, 
                                                            type = class(df[, dimension]))
  dt <- data.table(df)
  if (plot_type == 'bar')  dt[, position_text := cumsum(get(measure)) - (0.5 * get(measure)), by = c(dimension, facet_by)]
  if (!is.null(sum_label)) {
    dt[, alt_label := format(sum(eval(as.name(sum_label))), big.mark=","), by = c(dimension, facet_by)]
    setorderv(dt, alt_label)
    dt$alt_label <- factor(dt$alt_label, levels = unique(dt$alt_label))
  }
  if (background) {
    grid <- CJ(dt[, get(dimension)], dt[, get(facet_by)], unique = TRUE)
    names(grid) <- c(dimension, facet_by)
    dt <- merge(grid, dt, all.x = TRUE)
    dt[is.na(get(measure)), (measure) := 0]
    dt[, background := sum(get(measure)), by = get(dimension)]
    setkeyv(dt, facet_by)
  }
  # Changed to facet_by, groups on 3/9/2018 because CS Revenue faceted by region grouped by segment
  # text was on wrong points. Originally was groups, facet_by.
  dt <- dt[order(get(c(facet_by, groups, dimension))), ]
  df <- data.frame(dt, check.names = FALSE)
  df$measure_label <- format_label(df[, measure], currency = currency,
                                      measure_unit = measure_unit, measure_decimal = measure_decimal)
  if(!is.null(text_cutoff)) df[df[, measure] <= text_cutoff, "measure_label"] <- NA
  # if(class(df[, dimension]) == 'yearmon') stop("yay!")
  df$currency <- currency
  df$measure_unit <- measure_unit
  return(df)
}


#' Set various quik options for plotting
#' 
#' @param df The data frame containing plot data
#' @param dimension The column containing the values to compare across (e.g. quarters, types)
#' @param measure The data frame column with the numerical values to plot
#' @param groups The data frame column containing the different groups
#' @param palette_type A string. Allowed values are \code{"diverging"},
#' \code{"sequential"}, and \code{"qualitative"}. Default is \code{"qualitative"}.
#' @param colors The the colors to be used for the line(s)/point(s)
#' @param measure_unit A string. Can be \code{\%}, \code{K}, or \code{M}
#' @param measure_decimal An integer. The number of decimal places to show.
#' 
#' 
#' @export
set_quik_opts = function(df, dimension, measure, groups, palette_type, colors, measure_decimal, measure_unit) {
  quik_opts = list()
  # add additional formatted columns
  quik_opts$txt.d <- ggquik::plot_colors$text.dark
  # set the decimal
  if(is.null(measure_decimal)) measure_decimal <- set_decimal(df[, measure], measure_unit)
  quik_opts$measure_decimal <- measure_decimal
  # clean names if needed
  quik_opts$x.lab = dimension; quik_opts$y.lab = measure; quik_opts$c.lab = groups
  # update values based on one or many lines
  if (is.null(groups)) {
    if (is.null(colors)) colors = 'Purple'
    quik_opts$colors <- redhat_colors(colors)
    quik_opts$legend = FALSE
    quik_opts$groups <- shQuote("1")
  } else {
    quik_opts$colors <- set_group_colors(df[, groups], palette_type, colors)
    quik_opts$legend = 'legend'
    quik_opts$groups <- make.names(groups)
  }
  return(quik_opts)
}

