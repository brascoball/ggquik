#' Add a baseline to a plot.
#'
#' Add a horizontal or vertical baseline to a plot. This adds the baseline
#' under most layers.
#'
#' @param gg The ggplot object on which to add a baseline
#' @param direction The direction on which to add the baseline: \code{x}, or \code{y}
#' @param intercept The location on which to add the line (usually a dimension factor or a measure)
#'
#' @usage add_baseline(gg, direction, intercept)
#'
#' @export
add_baseline = function(gg, direction, intercept) {
  if (direction == 'x') gg <- gg + geom_vline(xintercept = intercept,
                                               linetype = "longdash", alpha = 0.5, size = 0.2)
  if (direction == 'y') gg <- gg + geom_hline(yintercept = intercept,
                                               linetype = "longdash", alpha = 0.5, size = 0.2)
  le <- length(gg$layers)
  # move the baseline under the points and text
  gg$layers <- c(gg$layers[1], gg$layers[le], gg$layers[2:(le-1)])
  return(gg)
}


#' Add a baseband to a plot.
#'
#' Add a horizontal or vertical background band (\code{geom_rect}) to a plot. 
#' This adds the band under most existing layers.
#'
#' @param gg The ggplot object on which to add a baseline
#' @param direction The direction on which to add the baseline: \code{x}, or \code{y}
#' @param min The location to start the band
#' @param max The location to end the band
#' @param bg_color The background color for the band area. Default is \code{"Green 4"}
#'
#' @usage add_baseband(gg, direction, min, max, bg_color)
#'
#' @export
add_baseband = function(gg, direction, min, max, bg_color = "Green 4") {
  if(direction == 'x') {
    gg <- gg + geom_rect(xmin=min, xmax=max, ymin=-Inf, ymax=Inf, alpha=0.02, fill=redhat_colors(bg_color), color = NA) +
      geom_vline(xintercept = min, alpha = 0.5, linetype = "longdash", size = 0.2) +
      geom_vline(xintercept = max, alpha = 0.5, linetype = "longdash", size = 0.2)
  } else {
    gg <- gg + geom_rect(xmin=-Inf, xmax=Inf, ymin=min, ymax=max, alpha=0.02, fill=redhat_colors(bg_color), color = NA) +
      geom_hline(yintercept = min, alpha = 0.5, linetype = "longdash", size = 0.2) +
      geom_hline(yintercept = max, alpha = 0.5, linetype = "longdash", size = 0.2)
  }
  le <- length(gg$layers)
  # move the base band under most layers
  # if (class(gg$layers[[1]]$geom)[[1]] == 'GeomLine') gg$layers <- c(gg$layers[1], gg$layers[(le-2):le], gg$layers[2:(le-3)])
  gg$layers <- c(gg$layers[(le-2):le], gg$layers[1:(le-3)])
  return(gg)
}


#' Add a confidence interval to a quik line plot
#'
#' After creating a line plot using \code{quik_lines},
#' this function will add a confidence interval
#'
#' @import ggplot2
#'
#' @param gg The ggplot to be themed
#' @param projection The projection (forecast) column
#' @param confidence A logical. Should confidence intervals be shown?
#' @param label_final A logical. Should the final maximum be labeled?
#'
#' @usage add_projection(gg, projection, confidence, label_final)
#'
#' @export
add_projection = function(gg, projection, confidence = FALSE, label_final = FALSE) {
  line.layer.num <- which(sapply(ggplot_build(gg)$data, function(x) any(grepl('shape', names(x)))))
  line_groups <- as.character(gg$layers[[line.layer.num]]$mapping$colour)
  curr.color <- unique(ggplot_build(gg)$data[[line.layer.num]]$colour)
  new.color <- change_shade(curr.color, -2)
  if (line_groups == 1) line_groups = shQuote(line_groups)
  if (confidence) {
    conf.nums <- sort(as.numeric(substr(names(gg$data)[grep("Lo.\\d\\d", names(gg$data))], 4, 5)))
    lo.names <- paste0('Lo.', conf.nums)
    hi.names <- paste0('Hi.', conf.nums)
    conf.titles <- shQuote(paste0(conf.nums, '% Confidence'))
    conf.a <- 0.25
    gg <- gg +
      geom_ribbon(aes_string(ymin=lo.names[1], ymax=hi.names[1], fill = conf.titles[1]),
                             alpha = conf.a) +
      geom_ribbon(aes_string(ymin=lo.names[2], ymax=hi.names[2], fill = conf.titles[2]),
                             alpha = conf.a) +
      scale_fill_manual(values = c(curr.color, new.color))
  }
  gg <- gg + geom_line(aes_string(y=make.names(projection), color=line_groups), linetype = "dashed", size = 0.25)
  if (label_final) {
    which.text <- which(sapply(ggplot_build(gg)$data, function(x) any(grepl('fontface', names(x)))))
    label.size <- ggplot_build(gg)$data[[which.text]]$size[[1]]*0.7
    gg$data$conf_label <- format_label(gg$data[, gsub("`", "", hi.names[2])], currency = gg$data$currency[1],
                                           measure_unit = gg$data$measure_unit[1], parenthesis = TRUE)
    gg$data$conf_label[-nrow(gg$data)] <- NA
    gg <- gg + geom_text(aes_string(y=hi.names[2]), family = 'Overpass',
                         size = label.size, label = gg$data$conf_label, vjust = -0.5)
  }
  return(gg)
}

#' Add Red Hat facets to a plot
#'
#' After creating your own ggplot, you may want to separate a plot by groups into
#' two plots. This is done by faceting.
#' 
#' @importFrom stats as.formula
#'
#' @param gg A ggplot object
#' @param facet_col The data frame column by which to group the data
#' @param ncol The number of columns to have plots
#' @param nrow The number of rows of plots
#' @param scales Do you want each plot to have the same measure scale ("fixed") or
#' each to have their own scale ("free")
add_facet = function(gg, facet_col, ncol = NULL, nrow = NULL, scales = 'free') {
  return(gg + facet_wrap(as.formula(paste0('~', facet_col)), scales = scales, ncol = ncol, nrow = nrow))
}
