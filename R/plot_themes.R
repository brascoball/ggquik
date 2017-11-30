#' Add Red Hat theming to a ggplot
#'
#' After creating your own ggplot, or using \code{\link{quik_bars}} or
#' \code{\link{quik_lines}}, or \code{\link{quik_bullets}},
#' \code{quik_theme} will help your plot match CEE Analytics standards. By
#' default, \code{quik_theme} will clean the grid and axes, clean and move
#' the legend to the bottom, and make all white elements transparent for
#' universal use. \code{clean_facet} can be set to TRUE to clean ugly
#' facet titles.
#'
#' @param gg The ggplot to be themed
#' @param clean_grid A logical. Should the plot grid be cleaned?
#' @param clean_axis A logical. Should the plot axes be cleaned?
#' @param clean_facet A logical. Should the facet titles be cleaned?
#' @param legend A string. One of three options: \code{"bottom"},
#' \code{"right"}, or \code{"none"}. Default is \code{"none"}.
#' @param transparent A logical. Should the plot be transparent for saving?
#' @param ... Parameters to pass on to \code{\link{quik_clean_axis}} such
#' as \code{axis.title} and \code{axis.text}, or to
#' \code{\link{quik_clean_grid}} such as \code{grid.lines}.
#'
#' @usage quik_theme(gg, clean_grid, clean_axis, clean_facet,
#'                       legend, transparent, ...)
#'
#' @export
quik_theme = function(gg, clean_grid = TRUE, clean_axis = TRUE, clean_facet = NULL,
                    legend = 'bottom', transparent = TRUE, ...) {
  # If clean_facet is not set, check if there's a facet to be cleaned
  if (is.null(clean_facet)) clean_facet = any(grepl("FacetWrap", class(ggplot_build(gg)$layout$facet)))
  p_axis <- names(formals('quik_clean_axis'))
  p_grid <- names(formals('quik_clean_grid'))
  params <- list(...)
  if (clean_axis) gg <- do.call('quik_clean_axis', c(list(gg = gg), params[names(params) %in% p_axis]))
  if (clean_grid) gg <- do.call('quik_clean_grid', c(list(gg = gg), params[names(params) %in% p_grid]))
  if (clean_facet) gg <- quik_clean_facet(gg)
  gg <- quik_legend(gg, legend)
  if (transparent) gg <- quik_transparent(gg)
  return(gg)
}


#' Clean the axis in a ggplot theme
#'
#' Usually called by quik_theme automatically, this function is used to
#' either update the font for the axis titles and text, or remove them.
#' It can do this for both axes, or one at a time.
#'
#' @import ggplot2
#'
#' @param gg The ggplot to be themed
#' @param axis.text Which axis text labels are
#' needed:  \code{NULL, 'x', 'y', or c('x', 'y')}
#' @param axis.title Which axis title labels are
#' needed:  \code{NULL, 'x', 'y', or c('x', 'y')}
#'
#' @usage quik_clean_axis(gg,  axis.title, axis.text)
#'
#' @export
quik_clean_axis = function(gg, axis.title = NULL, axis.text = NULL) {
  blnk <- element_blank()
  text.col <- ggquik::plot_colors$text.color
  txt <- element_text(color=text.col, family="Overpass")

  gg <- gg + theme(axis.title=txt)
  if (!('x' %in% axis.title)) gg <- gg + theme(axis.title.x=blnk)
  if (!('y' %in% axis.title)) gg <- gg + theme(axis.title.y=blnk)

  gg <- gg + theme(axis.text=txt)
  if (!('x' %in% axis.text)) gg <- gg + theme(axis.text.x=blnk)
  if (!('y' %in% axis.text)) gg <- gg + theme(axis.text.y=blnk)
  return(gg)
}


#' Clean the grid in a ggplot theme
#'
#' Usually called by quik_theme automatically, this function is used to
#' clear the gray grid background, and either update the grid lines
#' or remove them. It can do this for both axes, or just one.
#'
#' @import ggplot2
#'
#' @param gg The ggplot to be themed
#' @param grid.lines Which grid lines are
#' needed:  \code{NULL, 'x', 'y', or c('x', 'y')}
#'
#' @usage quik_clean_grid(gg, grid.lines)
#'
#' @export
quik_clean_grid = function(gg, grid.lines = NULL) {
  grid.col <- ggquik::plot_colors$grid.color
  lne <- element_line(color=grid.col, size=0.25)
  blnk <- element_blank()
  gg <- gg +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.ticks=element_blank())
  gg <- gg + theme(panel.grid.major = blnk)
  if ('x' %in% grid.lines) gg <- gg + theme(panel.grid.major.x = lne)
  if ('y' %in% grid.lines) gg <- gg + theme(panel.grid.major.y = lne)
  return(gg)
}


#' Clean the facet titles in a ggplot theme
#'
#' @import ggplot2
#'
#' @param gg The ggplot to be themed
#'
#' @usage quik_clean_facet(gg)
#'
#' @export
quik_clean_facet = function(gg) {
  gg <- gg +
    theme(strip.background = element_blank(), panel.spacing = unit(1.25, "cm"),
          strip.text.x = element_text(size = 10, colour = ggquik::plot_colors$text.color,
                                      family = 'Overpass', margin = margin(b = 7)))
  return(gg)
}


#' Move the legend to the bottom for a ggplot theme
#'
#' @import ggplot2
#'
#' @param gg The ggplot to modify the legend
#' @param legend_position The position place the legend. One
#' of three options: \code{"bottom"}, \code{"right"}, or
#' \code{"none"}. Default is \code{"none"}.
#'
#' @usage quik_legend(gg, legend_position)
#'
#' @export
quik_legend = function(gg, legend_position) {
    gg <- gg +
      theme(legend.position=legend_position, legend.justification="center",
            legend.key.height=unit(0.5,"line"), legend.key.width=unit(1,"line"),
            legend.title=element_blank(),
            legend.text=element_text(family = "Overpass",
                                     color = ggquik::plot_colors$text.color, size=10))
  return(gg)
}

#' Make a ggplot theme transparent
#'
#' @import ggplot2
#'
#' @param gg The ggplot to be themed
#'
#' @usage quik_transparent(gg)
#'
#' @export
quik_transparent = function(gg) {
  gg <- gg +
    theme(legend.key=element_rect(fill = "transparent", color = NA),
          legend.background=element_rect(fill = "transparent", color = NA),
          plot.background=element_rect(fill = "transparent", color = NA))
  return(gg)
}