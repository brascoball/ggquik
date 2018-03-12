#' Plot a quik bar plot
#'
#' ggquik has specific standards for plots, and
#' \code{quik_bars} helps your plot follow these.
#'
#' @import ggplot2
#' @importFrom stats as.formula
#'
#' @param df The data frame containing plot data
#' @param dimension Usually the x-axis, values like "quarter" or "account type"
#' @param measure The column containing numerical values to be plotted
#' @param groups If a stacked or dodged bar, the column containing the groups
#' @param position If groups, whether to \code{"stack"} (default) or \code{"dodge"} the groups.
#' @param text_size A numeric. The size of the text within the bars (default is 3)
#' @param alt_text_size A numeric. The size of the alternative text (default is 3)
#' @param alt_label The column containing an alternate label (if any)
#' @param facet_by The column to add facets to the plot (if necesary)
#' @param background If using \code{facet_by}, should background totals be added?
#' @param flip_plot A logical. Should the plot coordinates be flipped?
#' @param dim_breaks A vector of specific dimensions values that should be labeled.
#' @param palette_type A string. Allowed values are \code{"diverging"},
#' \code{"sequential"}, and \code{"qualitative"}. Default is \code{"qualitative"}.
#' @param colors A string. What color should be used for the bars (e.g.
#' "gray", "red", "dark red", "blue", "dark blue", "light blue", gold",
#' "green", "purple", "teal")
#' @param currency A string, usually \code{$}
#' @param measure_unit A string. Can be \code{\%}, \code{K}, or \code{M}
#' @param measure_decimal An integer. The number of decimal places to show. Default is 0.
#' @param v.just A numeric. The vertical adjustment for the text.
#' @param text_cutoff A number. If text values below a certain number should not be included.
#' @param ... Additional parameters to pass on it facet_wrap (e.g. scales = free, ncol = 1)
#'
#' @usage quik_bars(df, dimension, measure, groups, position,
#'                   text_size, alt_text_size, alt_label, facet_by, background,
#'                   flip_plot, dim_breaks, palette_type, colors,
#'                   currency, measure_unit, measure_decimal,
#'                   v.just, text_cutoff, ...)
#'                   
#' @examples 
#' # Create a bar plot from morley data
#' data(morley)
#' ggq <- quik_bars(morley, dimension = 'Run', measure = 'Speed', groups = 'Expt')
#' quik_theme(ggq, axis.text = 'y', axis.title = c('x', 'y'))
#'
#' @export
quik_bars = function(df, dimension, measure, groups = NULL, position = 'stack',
                       text_size = 3, alt_text_size = 4, alt_label = NULL, facet_by = NULL, background = FALSE,
                       flip_plot = FALSE, dim_breaks = NULL, palette_type = 'qualitative', colors = NULL,
                       currency = NULL, measure_unit = NULL, measure_decimal = NULL,
                       v.just = 0.5, text_cutoff = 0, ...) {
  bar_width <- 0.65
  if(is.null(measure_decimal)) measure_decimal <- set_decimal(df[, measure], measure_unit)
  if(background & is.null(facet_by)) stop("facet_by must be supplied if background = TRUE")
  df <- quik_prepare(df, dimension = dimension, measure = measure, plot_type = 'bar',
                     groups = groups, facet_by = facet_by, background = background,
                     currency = currency, measure_unit = measure_unit, 
                     measure_decimal = measure_decimal, 
                     sum_label = alt_label, text_cutoff = text_cutoff)
  fill.colors <- set_group_colors(df[, groups], palette_type, colors)
  if (palette_type == 'diverging') {
    keep_labels <- levels(df[, groups])[c(1, length(levels(df[, groups])))]
    df[!(df[, groups] %in% keep_labels), 'measure_label'] <- NA
  }
  txt.l <- ggquik::plot_colors$text.light
  txt.d <- ggquik::plot_colors$text.dark
  bg.f <- ggquik::plot_colors$background.fill
  if (flip_plot) {
    df$position_text <- 1 - df$position_text
    if(class(df[, dimension]) != 'factor') df[, dimension] <- as.factor(df[, dimension])
    df[, dimension] <- flip_levels(df[, dimension])
    h.just <- 0
    alt.pos <- 1.01
  } else {
    if (!is.null(groups)) {
      if(class(df[, groups]) != 'factor') df[, groups] <- as.factor(df[, groups])
      if (position == 'stack') df[, groups] <- flip_levels(df[, groups])
    } else { groups <- shQuote("1") }
    h.just <- 0.5
    alt.pos <- 1.03
  }
  gg <- ggplot(df, aes_string(x = dimension, y = measure))
  if (background) {
    bar_width <- 0.95
    gg <- gg + geom_bar(aes(y = background), stat = 'identity', fill = bg.f, position = position,
                        width = bar_width, alpha = 0.7)
  }
  gg <- gg + geom_bar(aes_string(group = groups, fill = groups), stat = 'identity', position = position, width = bar_width)
  if (!is.null(dim_breaks)) gg <- gg + scale_x_discrete(breaks = unique(df[,dimension])[dim_breaks])
  if (!is.null(facet_by)) gg <- gg + facet_wrap(as.formula(paste0("~", facet_by)), ...)
  if (flip_plot) {
    gg <- gg + coord_flip()
    fill.colors <- rev(fill.colors)
  }
  if (text_size > 0) {
    if (position == 'stack') {
      position = 'identity'
      position_text = 'position_text'
      text_groups = NULL
      guide_reverse = TRUE
    } else {
      position <- position_dodge(width = bar_width)
      position_text = measure
      text_groups = groups
      v.just = v.just + 1
      guide_reverse = FALSE
    }
    gg <- gg + geom_text(aes_string(y = position_text, label = 'measure_label', group = text_groups), 
                         position = position, family=set_quik_family(),
                         color=txt.l, size = text_size, vjust = v.just)
  }
  gg <- gg + scale_fill_manual(values=fill.colors) + guides(fill=guide_legend(reverse = guide_reverse))
  if (!is.null(alt_label)) {
    # y.expand <- waiver()
    y.expand <- expand_scale(mult = c(0, .1))
    gg <- gg + geom_text(data=unique(df[, c(dimension, facet_by, 'alt_label')]),
                         aes_string(y = alt.pos, label = 'alt_label'), hjust = h.just,
                         color = txt.d, family = set_quik_family(), size = alt_text_size)
  } else {
    y.expand <- c(0, 0)
  }
  y.labels = y.breaks = waiver()
  if (!is.null(measure_unit)) {
    if (measure_unit == '%') {
      y.labels = scales::percent
      y.breaks = c(0, .25, .5, .75, 1)
    }
  }
  gg <- gg + scale_y_continuous(breaks = y.breaks, labels = y.labels, expand = y.expand)
  return(gg)
}


#' Plot a quik line plot
#'
#' ggquik has specific standards for plots, and \code{quik_lines} helps your plot follow these.
#'
#' @import ggplot2
#' @importFrom stats as.formula
#'
#' @param df The data frame containing plot data
#' @param dimension Usually the x-axis, values like "quarter" or "account type"
#' @param measure The column containing numerical values to be plotted
#' @param groups The column containing the different groups of lines
#' @param palette_type A string. Allowed values are \code{"diverging"},
#' \code{"sequential"}, and \code{"qualitative"}. Default is \code{"qualitative"}.
#' @param colors The the colors to be used for the line(s)/point(s)
#' @param point_size A numeric. The size of the points (default is \code{0}, no points)
#' @param dim_breaks A vector of specific dimension values that should be labeled.
#' @param facet_by The column containing the group to facet the plots (if desired).
#' @param area A logical. Should the plot be drawn as an area plot?
#' @param text_size The size of the label text size. Default is 3
#' @param currency A string, usually \code{$}
#' @param measure_unit A string. Can be \code{\%}, \code{K}, or \code{M}
#' @param measure_decimal An integer. The number of decimal places to show.
#' @param ... Additional parameters to pass on it facet_wrap (e.g. scales = free, ncol = 1)
#'
#' @usage quik_lines(df, dimension, measure, groups, palette_type,
#'                         colors, point_size, dim_breaks,
#'                         facet_by, area, text_size,
#'                         currency, measure_unit, measure_decimal, ...)
#'
#' @examples 
#' # Create a line plot from morley data
#' data(morley)
#' ggq <- quik_lines(morley, dimension = 'Run', measure = 'Speed', groups = 'Expt')
#' quik_theme(ggq, axis.text = 'y', axis.title = c('x', 'y'))
#'
#' @export
quik_lines = function(df, dimension, measure, groups = NULL, palette_type = 'qualitative',
                        colors = NULL, point_size = 0, dim_breaks = NULL,
                        facet_by = NULL, area = FALSE, text_size = 3,
                        currency = NULL, measure_unit = NULL, measure_decimal = 0, ...) {
  quik_opts <- set_quik_opts(df, dimension = dimension, measure = measure, groups = groups, 
                             colors = colors, palette_type = palette_type, 
                             measure_decimal = measure_decimal, measure_unit = measure_unit)
  names(df) <- make.names(names(df)); dimension <- make.names(dimension); measure <- make.names(measure)
  if (!is.null(groups)) { 
    if(class(df[, groups]) != 'factor') df[, groups] <- as.factor(df[, groups])
  }
  df <- quik_prepare(df, dimension = dimension, measure = measure, plot_type = 'line',
                     groups = groups, facet_by = facet_by, currency = currency, 
                     measure_unit = measure_unit, measure_decimal = quik_opts$measure_decimal)
  # create initial plot
  gg <- ggplot(df, aes_string(x = dimension, y = measure, group = quik_opts$groups))
  # add lines or area:
  if (area) {
    gg <- gg + geom_area(aes_string(fill = quik_opts$groups), alpha = 0.6)
    p.pos <- position_stack()
    t.pos <- "identity"
  } else {
    gg <- gg + geom_line(aes_string(color = quik_opts$groups))
    p.pos <- "identity"
    t.pos <- "identity"
  }
  # add points
  if (point_size > 0) gg <- gg + geom_point(aes_string(color = quik_opts$groups), size = point_size,
                                            position = p.pos)
  # split to facets if needed
  if (!is.null(facet_by)) gg <- gg + facet_wrap(as.formula(paste0('~', facet_by)), ...)
  # add colors
  gg <- gg + scale_color_manual(values=quik_opts$colors, guide = quik_opts$legend)
  if (area) gg <- gg + scale_fill_manual(values=quik_opts$colors, guide = FALSE)
  # add dimension breaks if necessary
  if (!is.null(dim_breaks)) {
    x.breaks = unique(df[,dimension])[dim_breaks]
    gg <- gg + scale_x_discrete(breaks = x.breaks, drop = FALSE)
    df[!(df[, dimension] %in% x.breaks), 'measure_label'] <- NA
  }
  y.breaks = y.labels = waiver()
  y.expand <- expand_scale(mult = c(0.05, 0.05))
  if (!is.null(measure_unit)) {
    if (measure_unit == '%') y.labels = scales::percent
  }
  gg <- gg + scale_y_continuous(breaks = y.breaks, labels = y.labels, expand = y.expand)
  # add text labels
  if(text_size > 0) gg <- gg + geom_text(family = set_quik_family(), size = text_size, position = t.pos,
                       label = df$measure_label, vjust = -0.5, color = quik_opts$txt.d)
  return(gg)
}


#' Plot a quik points plot
#'
#' ggquik has specific standards for plots, and \code{quik_points} helps your plot follow these.
#'
#' @import ggplot2
#' @importFrom stats as.formula
#'
#' @param df The data frame containing plot data
#' @param dimension Usually the x-axis, values like "quarter" or "account type"
#' @param measure The column containing numerical values to be plotted
#' @param groups The column containing the different groups of lines
#' @param palette_type A string. Allowed values are \code{"diverging"},
#' \code{"sequential"}, and \code{"qualitative"}. Default is \code{"qualitative"}.
#' @param colors The the colors to be used for the line(s)/point(s)
#' @param point_size A numeric. The size of the points (default is \code{0}, no points)
#' @param dim_breaks A vector of specific dimension values that should be labeled.
#' @param facet_by The column containing the group to facet the plots (if desired).
#' @param area A logical. Should the plot be drawn as an area plot?
#' @param text_size The size of the label text size. Default is 3
#' @param currency A string, usually \code{$}
#' @param measure_unit A string. Can be \code{\%}, \code{K}, or \code{M}
#' @param measure_decimal An integer. The number of decimal places to show.
#' @param ... Additional parameters to pass on it facet_wrap (e.g. scales = free, ncol = 1)
#'
#' @usage quik_points(df, dimension, measure, groups, palette_type,
#'                         colors, point_size, dim_breaks,
#'                         facet_by, area, text_size,
#'                         currency, measure_unit, measure_decimal, ...)
#'
#' @examples 
#' # Create a line plot from morley data
#' data(morley)
#' ggq <- quik_points(morley, dimension = 'Run', measure = 'Speed', 
#'                  groups = 'Expt', text_size = 0)
#' quik_theme(ggq, axis.text = 'y', axis.title = c('x', 'y'))
#'
#' @export
quik_points = function(df, dimension, measure, groups = NULL, palette_type = 'qualitative',
                       colors = NULL, point_size = 2, dim_breaks = NULL,
                       facet_by = NULL, area = FALSE, text_size = 3,
                       currency = NULL, measure_unit = NULL, measure_decimal = NULL, ...) {
  quik_opts <- set_quik_opts(df, dimension = dimension, measure = measure, groups = groups, 
                             colors = colors, palette_type = palette_type, 
                             measure_decimal = measure_decimal, measure_unit = measure_unit)
  names(df) <- make.names(names(df)); dimension <- make.names(dimension); measure <- make.names(measure)
  if (!is.null(groups)) { 
    if(class(df[, groups]) != 'factor') df[, groups] <- as.factor(df[, groups])
  }
  df <- quik_prepare(df, dimension = dimension, measure = measure, plot_type = 'line',
                     groups = groups, currency = currency, measure_unit = measure_unit, 
                     measure_decimal = quik_opts$measure_decimal)
  # create initial plot
  gg <- ggplot(df, aes_string(x = dimension, y = measure, group = quik_opts$groups))
  gg <- gg + geom_point(aes_string(color = quik_opts$groups), size = point_size)
  # split to facets if needed
  if (!is.null(facet_by)) gg <- gg + facet_wrap(as.formula(paste0('~', facet_by)), ...)
  # add colors
  gg <- gg + scale_color_manual(values=quik_opts$colors, guide = quik_opts$legend)
  if (!is.null(dim_breaks)) {
    x.breaks = unique(df[,dimension])[dim_breaks]
    gg <- gg + scale_x_discrete(breaks = x.breaks, drop = FALSE)
    df[!(df[, dimension] %in% x.breaks), 'measure_label'] <- NA
  }
  y.breaks = y.labels = waiver()
  y.expand <- expand_scale(mult = c(0.05, 0.05))
  if (!is.null(measure_unit)) {
    if (measure_unit == '%') y.labels = scales::percent
  }
  gg <- gg + scale_y_continuous(breaks = y.breaks, labels = y.labels, expand = y.expand)
  # add text labels
  if(text_size > 0) gg <- gg + geom_text(family = set_quik_family(), size = text_size,
                                         label = df$measure_label, vjust = -1, color = quik_opts$txt.d)
  gg <- gg + labs(x = quik_opts$x.lab, y = quik_opts$y.lab, colour = quik_opts$c.lab)
  return(gg)
}


#' Plot a quik bullet plot
#'
#' ggquik has specific standards for plots, and
#' \code{quik_bullets} helps your plot follow these. A bullet plot
#' \url{http://en.wikipedia.org/wiki/Bullet_graph} is a bar plot with
#' additional progress indicators. None are required, but the options
#' are a partial fill, a dotted line, and a solid line.
#'
#' @import ggplot2
#'
#' @param df The data frame containing plot data
#' @param group_col The column containing the different groups to be faceted
#' @param range_low The column with the low end of the bar
#' @param range_high The column with the high end of the bar
#' @param bar_fill The column with the distance to fill the bar (if needed)
#' @param dotted_line The column with the dotted line distance (if needed)
#' @param solid_line The column with the solid line distance (if needed)
#' @param text_size The size of the label text size. Default is 3
#' @param palette_type A string. Allowed values are \code{"diverging"},
#' \code{"sequential"}, and \code{"qualitative"}. Default is \code{"qualitative"}.
#' @param line_colors A string. What color should be used for the lines (e.g.
#' "gray", "red", "dark red", "blue", "dark blue", "light blue", gold",
#' "green", "purple", "teal")
#' @param currency A string, usually \code{$}
#' @param measure_unit A string. Can be \code{\%}, \code{K}, or \code{M}
#' @param measure_decimal An integer. The number of decimal places to show.
#' @param ... Parameters to pass on to facet_wrap, such as \code{nrow} or \code{ncol}
#'
#' @usage quik_bullets(df, group_col, range_low, range_high, bar_fill,
#'                         dotted_line, solid_line, text_size,
#'                         palette_type, line_colors,
#'                         currency, measure_unit, measure_decimal, ...)
#'                         
#' @examples
#' # load a sample data frame
#' df <- data.frame(group = factor(c('Reliability', 'Accuracy', 'Uptime', 'Efficiency')),
#'                   dotted = sample(90:100, 4), solid = sample(80:100, 4),
#'                   fill = sample(80:100, 4), low = c(50, 75, 80, 80), high = rep(100, 4))
#' # create a plot
#' ggq <- quik_bullets(df, group_col = 'group', range_low = 'low', range_high = 'high',
#'                     bar_fill = 'fill', solid_line = 'solid', dotted_line = 'dotted')
#' quik_theme(ggq, axis.text = 'x')
#'
#' @export
quik_bullets = function(df, group_col, range_low, range_high, bar_fill = NULL,
                          dotted_line = NULL, solid_line = NULL, text_size = 3,
                          palette_type = 'qualitative', line_colors = "Purple",
                          currency = NULL, measure_unit = NULL, measure_decimal = 0, ...) {
  measures <- names(df)[names(df) %in% c(range_low, range_high, bar_fill, solid_line, dotted_line)]
  labels.df <- df[, measures]
  df[, paste0(measures, '_label')] <- sapply(labels.df, format_label, currency = currency, 
                                             measure_unit = measure_unit, measure_decimal = measure_decimal)
  df[, solid_line] <- ifelse(is.na(df[, solid_line]), (df$max + df$min)/2, df[, solid_line])
  df[, dotted_line] <- ifelse(is.na(df[, dotted_line]), (df$max + df$min)/2, df[, dotted_line])
  bar.color <- ggquik::plot_colors$grid.color
  fill.color <- change_shade(bar.color, -4)
  group.colors <- set_group_colors(df[, group_col], palette_type, line_colors)
  txt.d <- ggquik::plot_colors$text.dark
  txt.sd <- change_shade(txt.d, 2)
  gg <- ggplot(df, aes_string(x = group_col, group = group_col, color = group_col))
  if (!is.null(bar_fill)) gg <- gg + geom_crossbar(aes_string(y = range_low, ymin = range_low, ymax = bar_fill),
                         color = 'transparent', fill = fill.color, width = 0.6, size = 1, fatten = 0)
  gg <- gg + geom_crossbar(aes_string(y = range_low, ymin = range_low, ymax = range_high),
                  color = bar.color, width = 0.6, size = 1, fatten = 0)
  if (!is.null(solid_line)) {
    gg <- gg + geom_errorbar(aes_string(ymin = solid_line, ymax = solid_line), width = 0.8, size = 1) +
      geom_text(aes_string(x = 2, y = solid_line, label = paste0(solid_line, '_label')),
                vjust = 3, color = txt.sd, family = set_quik_family(), size = text_size)
  }
  if (!is.null(dotted_line)) {
    gg <- gg + geom_errorbar(aes_string(ymin = dotted_line, ymax = dotted_line), width = 0.8, size = 1, linetype = 'dashed') +
      geom_text(aes_string(x = 2, y = dotted_line, label = paste0(dotted_line, '_label')),
              vjust = 3, color = txt.d, family = set_quik_family(), size = text_size)
  }
  gg <- gg + coord_flip() + # xlim = c(0.75, 1.25), expand = FALSE
    facet_wrap(as.formula(paste0("~", group_col)), scales = 'free', ...)
  y.labels = waiver()
  if (!is.null(measure_unit)) {
    if (measure_unit == '%') y.labels = scales::percent
  }
  gg <- gg + scale_y_continuous(breaks = use_limits(), labels = y.labels, expand = expand_scale(mult = c(.1, .1)))
  gg <- gg + scale_color_manual(values=group.colors)
  return(gg)
}
