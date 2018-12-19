#' Collect hexidecimal colors
#'
#' @param color The Red Hat color(s) to be converted. Either a single color
#'   name, or a vector of color names.
#' @param partial  A logical scalar. Do you want to match partial color names?
#' @param exclude A logical scalar. Do you want get all colors that don't match the color?
#'
#'
#' @examples
#' # Get one color
#' x <- redhat_colors("Purple")
#'
#' # Get a vector of colors
#' y <- redhat_colors(c("Storage 2", "Gray 3", "Red Hat Red", "Gray 10"))
#' z <- redhat_colors("Purple", partial = TRUE)
#' non_grays <- redhat_colors(c("Gray", "Black", "White"), partial = TRUE, exclude = TRUE)
#'
#' @export
redhat_colors = function(color, partial = FALSE, exclude = FALSE) {
  rh_cols <- ggquik::rh_colors
  if (partial) {
    if (length(color) > 1) color = paste(color, collapse = "|")
    rh_cols <- rh_cols[rh_cols$type %in% c('sequential', 'grays'), ]
    hex.numbers <- as.character(rh_cols[grep(color, rh_cols$name), ]$color)
  } else {
    hex.numbers <- as.character(rh_cols[match(color, rh_cols$name),]$color)
  }
  if (exclude) {
    hex.numbers <- as.character(rh_cols[!(rh_cols$color %in% hex.numbers),]$color)
  }
  return(hex.numbers)
}

#' Show available Red Hat colors.
#'
#' @import graphics
#'
#'
#' @examples
#' # Show all Red Hat colors
#' show_redhat_colors()
#'
#' @export
show_redhat_colors <- function() {
  rh_hex <- as.character(ggquik::rh_colors$color)
  rh_txt_col <- as.character(sapply(rh_hex, pick_text_color))
  n <- length(rh_hex)
  ncol <- ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)
  rh_hex <- c(rh_hex, rep(NA, nrow * ncol - length(rh_hex)))
  rh_hex <- matrix(rh_hex, ncol = ncol, byrow = TRUE)
  rh_txt_col <- matrix(rh_txt_col, ncol = ncol, byrow = TRUE)

  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))

  size <- max(dim(rh_hex))
  plot(c(0, size), c(0, -size), type = "n", xlab="", ylab="", axes = FALSE)
  rect(col(rh_hex) - 1, -row(rh_hex) + 1, col(rh_hex), -row(rh_hex),
       col = rh_hex, border = '#FFFFFF')

  rh_names <- gsub(" ", "\n", ggquik::rh_colors$name)
  rh_names <- c(rh_names, rep(NA, nrow * ncol - length(rh_names)))
  rh_names <- matrix(rh_names, ncol = ncol, byrow = TRUE)
  text(col(rh_hex) - 0.5, -row(rh_hex) + 0.5, rh_names, cex = 0.75, col = rh_txt_col)
}

#' Pick text color based on background.
#'
#' In order to always see the text, function uses Gray 10
#' or White based on the "luminance" of the background.
#' Uses W3C guidelines and calculations found on stackoverflow
#' here: \url{https://stackoverflow.com/questions/3942878/
#' how-to-decide-font-color-in-white-or-black-depending-on-background-color}
#'
#' @importFrom grDevices col2rgb
#'
#' @param hex_color A hexidecimal color for the background
#'
#' @export
pick_text_color <- function(hex_color) {
  rgb_col <- col2rgb(hex_color, alpha = FALSE)
  rgb_col <- rgb_col/255
  rgb_col<- ifelse(rgb_col <= 0.03928, rgb_col/12.92, ((rgb_col+0.055)/1.055)^2.4)
  luminance <- 0.2126*rgb_col[1] + 0.7152*rgb_col[2] + 0.0722*rgb_col[3]
  if (luminance > 0.179) return(redhat_colors("Gray 10"))
  else return(redhat_colors("White"))
}

#' Change the shade of the current Red Hat Color
#'
#' Most colors have six shades. This function provides the color that
#' is "n" shades away.
#'
#' @param hex_color A hexidecimal color for the background
#' @param n A positive or negative number to indicate the direction
#' of the shade change.
#'
#' @export
change_shade <- function(hex_color, n) {
  cols <- ggquik::rh_colors
  curr.row <- as.numeric(row.names(cols[cols$color %in% hex_color & cols$type %in% c('sequential', 'grays'),]))
  new.row <- curr.row + n
  return(cols[new.row, 'color'])
}

#' Get the hex of a redhat color in rgb
#'
#' Most colors have six shades. This function provides the color that
#' is "n" shades away.
#'
#' @importFrom grDevices rgb
#'
#' @param r The red value. Default is 0.
#' @param g The green value. Default is 0.
#' @param b The blue value. Default is 0.
#'
#' @export
redhat_hex <- function(r=0, g=0, b=0) {
  return(substr(rgb(r, g, b, 0, maxColorValue = 255), 1, 7))
}
