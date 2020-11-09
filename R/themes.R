
#' theme_five
#'
#' @param grid (string) Grid lines. Options include any
#' combination of "X", "Y", "x" and "y"
#' @param ... Other arguments passed to ggplot methods.
theme_five <- function(grid = "none", ...) {
  if(!is.character(grid)) stop("grid must be a character.")
  if(.Platform$OS.type == "unix") {
    title_family <- "Lora"
    font_family <- "Lato Semibold"
  } else{
    title_family <- "Segoe UI"
    font_family <- "Segoe UI Semibold"
  }
  out <- ggplot2::theme(
    plot.title = ggplot2::element_text(color = "gray10", size = 16, family = title_family, face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "gray40", size = 12, family = font_family),
    plot.caption = ggplot2::element_text(color = "gray40", size = 11, family = font_family),
    axis.title.x = ggplot2::element_text(color = "grey20", family = font_family, margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = ggplot2::element_text(color = "grey20", family = font_family, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks = ggplot2::element_blank(), legend.position = "none",
    axis.line = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "grey80", size = 0.25),
    panel.grid.minor = ggplot2::element_line(color = "grey80", size = 0.25)
  )
  if (grid != "none") {
      if (!stringr::str_detect(grid, "Y")) out <- out + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      if (!stringr::str_detect(grid, "X")) out <- out + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      if (!stringr::str_detect(grid, "y")) out <- out + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      if (!stringr::str_detect(grid, "x")) out <- out + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  } else {
    out <- out + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  out
}

#' theme_5classic
#'
#' @param grid (string) Grid lines. Options include any
#' combination of "X", "Y", "x" and "y"
#' @param ... Other arguments passed to ggplot methods.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
#'   geom_point() +
#'   annotate(
#'     "text", x = 4.5, y = 34,
#'     label = "2019", parse = TRUE
#'     ) +
#'   labs(
#'     title = "Example title",
#'     subtitle = "Sample subtitle",
#'     caption = "Example caption"
#'   )
#' p + theme_5classic(grid = "Xx")
#' }
#'
theme_5classic <- function(grid = NULL) {
  ggplot2::theme_classic() + theme_five(grid)
}

#' theme_5minimal
#'
#' @param grid (string) Grid lines. Options include any
#' combination of "X", "Y", "x" and "y"
#' @param bg_fill (string) Plot backgroud file colour.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
#'   geom_point() +
#'   annotate(
#'     "text", x = 4.5, y = 34,
#'     label = "2019", parse = TRUE
#'     ) +
#'   labs(
#'     title = "Example title",
#'     subtitle = "Sample subtitle",
#'     caption = "Example caption"
#'   )
#' p + theme_5minimal()
#' }
#'
theme_5minimal <- function(grid = "none", bg_fill = "#F3F3F3") {
  ggplot2::theme_minimal() +
    theme_five(grid) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = bg_fill, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_fill, color = NA)
    )
}

#' theme_5dark
#'
#' @param grid (string) Grid lines. Options include any
#' combination of "X", "Y", "x" and "y".
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
#'   geom_point() +
#'   annotate(
#'     "text", x = 4.5, y = 34,
#'     label = "2019", parse = TRUE
#'     ) +
#'   labs(
#'     title = "Example title",
#'     subtitle = "Sample subtitle",
#'     caption = "Example caption"
#'   )
#' p + theme_5dark(grid = "Xx")
#' }
theme_5dark <- function(grid = TRUE) {
  font_family <- "Quattrocento Sans"
  title_family <- "Lora"

  title_colour <- "#F5F5F5"
  subtitle_colour <- "#A0A0A0"
  caption_colour <- "#888888"
  axes_colour <- "#8F9394"
  grid_colour <- "#606060"
  text_colour <- "#5D5E62"
  axes_label_colour <- "#888888"
  background_colour <- "#36373B"

  bg_rect <- ggplot2::element_rect(fill = background_colour, color = background_colour)

  theme_five(grid) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_family),
      plot.background = bg_rect,
      panel.background = bg_rect,
      legend.background = bg_rect,
      plot.title = ggplot2::element_text(color = title_colour, size = 16, family = title_family, face = "bold"),
      plot.subtitle = ggplot2::element_text(color = subtitle_colour, size = 12, family = font_family, face = "bold"),
      plot.caption = ggplot2::element_text(color = caption_colour, size = 11, family = font_family),
      axis.title.x = ggplot2::element_text(color = axes_label_colour, family = font_family, margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = ggplot2::element_text(color = axes_label_colour, family = font_family, margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
      axis.text = ggplot2::element_text(color = caption_colour, size = 9, family = font_family),
      panel.grid.major = ggplot2::element_line(color = grid_colour, size = 0.25),
      panel.grid.minor = ggplot2::element_line(color = grid_colour, size = 0.25)
    )
}






