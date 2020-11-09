library(fivethemes)
# examples
library(ggplot2)
p <- ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
  geom_point() +
  annotate("text", x = 4.5, y = 34, label = "dummy label", parse = TRUE) +
  labs(title = "Example title", subtitle = "Sample subtitle", caption = "Example caption")

p + fivethemes::theme_5classic(grid = "Xx")
p + fivethemes::theme_5minimal()
p + fivethemes::theme_5dark(grid = "Xx")




# my_theme <- function(bg_fill = "#F3F3F3", ...) {
# #cream background
# theme_cream <- function(bg_fill = "#F2F1EF"){
