# Scrpit with my functions

##  decimal mark

dec1 <- function(x, accuracy = .1){
  scales::comma(x, big.mark = ",", decimal.mark = ".",accuracy = accuracy)}

## ggplot2 Theme
theme_trueMinimal = function(font_size = 12,
                             font_family = "",
                             center_axis_labels = FALSE){
  if (center_axis_labels) {
    axis_just <- 0.5
  }
  else {
    axis_just <- 1
  }
  ggplot2::theme(
    text = ggplot2::element_text(
      family = font_family,
      face = "plain",
      colour = "gray46",
      size = font_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    # axis.text.x = ggplot2::element_text(),
    # axis.text.y = ggplot2::element_text(),
    axis.title.x = ggplot2::element_text(hjust = axis_just),
    axis.title.y = ggplot2::element_text(hjust = axis_just),
    title = ggplot2::element_text(colour='gray33', size = font_size*1.25),
    legend.key = ggplot2::element_rect(fill='transparent', colour = NA),
    legend.background = ggplot2::element_rect(fill='transparent', colour = NA),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  )
}