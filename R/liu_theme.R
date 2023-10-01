#' LiU light theme
#' @name liu_theme_light
#' @description This function is a LiU-inspired theme for ggplot2
#' @return returns a ggplot2 theme
#' @export
#' @importFrom ggplot2 ggplot theme element_text element_rect element_blank

liu_theme_light <- function() {
  ggplot2::theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.background = element_rect(fill = "lightblue"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "lightblue"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )
}