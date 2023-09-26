liu_theme_light <- function() {
  theme(
    text = element_text( size = 12),
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

liu_theme_dark <- function(){
  theme(
    text = element_text( size = 12),
    axis.text = element_text(size = 10, color = "white"),
    axis.title = element_text(size = 12, color = "white"),
    plot.title = element_text(size = 14, face = "bold", color = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "deepskyblue2"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "deepskyblue2"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )
}
#Note : Cannot change the font because availble font is OS dependant
#To use theme  : include the following line:
#source("liu_theme.R")

#Then, when using ggplot, simply use "+ liu_theme_light/dark()"