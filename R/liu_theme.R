liu_theme <- function() {
  theme(
    #starting code for the LiU theme, needs to be modified to fit the LiU theme
    #doc : https://blog.liu.se/itimhvartattveta/tag/grafisk-profil/ (Swedish only)
    #Might need two themes : white on vivid sky blue
    #                        black on light sky blue
    text = element_text(size = 12, family = "Arial"),
    axis.title = element_text(face = "bold", size = 14),
    panel.grid.major = element_line(color = "gray", size = 0.2)
  )
}