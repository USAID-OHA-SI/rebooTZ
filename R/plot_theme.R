##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  create a consistent plot theme
##  DATE:     2019-02-13


# PLOT THEME --------------------------------------------------------------

plot_theme <- function() {
  theme(text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14, color = "#595959"),
        panel.grid = element_line(color = "#ebebeb"),
        plot.title = element_text(size = 15, face = "bold", color = "black"),
        plot.caption = element_text(size = 11,  color = "#595959")
  )
}