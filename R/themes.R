
my_theme <-
  theme_bw() +
  theme(
    text = element_text(family = "sans"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.direction = "vertical",
    legend.title.position = "top",
    #legend.background = element_rect(colour = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(colour = "white", hjust = 0),
    plot.caption = element_text(hjust = 0)
  )

my_colour_scale <-
  scale_colour_manual(values = c("cyan", "magenta", "blue"))

my_fill_scale <-
  scale_fill_manual(values = c("cyan", "magenta", "blue"))



