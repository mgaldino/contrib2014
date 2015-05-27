## Theme for Tranasparência Brasil

theme_tb <- function(base_size = 12, base_family = "calibri", legenda="none", dir.Legenda="horizontal") {
   theme(
     line = element_line(),
     axis.title = element_text(size=base_size),
     axis.text = element_text(size=14),
     axis.ticks = element_blank(),
     axis.line = element_blank(),
     legend.background = element_rect(),
     legend.position = legenda,
     legend.direction = dir.Legenda,
     legend.box = "vertical",
     panel.grid = element_line(colour = NULL),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.background = element_blank(),
     strip.background=element_rect())
}

