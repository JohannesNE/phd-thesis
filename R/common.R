library(tidyverse)
library(patchwork)

# Setup theme

darkblue <- "#43658B"
lightblue <- "#4E89AE"
darkred <- "#ab2826"
lightred <- "#ED6663"
orange <- "#FFA372"

darkcolor <- "#222222"

theme_thesis <- function(..., base_size = 8) {
  theme_minimal(base_size, base_family = "Helvetica") %+replace% 
    theme(
      text = element_text(color = darkcolor),
      panel.border = element_blank(),
      axis.text = element_text(color = darkcolor, size = rel(0.9)),
      axis.line = element_line(color = darkcolor),
      axis.ticks = element_line(color = darkcolor),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linetype = 'dotted', color = "#bbbbbb"),
      panel.spacing = unit(2, "mm"),
      strip.text = element_text(color = darkcolor, size = rel(0.9)),
      legend.key = element_blank(),
      plot.margin = unit(c(2, 2, 1, 1), "mm"),
      plot.background = element_rect(fill = "white", color = NA),
      #panel.background = element_rect(fill = "white"),
      plot.subtitle = element_text(size = rel(1.1), hjust = 0),
      plot.tag.position = c(0, 1),
      plot.tag = element_text(size = rel(1.2), margin = margin(l = 1, unit = "lines"))
    )
}

# Default colors
update_geom_defaults("point",   list(colour = darkred))
update_geom_defaults("line",   list(colour = darkred))

theme_set(theme_thesis())


# Save plots
save_plot <- function(name, plot = last_plot(), units = "cm", width = 12, height = 8, dpi = 300, ...) {
  plot_path <- paste0("figures/", name)
  
  c(
    ggsave(paste0(plot_path, ".png"), plot = plot, device = ragg::agg_png,
           units = units, width = width, height = height, dpi = dpi, ...),
    ggsave(paste0(plot_path, ".pdf"), plot = plot, device = cairo_pdf,
           units = units, width = width, height = height, ...)
  )
}
