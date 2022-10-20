source("common.R")

plot_vent_settings <- function(vent_settings, 
                               extra_deadspace = 0, deadspace = 1, v_t_norm, rr_norm) {
  
  normal_ventilation <- tibble(
    v_t = v_t_norm, # Guess for settings resulting in normoventilation.
    rr = rr_norm) # The normoventilation line corresponds to the alveolar ventilation with these settings.
  normal_alveolar_minute_vol <-
    (normal_ventilation$v_t - deadspace) * normal_ventilation$rr
  iso_minvol_line <-
    function(rr, minute_vol = normal_alveolar_minute_vol)
      (minute_vol / rr) + deadspace + extra_deadspace
  iso_minvol_line <- Vectorize(iso_minvol_line)
  
  
  line_labels <- tibble(
    x = 20.5,
    y = iso_minvol_line(x),
    label = c('Hyperventilation', 'Hypoventilation')
  )
  
  grid_curves <- lapply(seq(25, 300, by = 25), function(x) {
    rr = seq(7, 38, by = 0.5)
    minute_vol = normal_alveolar_minute_vol * x / 100
    tibble(
      percent = x,
      minute_vol = minute_vol,
      rr = rr,
      v_t = iso_minvol_line(rr, minute_vol)
    )
  }) |> {\(df) do.call(rbind, df)}()
  
  grid_curves_labs <- filter(
    grid_curves,
    (rr == 11 & percent == 50) |
      (rr == 12.5 & percent == 75) |
      (rr == 14 & percent == 100) |
      (rr == 17 & percent == 150) |
      (rr == 20 & percent == 200) |
      (rr == 23 & percent == 250) 
    #(rr == 26 & percent == 300) 
  ) |>
    mutate(percent_lab = paste(percent, '%'))
  
  ggplot(vent_settings, aes(x = rr, y = v_t, group = 1)) +
    geom_line(
      data = grid_curves,
      aes(x = rr, y = v_t, group = minute_vol),
      color = '#aaaaaa',
      size = 0.2
    ) +
    stat_function(fun = iso_minvol_line,
                  color = darkblue,
                  xlim = c(7, 38)) +
    geom_text(
      data = line_labels,
      aes(x, y, label = label),
      color = '#777777',
      angle = -23,
      nudge_y = c(0.22,-0.20),
      nudge_x = c(0.4, -0.2),
      size = 2
    ) +
    geom_text(
      data = grid_curves_labs,
      aes(rr, v_t, label = percent_lab),
      color = '#777777',
      angle = -38,
      nudge_y = 0.05,
      nudge_x = 0.5,
      size = 1.8
    ) +
    geom_point(size = 3.8, color = darkred) +
    geom_text(
      aes(label = n),
      color = 'white',
      size = 2.2,
      nudge_x = 0,
      nudge_y = 0
    ) +
    labs(x = 'Respiratory rate [min⁻¹]', y = 'Tidal Volume [ml kg(pbw)⁻¹]') +
    scale_x_continuous(breaks = unique(vent_settings$rr), expand = expansion(0.1)) +
    scale_y_continuous(breaks = c(4, 6, 8, 10), expand = expansion(0.1)) +
    coord_cartesian(xlim = c(10, 31), ylim = c(4, 10)) +
    #ggtitle(ifelse(extra_deadspace == 0, "Normal deadspace", "Extra deadspace")) +
    theme(axis.title.x = ggtext::element_markdown(size = 8),
          axis.title.y = ggtext::element_markdown(size = 8),
          axis.text = element_text(size = 7))
  
  
}

make_vent_settings_plot <- function() {
  # Dataframe of ventilator settings
  vent_settings <- tibble(
    v_t = c(10,
            8, 8, 8, 8,
            6, 6, 6, 6,
            4 
    ),
    rr  = c(10, 
            10, 17, 24, 31,
            10, 17, 24, 31, 
            10
    ),
    #n = sample(1:length(rr), length(rr)) #Random order
    n = c(10,
          9 , 6, 2, 4,
          8 , 5, 1, 3,
          7
    )
  ) |> 
    arrange(n)
  
  vent_plot <- plot_vent_settings(
    vent_settings,
    deadspace = 1,
    rr_norm = 14,
    v_t_norm = 7
  )
  
  vent_plot
}

vent_plot <- make_vent_settings_plot()

save_plot("methods-vent-settings", width = 10, height = 8, scale = 0.7)
