source("R/common.R")

library(pROC)

set.seed(1)
sim1 <- tibble(
  Time = 1:8,
  SV=rnorm(8, mean = 70, sd = 5)
)

SV_variation <- ggplot(sim1, aes(Time, SV)) +
  geom_hline(aes(yintercept = 70, linetype = '"True" SV')) +
  geom_linerange(aes(ymin = 70, ymax = SV, color = 'Random error'), key_glyph = draw_key_vline, linetype = 3) +
  geom_line(aes(color = "Measured SV"), show.legend = FALSE) +
  geom_point(aes(color = "Measured SV")) +
  scale_color_manual(values = c(darkred, lightblue)) +
  scale_linetype_manual(values = c(2)) +
  guides(color = guide_legend(override.aes = list(shape = c(20, NA),
                                                  linetype = c('blank', 'dotted')))) +
  ylim(60, 80) +
  labs(linetype = NULL, y = 'SV [ml]', color = NULL) +
  theme(panel.grid.major = element_blank(),
        legend.margin = margin(t = -13, b = 0))

save_plot("methods-SV-variation", SV_variation)

# Null sim plot

# Set seed to make random numbers reproducible
set.seed(2)

# Number of patients
n <- 2000

# Generate a SV for each patient.
SV <- rnorm(n, mean = 75, sd = 10)

random_error_sd <- 3

# Add random variation to "true" SV
SVbaseline <- SV + rnorm(n, mean = 0, sd = random_error_sd)
SV100      <- SV + rnorm(n, mean = 0, sd = random_error_sd)
SV500      <- SV + rnorm(n, mean = 0, sd = random_error_sd)

sim_data <- tibble(
  id = 1:n,
  SV_true = SV,
  SVbaseline,
  SV100,
  SV500,
  response_100 = (SV100 - SVbaseline) / SVbaseline,  
  response_500 = (SV500 - SVbaseline) / SVbaseline, 
  responder_500 = response_500 > 0,
  response_400 = (SV500 - SV100) / SV100
)

sample_plot <- head(sim_data, 15) %>% 
  select(id, SVbaseline, SV100, SV500) %>% 
  pivot_longer(-id, names_to = "Window", values_to = "SV") %>% 
  ggplot(aes(Window, SV, group = id)) +
  geom_line() +
  geom_point() +
  labs(y = 'SV [ml]', x = "",
       title = "15 of 2000 simulated subjects") +
  scale_x_discrete(limits = c('SVbaseline', 'SV100', 'SV500'),
                   labels = c('SV<sub>baseline</sub>', 'SV<sub>100</sub>', 'SV<sub>500</sub>')) +
  theme(axis.text.x = ggtext::element_markdown())

predict_500_plot <- ggplot(sim_data, aes(response_100, response_500)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  geom_point(alpha = 0.2, shape = 16) +
  labs(x = '∆SV<sub>100</sub> (MFC)', y = '∆SV<sub>500</sub>')+
  coord_fixed() +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown())

predict_400_plot <- ggplot(sim_data, aes(response_100, response_400)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  geom_point(alpha = 0.2, shape = 16) +
  labs(x = '∆SV<sub>100</sub> (MFC)', y = '∆SV<sub>400</sub>')+
  coord_fixed() +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown())

null_sim_plot <- sample_plot + (predict_500_plot / predict_400_plot) + plot_annotation(tag_levels = "a")

save_plot("method-null-sim", null_sim_plot, width = 16, height = 12)
