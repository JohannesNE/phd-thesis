source("R/common.R")

options(ggplot2.discrete.colour = c(darkred, darkblue))

library(brms)
library(tidybayes)

m1 <- readRDS("sample_data/m1.rds")

PPV_df <- read_csv("sample_data/vent_setting_study-ppv.csv") |> 
  mutate(
    # Create factors for ventilator settings
    id_f = factor(id),
    vent_rel_vt_f = factor(vent_rel_vt, levels = c(10, 8, 6, 4)),
    vent_RR_f = factor(vent_RR, levels = c(10, 17, 24, 31)),
    vent_setting = interaction(vent_rel_vt, vent_RR, drop = TRUE) |> 
      forcats::fct_relevel("10.10", "8.10", "6.10", "4.10",
                           "8.17", "6.17",
                           "8.24", "6.24",
                           "8.31", "6.31")
  ) |> 
  # Remove the 13 / 520 rows without a PPV value. PPV is missing either because the
  # ventilator setting was not applied or because PPV estimation was infeasible 
  # because of frequent extra-systoles (≥3 in one window).
  drop_na(PPV_gam) 

# Labels for vent settings
vent_setting_levels <- c(
  "10.10" = "VT=10, RR=10",
  "8.10" = "VT=8, RR=10",
  "6.10" = "VT=6, RR=10",
  "4.10" = "VT=4, RR=10",
  "8.17" = "VT=8, RR=17",
  "6.17" = "VT=6, RR=17",
  "8.24" = "VT=8, RR=24",
  "6.24" = "VT=6, RR=24",
  "8.31" = "VT=8, RR=31",
  "6.31" = "VT=6, RR=31"
)

vt_levels <- c(
  "10" = "VT=10",
  "8" = "VT=8",
  "6" = "VT=6",
  "4" = "VT=4"
)

rr_levels <- c(
  "10" = "RR=10",
  "17" = "RR=17",
  "24" = "RR=24",
  "31" = "RR=31"
)

# Pivot PPV data frame to long format with one column for PPV
# and one column indicating the method (Classic or GAM)
PPV_df_long <- PPV_df |> 
  pivot_longer(c(PPV_gam, PPV_classic),
               values_to = "PPV",
               names_to = "PPV_method",
               names_prefix = "PPV_") |> 
  mutate(PPV_vt = 10*PPV/vent_rel_vt,
         label = vent_setting_levels[as.character(vent_setting)] |> 
           factor(levels = vent_setting_levels),
         PPV_method = factor(PPV_method, levels = c("gam", "classic"))) 



observed_plot <- ggplot(PPV_df_long, aes(label, PPV)) + 
  ggbeeswarm::geom_quasirandom(aes(color = PPV_method), 
                               dodge.width=.6,
                               width = 0.1,
                               size = 0.7,
                               shape=16) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  scale_color_discrete(limits = c("gam", "classic"), labels = c("GAM", "Classic")) +
  labs(x="", y="PPV [%]",
       color = "PPV method") +
  theme(axis.text.x = element_text(hjust = 1, angle = 30),
        legend.position = c(0.5, 0.9),
        legend.direction = "horizontal",
        legend.justification = c(0.5,0.5),
        plot.margin = margin(t = 5, b = 0, l = 10, r = 5),
        legend.box.background = element_rect(color = NA, fill = "white"),
        legend.text = element_text(size = rel(1)))

observed_plot

save_plot("results-ppv-observed", observed_plot, width = 18, height = 6)

## Plot ventilation effects

# Intercepts 
intercept_draws_m1 <- gather_draws(m1, `b_PPV_method(gam|classic)`, regex = TRUE) |> 
  mutate(PPV_method = str_remove(.variable, "b_PPV_method") |> 
           factor(levels = c("gam", "classic")),
         intercept = exp(.value),
         label = "VT=10\nRR=10")

intercept_plot_m1 <- ggplot(intercept_draws_m1, aes(label, intercept, color = PPV_method)) +
  stat_pointinterval(point_size = 1, 
                     interval_size = 1,
                     position = position_dodge(width = 0.4),
                     .width = 0.95) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(x="", y="PPV",
       title = "Intercept") +
  theme(legend.position = "none")

intercept_plot_m1

# Contrasts
contrast_draws_m1 <- gather_draws(m1, `b_PPV_method(gam|classic):.+`, regex = TRUE) |> 
  separate(.variable, into = c("PPV_method", "setting"), sep = ":") |> 
  separate(setting, into = c("setting_type", "setting"), sep = "_f") |> 
  mutate(PPV_method = str_remove(PPV_method, "b_PPV_method") |> 
           factor(levels = c("gam", "classic")),
         rel_effect = exp(.value))

contrast_draws_vt_m1 <- filter(contrast_draws_m1, setting_type == "vent_rel_vt") |> 
  mutate(label = vt_levels[setting] |> factor(levels = vt_levels))

contrast_plot_layers <- list(
  stat_pointinterval(point_size = 1, 
                     interval_size = 1,
                     position = position_dodge(width = 0.4),
                     .width = 0.95),
  labs(y = "", x = ""), 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = seq(0.4, 1, by = 0.2)),
  coord_cartesian(ylim = c(0.4, 1)),
  theme(legend.position = "none") 
)

contrast_plot_vt_m1 <- ggplot(contrast_draws_vt_m1, 
                              aes(label, rel_effect, color = PPV_method)) + 
  contrast_plot_layers +
  labs(title = "Effect of VT", 
       subtitle = "Relative to VT=10 ml kg⁻¹")


contrast_draws_rr_m1 <- filter(contrast_draws_m1, setting_type == "vent_RR") |> 
  mutate(label = rr_levels[setting] |> factor(levels = rr_levels))

contrast_plot_rr_m1 <- ggplot(contrast_draws_rr_m1, 
                              aes(label, rel_effect, color = PPV_method)) +
  contrast_plot_layers +
  labs(title = "Effect of RR", 
       subtitle = "Relative to RR=10 min⁻¹")


contrast_plot_vt_m1 + contrast_plot_rr_m1

param_plot_design_m1_simple <- "
  ABC
"

m1_plot_simple <-  
  intercept_plot_m1 + contrast_plot_vt_m1 + contrast_plot_rr_m1 + 
  plot_layout(design = param_plot_design_m1_simple, 
              widths = c(1.1, 3, 3)
  ) +
  plot_annotation(tag_levels = "a", theme = theme(plot.tag.position = c(0.1, 1))) 

save_plot("results-mix-mod-est", m1_plot_simple, width = 18, height = 8, scale = 1)

# M1 spread
sd_t <- function(sigma, nu) {
  stopifnot(nu > 2)
  sqrt( sigma^2 * (nu / (nu-2)) )
}

newdata_method_setting <- PPV_df_long |> 
  tidyr::expand(PPV_method, nesting(vent_setting, 
                                    vent_rel_vt, vent_RR,
                                    vent_rel_vt_f, vent_RR_f))

# Make draws of mean of posterior predictions (epred) 
# include sigma and nu for each draw (they are used to calculate SD).
vent_setting_epred_m1 <- newdata_method_setting |> 
  add_epred_draws(m1, re_formula = NA,
                  dpar = c("sigma", "nu")) |> 
  mutate(label = vent_setting_levels[as.character(vent_setting)] |> 
           factor(levels = vent_setting_levels),
         SD = sd_t(sigma, nu),
         CV = SD/.epred)

sd_plot_m1 <- ggplot(vent_setting_epred_m1, aes(label, 
                                                CV,
                                                color = PPV_method)) +
  stat_pointinterval(position = position_dodge(width = 0.3), 
                     .width = 0.95, interval_size = 1, 
                     point_size = 1) + 
  scale_y_continuous(limits = c(0, NA), labels = scales::label_percent()) + 
  labs(x="", y="SD(residuals) / E(PPV)", color = "PPV method") +
  scale_color_discrete(limits = c("gam", "classic"), labels = c("GAM", "Classic")) +
  guides(.width = guide_none()) +
  theme(axis.text.x = element_text(hjust = 1, angle = 25),
        legend.position = c(0.5, 0.9),
        legend.direction = "horizontal",
        legend.justification = c(0.5,0.5),
        legend.box.background = element_rect(color = NA, fill = "white"),
        legend.text = element_text(size = rel(1)),
        plot.margin = margin(t = 5, r = 5, b = 0, l=10)
        )

sd_plot_m1

save_plot("results-mix-mod-var", sd_plot_m1, width = 16, height = 8)
