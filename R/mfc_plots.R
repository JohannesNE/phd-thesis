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

save_plot("method-reg-to-the-mean", SV_variation)

# Autocorrelation
set.seed(1)
sim2 <- tibble(
  Time = 1:60,
  mean_SV = 70 + cos(Time/5)*2 + cos(2 + Time/6)*2 + cos(1 + Time/3)*2,
  SV=rnorm(60, mean = mean_SV, sd = 2)
)

SV_variation_autocor <- ggplot(sim2, aes(Time, SV)) +
  geom_hline(aes(yintercept = 70, linetype = 'Mean SV')) +
  #geom_linerange(aes(ymin = 70, ymax = SV, color = 'Random error'), key_glyph = draw_key_vline, linetype = 3) +
  geom_line(aes(color = "Measured SV"), show.legend = FALSE) +
  geom_line(aes(y = mean_SV, color = "Actual SV"), linetype = 2) +
  #geom_point(aes(color = "Measured SV")) +
  scale_color_manual(values = c(darkred, lightblue)) +
  scale_linetype_manual(values = c(2)) +
  guides(color = guide_legend(override.aes = list(linetype = c('dashed', 'solid')))) +
  ylim(60, 80) +
  labs(linetype = NULL, y = 'SV [ml]', color = NULL) +
  theme(panel.grid.major = element_blank(),
        legend.margin = margin(t = -13, b = 0))

ggsave("/home/johannes/MEGAsync/Docs/PhD/8_Assets/figures/defense/SV_autocor.png", 
       width = 8, height = 3,
       SV_variation_autocor)


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
SV100b      <- SV + rnorm(n, mean = 0, sd = random_error_sd)

sim_data <- tibble(
  id = 1:n,
  SV_true = SV,
  SVbaseline,
  SV100,
  SV100b,
  SV500,
  response_100 = (SV100 - SVbaseline) / SVbaseline,  
  response_500 = (SV500 - SVbaseline) / SVbaseline, 
  responder_500 = response_500 > 0,
  response_400 = (SV500 - SV100) / SV100,
  response_400b = (SV500 - SV100b) / SV100
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

save_plot("method-mfc-null-sim", null_sim_plot, width = 16, height = 12)

# Plot with 100b

sample_plot_b <- head(sim_data, 10) %>% 
  select(id, SVbaseline, SV100, SV100b, SV500) %>% 
  pivot_longer(-id, names_to = "Window", values_to = "SV") %>% 
  ggplot(aes(Window, SV, group = id)) +
  geom_line() +
  geom_point() +
  labs(y = 'SV [ml]', x = "",
       title = "10 of 2000 simulated subjects") +
  scale_x_discrete(limits = c('SVbaseline', 'SV100', 'SV100b', 'SV500'),
                   labels = c('SV<sub>baseline</sub>', 'SV<sub>100</sub>', 
                              'SV<sub>100</sub>b', 'SV<sub>500</sub>')) +
  theme(axis.text.x = ggtext::element_markdown())

predict_400b_plot <- ggplot(sim_data, aes(response_100, response_400b)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  geom_point(alpha = 0.2, shape = 16) +
  labs(x = '∆SV<sub>100</sub> (MFC)', 
       y = expression("Δ"*SV[400]*b == frac(SV[500]~"-"~SV[100]*b, SV[100]*b))
       )+
  coord_fixed() +
  theme(axis.title.x = ggtext::element_markdown())

null_sim_plot_b <- sample_plot_b + predict_400b_plot + plot_annotation(tag_levels = "a")

save_plot("results-mfc-null-sim-better", null_sim_plot_b, width = 18, height = 8)

## Muller reanalysis

muller_data <- readRDS("sample_data/muller_data.RDS")            

line_colors <- c(darkblue, darkred)

scatterPlot500 = ggplot(muller_data, aes(x = DeltaVTI100, y = DeltaVTI500))+
  geom_abline(intercept = 15, slope = 0, color = line_colors[2]) +
  geom_abline(intercept = 15, slope = 1.15, linetype = "dashed", color = line_colors[1]) +
  geom_point(color = "#222222", size = 1.2) +
  #labs(title = "Reconstruction of fig. 3A from Muller et al, 2011") +
  ylab(expression(ΔSV[500])) +
  xlab(expression(ΔSV[100])) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(ylim = c(-15,80), xlim = c(-15,75))

scatterPlot400 = ggplot(muller_data, aes(x = DeltaVTI100, y = DeltaVTI400))+
  geom_abline(aes(color = "ΔSV₄₀₀ = 15%", linetype = "ΔSV₄₀₀ = 15%", intercept = 15, slope = 0)) +
  geom_function(aes(color = "ΔSV₅₀₀ = 15%", linetype = "ΔSV₅₀₀ = 15%"), fun = function(x) 100*(0.15-x/100)/(x/100 + 1), xlim = c(-20, 50),
                key_glyph = "abline") +
  scale_color_manual(values = unname(line_colors)) +
  scale_linetype_manual(values = c(2,1)) +
  geom_point(color = "#222222", size = 1.2) +
  labs(#title = expression(paste(ΔSV[400], " calculated from ", ΔSV[100]~and~ΔSV[500])),
       color = NULL, linetype = NULL) +
  guides(linetype = guide_legend(), color = guide_legend()) +
  ylab(expression(ΔSV[400])) +
  xlab(expression(ΔSV[100])) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(ylim = c(-15,80), xlim = c(-15,75)) +
  theme(legend.position = c(0.7, 0.85))

gen_roc_plot <- function(roc_sim) {
  ci_auc <- ci.auc(roc_sim)
  ggroc(roc_sim, color = "#222222") +
    geom_abline(intercept = 1, linetype = 2) +
    geom_label(
        x = 0,
        y = 0,
        label.size = 0,
        hjust = 1,
        vjust = 0,
        size = 3.5,
        label = sprintf("AUC = %.2f",
                        ci_auc[2])
    ) +
    labs(x = 'Specificity', y = 'Sensitivity'#, 
         #caption = sprintf("AUC = %.2f; 95%% CI [%.2f, %.2f]", ci_auc[2], ci_auc[1], ci_auc[3])
         ) +
    coord_cartesian(expand = TRUE, xlim = c(1, -0.01), ylim =c(0,1.01)) +
    theme(plot.caption = element_text(hjust = 0.5))
}

roc_500 <- pROC::roc(DeltaVTI500 > 15 ~ DeltaVTI100, data = muller_data, direction = '<') 
ROCplot500 = gen_roc_plot(roc_500) 

roc_400 <- pROC::roc(DeltaVTI400 > 15 ~ DeltaVTI100, data = muller_data, direction = '<')
ROCplot400 = gen_roc_plot(roc_400) 

combined_muller <- scatterPlot500 + scatterPlot400 + ROCplot500 + ROCplot400 + 
  plot_layout(nrow = 2, ncol = 2) + plot_annotation(tag_levels = "a") 

save_plot("results-muller-plot", combined_muller, width = 13, height = 12)
