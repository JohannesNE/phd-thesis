source("R/common.R")

library(mgcv)

# Generate data

set.seed(1)

n <- 70

X <- exp(runif(n, -1, log(5.5)))

y_func <- function(x) {
  sin(x)
}

Y <- y_func(X) + rnorm(n, sd = 0.2)

# plot(X, Y)

sim <- tibble(Y, X)

sim_gam_cr <- gam(Y ~ s(X, k = 8, bs = 'cr', fx = FALSE), data = sim, method = 'REML')
sim_gam_cc <- gam(Y ~ s(X, k = 8, bs = 'cc', fx = FALSE), knots = list(X=c(0, 2*pi)), data = sim, method = 'REML')
sim_gam_cc_unpen20 <- gam(Y ~ s(X, k = 20, bs = 'cc', sp = 0, fx = FALSE), knots = list(X=c(0, 2*pi)), data = sim, method = 'REML')
sim_gam_cc_pen_sp10000 <- gam(Y ~ s(X, k = 20, bs = 'cc', sp = 1e10, fx = FALSE), knots = list(X=c(0, 2*pi)), data = sim, method = 'REML')
sim_gam_cc_pen20 <- gam(Y ~ s(X, k = 20, bs = 'cc', fx = FALSE), knots = list(X=c(0, 2*pi)), data = sim, method = 'REML')
sim_gam_lm <- lm(Y ~ X, data = sim)

sim_pred <- tibble(X = seq(0, 2*pi, length.out = 200),
                   Y_cr = predict(sim_gam_cr, newdata = tibble(X=X)),
                   Y_cc = predict(sim_gam_cc, newdata = tibble(X=X)),
                   Y_unpen20 = predict(sim_gam_cc_unpen20, newdata = tibble(X=X)),
                   Y_pen_sp10000 = predict(sim_gam_cc_pen_sp10000, newdata = tibble(X=X)),
                   Y_pen20 = predict(sim_gam_cc_pen20, newdata = tibble(X=X)),
                   Y_lm = predict(sim_gam_lm, newdata = tibble(X=X))
) 

labels1 <- fct_inorder(
  c(
    'Y = sin(X)',
    'Linear regression',
    'Natural cubic spline \n(penalized, 8 knots)',
    'Cyclic cubic spline \n(penalized, 8 knots*)'
  )
)

plot_sim_lm <- ggplot(sim, aes(X, Y)) +
  geom_point(color = 'gray20', size = 1) +
  geom_line(aes(y = Y_lm), data = sim_pred) +
  labs(title = "Linear regression")

plot_sim_gam <- ggplot(sim, aes(X, Y)) +
  geom_point(color = 'gray20', size = 1) +
  geom_line(aes(y = Y_cr), data = sim_pred) +
  labs(title = "GAM")

lm_vs_gam <- plot_sim_lm + plot_sim_gam

save_plot("method-lm-vs-gam", lm_vs_gam, width = 15)


plot_sim_gam_cyclic <- ggplot(sim, aes(X, Y)) +
  stat_function(aes(colour = "Y = sin(X)",
                    linetype = "Y = sin(X)"), fun = y_func) +
  geom_point(color = 'gray20', size = 1) +
  geom_line(aes(y = Y_cc, colour = "Cyclic spline",
                linetype = "Cyclic spline"), data = sim_pred) +
  scale_color_manual(values = c(darkred, "gray50")) +
  labs(linetype = NULL, color = NULL) 

save_plot("method_gam_cyclic", plot_sim_gam_cyclic)

## Wiggliness

plot_sim_wiggliness <- ggplot(sim, aes(X, Y)) +
  geom_point(color = 'gray20', size = 1) +
  geom_line(aes(y = Y_unpen20, color = "λ=0", linetype = "λ=0"), data = sim_pred) + 
  geom_line(aes(y = Y_pen20, color = "Automatic λ", linetype = "Automatic λ"), data = sim_pred) + 
  geom_line(aes(y = Y_pen_sp10000, color = "λ=∞", linetype = "λ=∞"), data = sim_pred) + 
  scale_color_manual(values = c(darkred, darkblue, orange),
                     breaks = c("λ=0", "Automatic λ", "λ=∞")) +
  scale_linetype_manual(values = c('solid', '21', "52"),
                        breaks = c("λ=0", "Automatic λ", "λ=∞")) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  labs(color = 'Cyclic splines',
       linetype = 'Cyclic splines')

save_plot("method-gam-wiggliness", plot_sim_wiggliness)


# PP gam ==================

sample_pp <- read_rds("sample_data/sample_PP.RDS")

beats <- sample_pp$beats %>% 
  mutate(resp_n = 1 + resp_n - min(resp_n))


insp <- sample_pp$insp_start

# Design plots

scale_shape_PP <- scale_shape_manual(values = c(1, 2, 17), 
                                     limits = c('Diastole', 'Systole', 'Inspiration start'),
                                     name = '')

PP_plot <- ggplot(beats, aes(time, PP)) +
  geom_line(color = darkcolor) +
  geom_point(color = darkcolor) +
  geom_point(aes(time, y = 53, shape = 'Inspiration start'), 
             color = darkcolor, data = insp, show.legend = FALSE) +
  geom_point(aes(color = as.factor(resp_n)), 
             data = filter(beats_pre, resp_n < 5), size = 1.6, show.legend = FALSE) +
  geom_line(aes(color = as.factor(resp_n), group = as.factor(resp_n)), 
            data = filter(beats_pre, resp_n < 5), show.legend = FALSE) +
  #scale_color_manual(values = unname(plot_colors_alt)) +
  scale_x_continuous(limits = c(0,30), expand = c(0.02,0.02)) +
  labs(x = 'Time [s]',
       y = ' PP [mmHg]',
       shape = NULL) +
  ggtitle('Pulse pressure (PP = systole - diastole)') + 
  scale_shape_PP +
  theme(legend.position = 'bottom')

gen_model_plot <- function(beats, color = TRUE) {
  #GAM
  PP_gam <- gam(PP ~ s(resp_rel_index, k = 15, bs = 'cc') + s(time, bs = 'cr'), 
                knots = list(resp_rel_index = c(
                  0, 
                  1)),
                method = 'REML', data = beats)
  
  beats_p <- mutate(beats, PP_predict = predict(PP_gam),
                    PP_res = residuals(PP_gam),
                    PP_trend = predict(PP_gam, type = 'terms', terms = 's(time)'),
                    PP_insp = predict(PP_gam, type = 'terms', terms = 's(resp_rel_index)'),
                    PP_detrend = PP_res + PP_insp)
  
  # Model visualizations
  
  # Get representations of smooths
  insp_smooth <- gratia::smooth_estimates(PP_gam, 's(resp_rel_index)', 
                                          data = tibble(resp_rel_index = seq(0, 1, length.out = 100), 
                                                        time = 999))
  
  intercept <- coef(PP_gam)[1]
  time_smooth <- gratia::smooth_estimates(PP_gam, 's(time)') %>% 
    mutate(est = est + intercept)
  
  
  y_scale_smooths <- scale_y_continuous(limits = c(-3, 3))
  
  geom_ci_ribbon <- geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), fill =alpha('black', 0), 
                                colour = "#333333", linetype = 2, outline.type = 'both')
  
  insp_smooth_plot <- ggplot(insp_smooth, aes(resp_rel_index, est)) +
    geom_ci_ribbon +
    geom_line(colour = "#333333") + 
    geom_point(aes(y = PP_detrend), data = beats_p, color = darkcolor, alpha = 1) +
    #geom_point(aes(x = 1, y = -1.5, shape = 'Inspiration start'), show.legend = FALSE) +
    #geom_point(aes(x = 0, y = -1.5, shape = 'Inspiration start'), show.legend = FALSE) +
    labs(x = 'Time since inspiration start / cycle length',
         y = 'Partial PP [mmHg]') +
    ggtitle('Position in respiratory cycle') +
    scale_x_continuous(labels = scales::percent, expand = expansion(0.05, c(0, 0.05))) +
    scale_shape_PP +
    y_scale_smooths
  
  time_smooth_plot <- ggplot(time_smooth, aes(time, est)) +
    geom_ci_ribbon + 
    geom_line(colour = "#333333") +
    labs(x = 'Time [s]',
         y = 'Partial PP [mmHg]') + 
    ggtitle('Trend over time') +
    scale_y_continuous(limits = c(intercept - 2, intercept + 2))
  
  if (color) {
    insp_smooth_plot <- insp_smooth_plot + 
      geom_point(aes(y = PP_detrend, color = as.factor(resp_n)), 
                 data = filter(beats_p, resp_n < 5), size = 1.6, show.legend = FALSE) 
      #geom_line(aes(y = PP_detrend, color = as.factor(resp_n), group = as.factor(resp_n)), 
      #          data = filter(beats_p, resp_n < 5), size = 1, show.legend = FALSE)
      
    
  }
  
  model_plot <- insp_smooth_plot + time_smooth_plot
  model_plot
  
  
}

model_plot <- gen_model_plot(beats)

combined_gam_PP_plot <- PP_plot + model_plot + plot_layout(nrow = 2, heights = c(2, 4)) + 
  plot_annotation(tag_levels = c('a')) 

save_plot('methods-gam-PP', combined_gam_PP_plot, width = 18, height = 11)
