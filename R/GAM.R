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
