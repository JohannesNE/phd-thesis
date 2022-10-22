source("R/common.R")

update_geom_defaults("point",   list(colour = darkcolor))
update_geom_defaults("line",   list(colour = darkcolor))

theme_set(theme_thesis() %+replace% 
            theme(plot.title = element_text(size = rel(1), hjust = 0, margin = margin(b = 5)),
                       plot.subtitle = element_text(size = rel(0.8), hjust = 0)))

library(waveformtools)

cvp_viz_sample <- readRDS("sample_data/sample_cvp_insp_qrs_6min.RDS")

# P-Q interval 150 ms

cvp <- cvp_viz_sample$cvp %>% 
  add_time_since_event(cvp_viz_sample$qrs$time-0.150, prefix = 'p') %>% 
  add_time_since_event(cvp_viz_sample$insp_start$time, prefix = 'insp') %>%
  mutate(window = factor(case_when(time < cvp_viz_sample$fluid_start ~ 'pre fluid',
                                   time < cvp_viz_sample$fluid_end   ~ 'during fluid',
                                   TRUE               ~ 'post fluid'))) %>% 
  drop_na(everything()) %>% 
  mutate(time_s = time - time[1])

# Demonstrate using time where CVP increases slightly
cvp_demo_60 <- filter(cvp, time_s < 60)

insp_demo_60 <- filter(cvp_viz_sample$insp_start, 
                       time < 60) %>% 
  mutate(time_s = time - first(cvp_demo_60$time))

qrs_demo_60 <- filter(cvp_viz_sample$qrs, 
                      between(time, 
                              first(cvp_demo_60$time), 
                              last(cvp_demo_60$time))) %>% 
  mutate(time_s = time - first(cvp_demo_60$time))

# p wave start
p_demo_60 <- tibble(time_s = qrs_demo_60$time_s - 0.150, label = "P wave")

mustashe::stash("gam_cvp_demo_60", depends_on = "cvp_demo_60", {
  bam(
    CVP ~ 
      s(p_index, bs = 'cr', k = 40) +
      s(insp_rel_index, bs = 'cc', k = 30) +
      ti(
        p_index,
        insp_rel_index,
        bs = c('cr', 'cc'),
        k = c(30, 30)
      ) +
      s(time_s, bs = "cr", sp = 10),
    knots = list(insp_rel_index = c(0, 1)),
    method = 'fREML',
    rho = 0.95,
    gamma = 5,
    data = cvp_demo_60
  )
  
})

cvp_demo_60_p <- mutate(cvp_demo_60, CVP_predict = predict(gam_cvp_demo_60),
                        CVP_res = residuals(gam_cvp_demo_60))

# Design Plot
first_20_s <- . %>% filter(time_s < 20)

cvp_plot <- ggplot(cvp_demo_60 %>% first_20_s(), aes(time_s, CVP, group = 1)) +
  geom_line(show.legend = FALSE) +
  geom_point(aes(time_s, y = 6, shape = 'Inspiration start'), 
             data = insp_demo_60 %>% first_20_s()) +
  geom_point(aes(time_s, y = 18, shape = 'P wave (ECG)'), 
             data = p_demo_60 %>% first_20_s()) +
  scale_colour_discrete() +
  labs(title = "Observed CVP [mmHg]",
       x = 'Time [s]',
       y = ' CVP') +
  theme(legend.position = c(0.8, 1.3), legend.direction = "horizontal")

# Get representations of smooths
cardiac_smooth <- gratia::smooth_estimates(gam_cvp_demo_60, 's(p_index)')
insp_smooth <- gratia::smooth_estimates(gam_cvp_demo_60, 's(insp_rel_index)')

intercept <- coef(gam_cvp_demo_60)[1]
time_smooth <- gratia::smooth_estimates(gam_cvp_demo_60, 's(time_s)') %>% 
  mutate(est = est + intercept) # Add intercept

interact_smooth <- gratia::smooth_estimates(gam_cvp_demo_60, 'ti(p_index,insp_rel_index)', dist=0.3)

y_scale_smooths <- scale_y_continuous(limits = c(-4, 4), expand = c(0.05,0))


cvp_landmarks <- tribble(
  ~p_index, ~est, ~label,
  0.13,     4.3, "a",
  0.30,     0.5, "c",
  0.38,  -2.5, "x'",
  0.60,   1, "v",
  0.75,  -0.5, "y",
)

cardiac_smooth_plot <- ggplot(cardiac_smooth, aes(p_index, est)) +
  geom_line() +
  # Label CVP landmarks
  # geom_text(aes(label = label), 
  #           color = "black",
  #           size = 3,
  #           nudge_y = 0.4,
  #           data = cvp_landmarks, show.legend = FALSE) +  
  geom_point(aes(x = 0, y = 3.5, shape = 'P wave (ECG)'), data = data.frame(), show.legend = FALSE) +
  labs(x = 'Time since P wave [s]',
       y = 'Partial CVP') +
  ggtitle('Position in cardiac cycle') +
  #xlim(c(0,1.1)) +
  y_scale_smooths +
  coord_cartesian(clip = "off")

insp_smooth_plot <- ggplot(insp_smooth, aes(insp_rel_index, est)) +
  geom_line() +
  geom_point(aes(x = 1, y = -1, shape = 'Inspiration start'), data = data.frame(), show.legend = FALSE) +
  geom_point(aes(x = 0, y = -1, shape = 'Inspiration start'), data = data.frame(), show.legend = FALSE) +
  labs(x = 'Respiratory cycle',
       y = 'Partial CVP') +
  ggtitle('Position in respiratory cycle') +
  scale_x_continuous(labels = scales::percent) +
  y_scale_smooths

interact_smooth_plot <- ggplot(interact_smooth, aes(p_index, insp_rel_index, z = est)) +
  #geom_contour_filled(show.legend = FALSE, binwidth = 0.2) +
  geom_raster(aes(fill = est), show.legend = FALSE) +
  geom_contour(colour = 'black', binwidth = 0.2) +
  metR::geom_label_contour(skip = 1, size = 2.5, label.size = 0,
                           #label.placement = metR::label_placement_fraction(frac = 0.1),
                           label.padding = unit(0.01, "lines"), binwidth = 0.4) +
  #metR::geom_text_contour(skip = 1, rotate = FALSE, size = 3) +
  
  labs(x = 'Time since P wave [s]',
       y = 'Respiratory cycle',
       subtitle = 'Contour height = partial CVP') +
  ggtitle('Interaction between cycles') +
  scale_fill_distiller(palette = "RdBu", 
                       type = "div") +
  scale_y_continuous(labels = scales::percent) 


time_smooth_plot <- ggplot(time_smooth, aes(time_s, est)) +
  geom_line() +
  labs(x = 'Time [s]',
       y = 'Partial CVP') + 
  ggtitle('Trend over time') +
  scale_y_continuous(limits = c(intercept - 2, intercept + 2))


shape_scale <- scale_shape_manual(values = c(3, 17), 
                                  limits = c('P wave (ECG)', 
                                             'Inspiration start'),
                                  name = '')


model_cvp_plot <- (cardiac_smooth_plot + insp_smooth_plot) / 
  (interact_smooth_plot + time_smooth_plot) +
  plot_layout(heights = c(1,1.5)) & 
  shape_scale
# model_cvp_plot

predict_cvp_plot <- ggplot(cvp_demo_60_p %>% first_20_s(), aes(time_s, CVP, colour = 'Observed')) +
  geom_line() +
  geom_line(aes(y = CVP_predict, colour = 'Predicted'), linetype = "31") +
  labs(title = 'Residuals (observed CVP - predicted CVP)',
       y='CVP', x = 'Time [s]', colour = '') +
  scale_colour_manual(values = c(darkblue, darkred, orange)) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 2, 1)) ) ) +
  
  scale_y_continuous(limits = c(7, 17), breaks = seq(7, 17, by = 2)) +
  theme(#axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.line.x = element_blank(),
    legend.position = 'bottom')
# predict_cvp_plot

predict_resid_cvp_plot_compact <- predict_cvp_plot + 
  geom_line(aes(y = CVP_res, color = "Residual")) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 2, 1)) ) ) +
  ylim(c(-2,17)) +
  theme(legend.position = c(0.78, 1.15), legend.direction = "horizontal")

combined_gam_cvp_plot_simple <- cvp_plot + model_cvp_plot + predict_resid_cvp_plot_compact + 
  plot_annotation(tag_levels = "a") +
  plot_layout(ncol = 1, heights = c(1, 5, 1.5)) & shape_scale

save_plot("method-gam-CVP", combined_gam_cvp_plot_simple, width = 18, height = 18)


## Before - After fluid fig =======

cvp_pre_post <- readRDS("sample_data/cvp_pre_post.RDS")
gam_cvp_pre_post_ad <- readRDS("sample_data/fitted_gam_cvp_pre_post_ad.RDS")

scale_linetype_ventstate <- scale_linetype_manual(values = c("52","21"),
                                                  breaks = c("End expiration",
                                                             "End inspiration")) 

scale_color_win <- scale_color_manual(values = c(darkred,
                                                 darkblue),
                                      breaks = c("Before 250 mL fluid",
                                                 "After 250 mL fluid"))

# Compare individual models
new_data_cardiac <- expand_grid(window_f = factor(c("pre fluid", "post fluid")),
                                time_s = 0,
                                insp_rel_index = c(0,1/3),
                                p_index = seq(0, 60/90, length.out = 200)) %>% 
  mutate(vent_state = ifelse(insp_rel_index == 0, 
                             "End expiration",
                             "End inspiration"),
         win = ifelse(window_f == "pre fluid", 
                      "Before 250 mL fluid", "After 250 mL fluid") %>% fct_inorder() )

new_data_insp <- expand_grid(window_f = factor(c("pre fluid", "post fluid")),
                             time_s = 0,
                             insp_rel_index = seq(0, 1, by = 0.005),
                             p_index = 0) %>% 
  mutate(win = ifelse(window_f == "pre fluid", 
                      "Before 250 mL fluid", "After 250 mL fluid") %>% fct_inorder())

new_data_interaction <- expand_grid(window_f = factor(c("pre fluid", "post fluid")),
                                    time_s = 0,
                                    p_index = seq(0, 60/90, length.out = 200),
                                    insp_rel_index = seq(0, 1, by = 0.005)) %>% 
  mutate(win = ifelse(window_f == "pre fluid", 
                      "Before 250 mL fluid", "After 250 mL fluid") %>% fct_inorder())


# Generate predictions and bind to newdata
get_smooth <-  function(new_data, model, ...) {
  bind_cols(
    new_data,
    predict(
      model,
      newdata = new_data,
      se.fit = TRUE,
      ...
    )
  )
} 

terms_by_window <- function(smooth) {
  paste0(smooth, c(":window_fpre fluid", ":window_fpost fluid"))
}

cardiac_smooth_comb <- get_smooth(new_data_cardiac, gam_cvp_pre_post_ad, exclude = terms_by_window("s(time_s)"))

cvp_landmarks <- tribble(
  ~p_index, ~label,
  0.12,   "a",
  0.22,   "c",
  0.27,   "x'",
  0.45,   "v",
  0.53,   "y",
) 

lab_cvp <- ylab("CVP [mmHg]")

cardiac_smooth_landmark <- waveformtools::join_nearest(cardiac_smooth_comb %>% filter(vent_state == "End inspiration",
                                                                       win == "After 250 mL fluid"),
                                        cvp_landmarks,
                                        xkey = "p_index",
                                        ykey = "p_index") %>% 
  group_by(label) %>% 
  # Only keep label at minimum distance
  mutate(label = ifelse(abs(p_index-p_index.y) == min(abs(p_index-p_index.y)),
                        label,
                        ""))


plot_cardiac <- cardiac_smooth_comb %>%
  ggplot(aes(p_index, fit, group = vent_state)) + 
  geom_line(aes(color = win), show.legend = FALSE) + 
  facet_wrap(~win, nrow = 1) +
  scale_linetype_ventstate +
  scale_color_win + 
  directlabels::geom_dl(aes(label = vent_state),
                        position = position_nudge(x = 0.02),
                        color = "black",
                        method = list("lines2", cex = 0.7), hjust = 0,
                        data = ~filter(.x, p_index > 0.35, p_index < 0.7, 
                                       win == "Before 250 mL fluid")) +
  # Label CVP landmarks
  geom_text(aes(label = label), 
            color = "black",
            size = 3,
            position = ggpp::position_nudge_line(x = 0.02, y = 1),
            data = cardiac_smooth_landmark, show.legend = FALSE) +
  coord_cartesian(clip="off")+
  
  labs(y = "CVP [mmHg]", x = "Time since P wave [seconds]")


cvp_aug <- mutate(cvp_pre_post, res = residuals(gam_cvp_pre_post_ad),
                  fit_vent = predict(gam_cvp_pre_post_ad, 
                                     terms = terms_by_window("s(insp_rel_index)")),
                  fit_cardiac = predict(gam_cvp_pre_post_ad, 
                                        terms = terms_by_window("s(p_index)")),
                  fit_time = predict(gam_cvp_pre_post_ad,
                                     terms = terms_by_window("s(time_s)")),
                  partial_res_vent = res + fit_vent,
                  partial_res_cardiac = res + fit_cardiac,
                  partial_res_time = res + fit_time,
                  win = ifelse(window_f == "pre fluid", 
                               "Before 250 mL fluid", "After 250 mL fluid") %>% fct_inorder()
)



plot_vent_resid <- cvp_aug %>% 
  ggplot(aes(insp_rel_index, fit_vent, group = win, color = win)) + 
  geom_vline(xintercept = 0, linetype = 2, color = "#444444") + 
  geom_vline(xintercept = 1/3, linetype = 2, color = "#444444") + 
  geom_point(aes(y = partial_res_vent), size = 0.1, alpha = 0.2, show.legend = FALSE) +
  geom_line(show.legend = FALSE, color = "#444444") +
  scale_color_win +
  scale_x_continuous(labels = scales::percent) + 
  lab_cvp +
  xlab("Position in respiratory cycle")


plot_fluid_comb <- plot_cardiac + plot_vent_resid + 
  plot_layout(nrow = 1, widths = c(2, 1)) + plot_annotation(tag_levels = "a")

save_plot("results-GAM-CVP-fluid", plot_fluid_comb, width = 18, height = 7)

# raw plot

cvp_sample_comb <- cvp_pre_post %>% 
  group_by(window_f) %>% 
  filter(insp_n == first(insp_n) + 1) %>% 
  mutate(win = ifelse(window_f == "pre fluid", 
                      "Before 250 mL fluid", "After 250 mL fluid") %>% fct_inorder(),
         time_s = time_s - first(time_s))

plot_cvp <- cvp_sample_comb %>% 
  ggplot(aes(time_s, CVP, group = win, color = win)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~win, nrow = 1) +
  scale_color_win +
  lab_cvp +
  xlab("Time [seconds]")

save_plot("results-raw-CVP-fluid", plot_cvp, width = 14, height = 6)

