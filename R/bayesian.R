source("R/common.R")

mean_height <- 165
sd_height <- 10

grid_mu <- seq(140, 210, length.out = 200)
grid_sigma <- seq(0, 35, length.out = 200)

prior <- expand.grid( mu=grid_mu , sigma=grid_sigma ) |> 
  mutate(L_prior = dnorm(mu , 175 , 20 , log = TRUE ) +
                   dunif( sigma , 1 , 30 , TRUE ),
         prior = exp(L_prior))

set.seed(15)
heights <- tibble(
  id = 1:100,
  height = rnorm(100, mean_height, sd_height)
)

get_post <- function(d) {
  post <- prior
  if (nrow(d) < 1) {
    post$prod <- post$L_prior
  } else {
    post$LL <- sapply( 1:nrow(post) , function(i) sum(
      dnorm( d$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
    post$prod <- post$LL + post$L_prior
  }
  post$prob <- exp(post$prod) / sum(exp(post$prod))
  post
}

make_panel <- function(n, y_max_mu = NA, y_max_sigma = NA, hide_y = FALSE) {
  d <- heights |> filter(id <= n)
  
  post_n <- get_post(d)
  
  post_mu <- post_n |> 
    group_by(mu) |> 
    summarise(prob = sum(prob)) |> 
    mutate(dens = prob * n() / (max(mu) - min(mu)),
           ci_low = first(mu[cumsum(prob) > 0.025]),
           ci_high = last(mu[cumsum(prob) < 0.975])
           )
  
  y_theme <- if(hide_y) {
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = margin(t = 0, r = 0, l = 20, b = 10))
  } else{
    theme(plot.margin = margin(t = 0, r = 0, l = 0, b = 10))
  }
  
  post_sigma <- post_n |> 
    group_by(sigma) |> 
    summarise(prob = sum(prob)) |> 
    mutate(dens = prob * n() / (max(sigma) - min(sigma)),
           ci_low = first(sigma[cumsum(prob) > 0.025]),
           ci_high = last(sigma[cumsum(prob) < 0.975]))
  
  densline <- ifelse(n == 0, 2, 1)
  
  plot_mu <- ggplot(post_mu, aes(mu, dens)) +
    geom_vline(xintercept = mean_height, 
               color = darkred, size = 0.7) +
    geom_line(linetype = densline) +
    geom_area(data = ~filter(.x, mu > ci_low, mu < ci_high),
              fill = darkred, alpha = 0.3) +
    geom_rug(aes(x = height), inherit.aes = FALSE, data = d,
             length = unit(0.1, "npc")) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.05)), 
                       breaks = c(0, y_max_mu)
                       ) +
    coord_cartesian(clip="off", ylim = c(0, y_max_mu)) +
    labs(x = "μ [cm]", y = "Density",
         subtitle = ifelse(n==0, 
                        "n = 0 (prior)", 
                        glue::glue("n = {n}")
         )) +
    y_theme
  
  plot_sigma <- ggplot(post_sigma, aes(sigma, dens)) +
    geom_vline(xintercept = sd_height,
               color = darkblue, size = 0.7) +
    geom_line(linetype = densline, color = darkblue) +
    geom_area(data = ~filter(.x, sigma > ci_low, sigma < ci_high),
              fill = darkblue, alpha = 0.3) +
    scale_y_continuous(breaks = c(0, y_max_sigma)) +
    coord_cartesian(clip="off", ylim = c(0, y_max_sigma)) +
    labs(x = "σ [cm]", y = "Density") +
    y_theme
  
  plot_mu / plot_sigma 
}


make_panel(1)

bayes_update_plot <- 
  (make_panel(0, y_max_mu = 0.05, y_max_sigma = 0.05) |
  make_panel(1, y_max_mu = 0.05, y_max_sigma = 0.05, hide_y = TRUE) |
  make_panel(2, y_max_mu = 0.05, y_max_sigma = 0.05, hide_y = TRUE)) /
  plot_spacer()/
  (make_panel(8, y_max_mu = 0.25, y_max_sigma = 0.4) |
  make_panel(20, y_max_mu = 0.25, y_max_sigma = 0.4, hide_y = TRUE)|
  make_panel(100, y_max_mu = 0.25, y_max_sigma = 0.4, hide_y = TRUE)) +
  plot_layout(heights = c(1, 0.1,1)) 


save_plot("method-bayes-update", bayes_update_plot,
          width = 18, height = 10,
          scale = 1.2)

make_2d_panel <- function(n) {
  d <- heights |> filter(id <= n)
  
  post_n <- get_post(d)
  
  ggplot(post_n, aes(mu, sigma, alpha = prob)) +
    geom_raster(fill = darkred, show.legend = FALSE) +
    #geom_contour(aes(z=prob), color = darkcolor, show.legend = FALSE) +
    scale_alpha_continuous(range = c(0, 1)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(limits = c(0, NA), 
                       expand = expansion(mult = c(0.2, 0.05)), 
                       breaks = scales::breaks_pretty(n=3)
    ) +
    geom_point(x = mean_height, 
               y = sd_height,
               color = darkblue, inherit.aes = FALSE) +
    labs(x = "μ [cm]", y = "σ [cm]",
         subtitle = ifelse(n==0, 
                           "n = 0 (prior)", 
                           glue::glue("n = {n}")
         ))

}

make_2d_panel(8)

bayes_update_2d_plot <- 
  (make_2d_panel(0) |
     make_2d_panel(2) |
     make_2d_panel(20))

save_plot("method-bayes-update-2d", bayes_update_2d_plot,
          width = 18, height = 6,
          scale = 1)



