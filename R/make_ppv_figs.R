source("R/common.R")

sample_pp <- readRDS("sample_data/sample_PP.RDS")

# Figure background-ppv-example

abp_plot <- 
  sample_pp$abp |> 
  filter(time < 20) |> 
  ggplot(aes(time, ABP)) +
  geom_line() +
  labs(y = "ABP [mmHg]") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) 

pp_plot <- 
  sample_pp$beats |> 
  filter(time < 20) |> 
  ggplot(aes(time, PP)) +
  geom_line() +
  geom_point() +
  labs(x = "Time [s]",
       y = "PP [mmHg]") +
  xlim(0,20)


fig_ppv_example <- abp_plot / pp_plot

save_plot("background-ppv-example", fig_ppv_example)
