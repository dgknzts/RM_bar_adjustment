# libraries ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggtext)

# importing and processing ------------------------------------------------
setwd("G:/My Drive/Projects/RM_adjustment/")
data <- read.csv("Data/processed.csv") 

data <- data %>% 
  filter(number_deviation %in% c(-1,0)) %>%
  filter(exp_version != "Exp1C")
# We need a new spacing categorization
# because we are comparing different experiments
# OR we can use spacing without categorizing it.


## by participant and correction for across participants variation  ---------------------------------
data_by_pp <- data %>%
  group_by(exp_version) %>%
  mutate(grand_avg = mean(width_deviation)) %>%
  group_by(subID, exp_version) %>%
  mutate(participant_avg = mean(width_deviation),
         temp_width_deviation = width_deviation - participant_avg,
         adj_width_deviation = temp_width_deviation + grand_avg) %>% #Cousineau, 2005
  group_by(edge_to_edge_spacing, correct_width, number_deviation, subID, exp_version) %>%
  summarise(
    mean_width = mean(adj_width_deviation), 
    n = n()
  )

## across participant ------------------------
data_across_pp <- data_by_pp %>%
  group_by(edge_to_edge_spacing, correct_width, number_deviation, exp_version) %>%
  summarise(
    width_mean = mean(mean_width),
    width_sd = sd(mean_width),
    n = n()
  ) %>%
  mutate(
    width_SEM = width_sd / sqrt(n),
    width_CI = width_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

# plot --------------------------------------------------------------------
plot_width_all <- ggplot(data, aes(x = edge_to_edge_spacing, 
                 y = width_deviation, 
                 color = as.factor(number_deviation))) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  
  geom_point(alpha = 0.05,
             size = 3,
             position = position_jitter(width = 0.05)) +  # Continuous variable requires points instead of position_dodge
  
  geom_smooth(method = "loess", 
              se = TRUE,  # Show confidence interval
              aes(fill = factor(number_deviation, levels = c(-1, 0))),
              alpha = 0.2,
              show.legend = FALSE) +  # Smooth curve with confidence interval
  
  scale_color_manual(
    values = c("-1" = "#A84847", "0" = "#509464"), 
    limits = c("-1", "0")
  ) +
  
  scale_fill_manual(
    values = c("-1" = "#A84847", "0" = "#509464")  # Match fill color for smooth CI
  ) +
  
  facet_grid(
    rows = vars(correct_width),          
    cols = vars(exp_version),            
    switch = "y",                        
    labeller = as_labeller(c(
      "0.1" = "0.1\u00B0 Width",
      "0.25" = "0.25\u00B0 Width",
      "0.4" = "0.4\u00B0 Width",
      "0.55" = "0.55\u00B0 Width",
      "Exp1A" = "Experiment 1A",
      "Exp1B" = "Experiment 1B",
      "Exp1C" = "Experiment 1C"
    ))
  ) +
  
  labs(
    title = "Width Deviation by Spacing + Width and Number Deviation",
    x = "Edge to Edge Spacing",
    y = "Width Deviation",
    fill = "Number\nDeviation"
  ) + 
  
  # geom_vline(xintercept = 0.1) +
  # geom_vline(xintercept = 0.25) +
  # geom_vline(xintercept = 0.4) +
  geom_vline(xintercept = 0.55) +
  
  coord_flip() +
  
  theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(color = "black"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    strip.background = element_rect(fill = "white", color = "white", size = 0.1),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

# Save the plot -----------------------------------------------------------
ggsave("figures/by_spacing_width_deviation_all_exps.svg", plot = plot_width_all, width = 6, height = 5, units = "in", dpi = 300)


