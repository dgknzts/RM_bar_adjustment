# libraries ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggstar)
library(ggnewscale)

# importing and processing ------------------------------------------------
data <- read.csv("processed.csv") %>% 
  #filter(exp_version == "Exp1A") %>% 
  #mutate(number_deviation = ifelse(number_deviation < 0, -1, 0)) %>%
  #mutate(response_width = response_width*response_num) %>%
  filter(number_deviation %in% c(-1,0)) %>%
  mutate(expected_width = actual_pooled_width / response_num)

## by participant and correction for across participants variation  ---------------------------------
data_by_pp <- data %>%
  group_by(exp_version) %>%
  mutate(grand_avg = mean(response_width)) %>%
  group_by(subID, exp_version) %>%
  mutate(participant_avg = mean(response_width),
         temp_response_width = response_width - participant_avg,
         adj_response_width = temp_response_width + grand_avg) %>% #Cousineau, 2005
  group_by(correct_num, correct_width, number_deviation, subID, exp_version) %>%
  summarise(
    expected_width = mean(expected_width),
    mean_width = mean(adj_response_width),
    n = n()
  )

## across participant ------------------------
data_across_pp <- data_by_pp %>%
  group_by(correct_num, correct_width, number_deviation, exp_version) %>%
  summarise(
    expected_width = mean(expected_width),
    width_mean = mean(mean_width),
    width_sd = sd(mean_width),
    n = n()
  ) %>%
  mutate(
    width_SEM = width_sd / sqrt(n),
    width_CI = width_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

# plot --------------------------------------------------------------------
plot_response_width_all <- ggplot(data_across_pp, aes(x = correct_num, 
                                             y = width_mean, 
                                             fill = factor(number_deviation, levels = c(-1, 0)))) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  
  geom_bar(stat = "identity", 
           position = position_dodge(-0.6),
           alpha = 0.7,
           color = "black"
  ) +
  
  geom_errorbar(
    aes(
      x = correct_num,
      y = width_mean,
      group = factor(number_deviation, levels = c(-1, 0)),
      color = factor(number_deviation, levels = c(-1, 0)),
      ymin = width_mean - width_CI,
      ymax = width_mean + width_CI
    ),
    size = 1.0,            # Smaller size for inner error bar
    width = 0.3,
    #color = "gray",
    alpha = 0.8,
    position = position_dodge(-0.6),
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = c("-1" = "#A84847", "0" = "#509464"), 
    limits = c("-1", "0")
  ) +
  
  geom_star(data = data_across_pp %>% filter(number_deviation == -1),
    aes(
      y = expected_width,
      x = correct_num - 0.17
    ),
    size = 2.78,
    inherit.aes = FALSE,
    fill = "#A84847",
    color = "black",
    alpha = 0.75
  ) +
  
  geom_star(data = data_across_pp %>% filter(number_deviation == 0),
            aes(
              y = expected_width,
              x = correct_num + 0.17
            ),
            size = 2.78,
            inherit.aes = FALSE,
            fill = "#509464",
            color = "black",
            alpha = 0.75
  ) +

  labs(
    title = "Response Width by Set Size and Number Deviation",
    subtitle = "Stars are the expected widths considering pooling hypothesis",
    x = "Set Size",
    y = "Response Width",
    fill = "Number\nDeviation"
  ) +
  
  scale_x_reverse(breaks = c(3, 4, 5)) +
  
  scale_y_continuous(breaks = c(0.1, 0.25, 0.4, 0.55, 0.7), 
                     labels = c("0.1\u00B0", "0.25\u00B0", "0.4\u00B0", "0.55\u00B0", "0.7\u00B0")
  ) +

  
  scale_fill_manual(
    values = c("-1" = "#A84847", "0" = "#509464"), 
    limits = c("-1", "0")
  ) +
  
  facet_grid(
    rows = vars(correct_width),          # Satır değişkeni
    cols = vars(exp_version),            # Sütun değişkeni
    switch = "y",                        # Etiketleri eksenlere taşır
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
  
  coord_flip() +
  
  theme_classic() +
  
  
  
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.9),
    legend.background = element_rect(color = "black"),
    legend.title.position = "left",
    legend.margin = margin(3,3,3,3),
    panel.spacing.y = unit(0.00, "lines"),
    panel.spacing.x = unit(0.00, "lines"),
    strip.background = element_rect(fill = "white", color = "white", size = 0.1),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 10, hjust = -0.05, margin = margin(b = 0.1)), 
    plot.subtitle = element_text(size = 7, hjust = -0.045, margin = margin(b = 0.1))
  ) +
  
  geom_vline(xintercept = 2.5, linetype = "solid", color = "darkgrey", size = 0.5)

# Save the plot -----------------------------------------------------------
ggsave("figures/response_width_compared_to_expected.svg", plot = plot_response_width_all, width = 9, height = 5, units = "in", dpi = 300)

