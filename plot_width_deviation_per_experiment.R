# libraries ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggtext)

# importing and processing ------------------------------------------------
data <- read.csv("processed.csv") %>% 
  filter(number_deviation %in% c(-1, 0))

# Define versions to loop through -----------------------------------------
versions <- c("Exp1A", "Exp1B", "Exp1C")

for (version in versions) {
  # Filter data for the specific version
  data_filtered <- data %>% filter(exp_version == version)
  
  ## by participant and correction for across participants variation  ---------------------------------
  data_by_pp <- data_filtered %>%
    mutate(grand_avg = mean(width_deviation)) %>%
    group_by(subID) %>%
    mutate(participant_avg = mean(width_deviation),
           temp_width_deviation = width_deviation - participant_avg,
           adj_width_deviation = temp_width_deviation + grand_avg) %>% # Cousineau, 2005
    group_by(correct_num, correct_width, number_deviation, subID) %>%
    summarise(
      mean_width = mean(adj_width_deviation), 
      n = n()
    )
  
  ## across participant ------------------------
  data_across_pp <- data_by_pp %>%
    group_by(correct_num, correct_width, number_deviation) %>%
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
  plot <- ggplot(data_across_pp, aes(x = correct_num, 
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
      alpha = 0.8,
      position = position_dodge(-0.6),
      show.legend = FALSE
    ) +
    
    labs(
      title = paste("Width Deviation by Set Size and Number Deviation (", version, ")", sep = ""),
      x = "Set Size",
      y = "Width Deviation",
      fill = "Number\nDeviation"
    ) +
    
    scale_y_continuous(
      limits = c(-0.135, 0.135),     # Eksenin limitlerini ayarla
      breaks = c(-0.1, -0.05, 0, 0.05, 0.1) # Eksen aralıklarını belirt
    ) +
    
    scale_x_reverse(breaks = c(3, 4, 5)) +
    
    scale_fill_manual(
      values = c("-1" = "#A84847", "0" = "#509464"), 
      limits = c("-1", "0")
    ) +
    scale_color_manual(
      values = c("-1" = "#A84847", "0" = "#509464"), 
      limits = c("-1", "0")
    ) +
    
    facet_grid(
      rows = vars(correct_width),         
      switch = "y",                       
      labeller = as_labeller(c(
        "0.1" = "0.1° Width",
        "0.25" = "0.25° Width",
        "0.4" = "0.4° Width",
        "0.55" = "0.55° Width"
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
      strip.text = element_text(face = "bold")
    ) +
    
    geom_vline(xintercept = 2.5, linetype = "solid", color = "darkgrey", size = 0.5)
  
  # Save the plot -----------------------------------------------------------
  output_filename <- paste("figures/width_deviation_", version, ".svg", sep = "")
  ggsave(output_filename, plot = plot, width = 6, height = 5, units = "in", dpi = 300)
}
