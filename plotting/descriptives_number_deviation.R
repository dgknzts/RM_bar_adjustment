# libraries ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggtext)

# importing and processing ------------------------------------------------
setwd("G:/My Drive/Projects/RM_adjustment/")
data <- read.csv("Data/processed.csv") 

# Assuming your data has a `correct_num` variable 
percentage_data <- data %>%
  group_by(exp_version, correct_num, number_deviation) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(exp_version, correct_num) %>%
  mutate(
    total_trials = sum(count),
    percentage = (count / total_trials) * 100
  )

# Plot with facet_wrap by correct_num
ggplot(percentage_data, aes(x = factor(number_deviation), y = percentage, fill = exp_version)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3, alpha = 0.5) + 
  scale_x_discrete(name = "Number Deviation", limits = as.character(-3:3)) +
  scale_fill_manual(values = c("Exp1A" = "#1f78b4", "Exp1B" = "#33a02c", "Exp1C" = "#e31a1c")) +
  labs(
    title = "Descriptives of Number Deviation",
    x = "Number Deviation",
    y = "Percentage (%)",
    fill = "Experiment Version"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  facet_wrap(~ correct_num, ncol = 3,
             labeller = labeller(correct_num = function(x) paste("Set Size:", x)))

