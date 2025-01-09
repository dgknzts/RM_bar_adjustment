# Before modelling -----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(lme4)
library(emmeans)
library(sjPlot)

model_table <- function(modelx,filename) {
  tab_model(
    modelx,
    p.style = 'scientific_stars',
    show.se = T,
    show.stat = T,
    digits = 3,
    file = filename
  )
}


setwd("G:/My Drive/Projects/RM_adjustment")
data <- read.csv("Data/processed.csv") %>%
  filter(number_deviation %in% c(-1,0)) %>%
  mutate(number_deviation = ifelse(number_deviation == -1, "1", "0")) %>%
  mutate(number_deviation = as.factor(number_deviation),
         width_spacing_ratio = correct_width / correct_space,
         width_edge_spacing_ratio = correct_width / actual_edge_to_edge_spacing)


# Re-coding Factors ---------------------------------------------------------
data$subID <- as.factor(data$subID)
data$correct_num <- factor(data$correct_num, levels = c("3","4","5"))
data$correct_width <- as.factor(data$correct_width)
data$response_num <- as.factor(data$response_num)
data$exp_version <- as.factor(data$exp_version)
data$keyboard_condition <- as.factor(data$keyboard_condition)
data$trial_type <- as.factor(data$trial_type)
data$spacing <- factor(data$spacing, levels = c("small","middle","large"))


# Experiment 1A -----------------------------------------------------
df_1A <- data %>% filter(exp_version == "Exp1A")

# Model 1: Number deviation is binomial (-1 or 0) (coded as 1 and 0) -----------------

# Full model with all the interactions--------------------
model_1A <- glmer(number_deviation ~  correct_num * 
                    correct_width * 
                    spacing + 
                    (1|subID),
                    family = "binomial",
                    data = df_1A)

model_table(model_1A, NULL)

# Spacing as a continuous variable ---------------------------
model_1.1A <- glmer(number_deviation ~correct_num * 
                    correct_width *
                    actual_edge_to_edge_spacing + 
                    (1|subID),
                    family = "binomial",
                    data = df_1A)

anova(model_1A, model_1.1A) #Continous spacing is better.

# No/less interactions ---------------------------
model_1.2A <- glmer(number_deviation ~correct_num * 
                    correct_width +
                    actual_edge_to_edge_spacing + 
                    (1|subID),
                    family = "binomial",
                    data = df_1A)

anova(model_1.1A, model_1.2A) # 1.1A is the best one
# Full model is the best one...
best_model_1A <- model_1.1A
model_table(best_model_1A, NULL)

# Experiment 1B ---------------------------------------------
df_1B <- data %>% filter(exp_version == "Exp1B")

# Model 1: Number deviation is binary  -----------------
model_1B <- glmer(number_deviation ~  correct_num * 
                     correct_width * 
                     spacing + 
                     (1|subID),
                   family = "binomial",
                   data = df_1B)

model_table(model_1B, NULL)

# Model 1.1. Trying to reduce the model: No/less interactions
model_1.1B <- glmer(number_deviation ~  correct_num *
                    correct_width +
                    spacing + 
                    (1|subID),
                  family = "binomial",
                  data = df_1B)

anova(model_1B, model_1.1B)
# Full model is the best one
best_model_1B <- model_1B

# Experiment 1C ------------------------------------------------------------
df_1C <- data %>% filter(exp_version == "Exp1C")

# Model 1: Binary number deviation -----------------
# In this experiment spacing did not vary between different widths
model_1C <- glmer(number_deviation ~  correct_num * 
                     correct_width *
                     spacing +
                     (1|subID),
                   family = "binomial",
                   data = df_1C)

model_table(model_1C, NULL)

# No/less interactions ---------------------------
model_1.1C <- glmer(number_deviation ~  correct_num + 
                     correct_width + 
                     spacing +
                     (1|subID),
                   family = "binomial",
                   data = df_1C)

anova(model_1C, model_1.1C) #p = 0.04688
# Full model is the best one
best_model_1C <- model_1C

# Creating the estimated data:
df_1A$predictions <- predict(best_model_1A, type = "response")  # Predictions for Exp 1A
df_1B$predictions <- predict(best_model_1B, type = "response")  # Predictions for Exp 1B
df_1C$predictions <- predict(best_model_1C, type = "response")  # Predictions for Exp 1C

# Combine all experiments into one dataframe
df_1ABC <- rbind(df_1A, df_1B, df_1C)

# Comparing emmeans for each experiment -----------------------------------------
contrast_formula = pairwise ~ correct_width | correct_num

# Experiment 1A ---------------------------------------------------------
pairwise_results_1A <- emmeans(best_model_1A, contrast_formula)
summary(pairwise_results_1A$contrasts, infer = TRUE, adjust = "Tukey")

# Experiment 1B ---------------------------------------------------------
pairwise_results_1B <- emmeans(best_model_1B, contrast_formula)
summary(pairwise_results_1B$contrasts, infer = TRUE, adjust = "Tukey")

# Experiment 1C ---------------------------------------------------------
pairwise_results_1C <- emmeans(best_model_1C, contrast_formula)
summary(pairwise_results_1C$contrasts, infer = TRUE, adjust = "Tukey")

# Calculate the estimated marginal means (emmean) with 95% confidence intervals
est_data_ABC <- df_1ABC %>%
  group_by(exp_version, correct_num, correct_width, spacing) %>%
  summarize(
    emmean = mean(predictions),  # Mean of predictions for the group
    se = sd(predictions) / sqrt(n()),  # Standard error of the mean
    asymp.LCL = mean(predictions) - 1.96 * se,  # Lower 95% confidence limit
    asymp.UCL = mean(predictions) + 1.96 * se   # Upper 95% confidence limit
  )

# Plotting the results using ggplot2
ggplot(est_data_ABC, aes(x = correct_num, y = emmean, color = spacing)) +
  geom_point(size = 2,
             alpha = 0.4) +  # Plot individual points for the predictions
  
  geom_line(aes(group = spacing),
            alpha = 0.4) +  # Add a line connecting points for each spacing group
  
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.01) +  # Add error bars with 95% CIs
  
  facet_grid(exp_version ~ correct_width, 
             labeller = labeller(
               exp_version = c("Exp1A" = "Experiment 1A", 
                               "Exp1B" = "Experiment 1B",
                               "Exp1C" = "Experiment 1C"),  # Added label for Exp1C
               correct_width = c("0.1" = "0.1\u00B0 Width", 
                                 "0.25" = "0.25\u00B0 Width", 
                                 "0.4" = "0.4\u00B0 Width", 
                                 "0.55" = "0.55\u00B0 Width")  # Custom labels for correct_width
             )
  ) + 
  
  scale_y_continuous(limits = c(0, 1.1),
                     breaks = seq(0, 1, 0.25)) +

  scale_color_manual(values = c("#9B4949", "#486170", "#707048")) +
  
  labs(
    title = "GLMM Predictions of Probability of Redundancy Masking",  # Clearer title
    subtitle = "Estimated probability of reporting one less item for different conditions\nwith 95% Confidence Intervals",  # More descriptive subtitle
    x = "Set Size",  # Updated x-axis label for correct_num
    y = "Predicted Probability of Redundancy Masking",  # Updated y-axis label
    color = "Spacing" 
  ) +
  
  guides(color = guide_legend(override.aes = list(linetype = NA, size = 3))) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "inside",
    legend.position.inside = c(0.87, 0.85),
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.title.position = "top",
    legend.margin = margin(3,3,3,3),
    panel.spacing.y = unit(0.00, "lines"),
    panel.spacing.x = unit(0.00, "lines"),
    strip.background = element_rect(fill = "white", color = "white", size = 0.1),
    strip.text = element_text(face = "bold")
  ) +
  
  geom_hline(yintercept = 1.1, linetype = "solid", color = "darkgrey", size = 0.5)

# Save as svg
ggsave("Figures/predicted_prob_of_number_deviation_outcome.svg", width = 6, height = 6, units = "in", dpi = 300)
