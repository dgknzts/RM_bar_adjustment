# Before modelling -----------------------------------------------------------
  
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(lme4)
library(emmeans)

model_table <- function(modelx) {
  sjPlot::tab_model(
    modelx,
    p.style = 'scientific_stars',
    show.se = T,
    show.stat = T,
    digits = 3
  )
}


setwd("G:/My Drive/Projects/RM_adjustment/")
data <- read.csv("processed.csv")


data$subID <- as.factor(data$subID)
data$correct_num <- as.factor(data$correct_num)
data$correct_width <- as.factor(data$correct_width)
data$response_num <- as.factor(data$response_num)
data$exp_version <- as.factor(data$exp_version)
data$keyboard_condition <- as.factor(data$keyboard_condition)
data$trial_type <- as.factor(data$trial_type)
data$number_deviation <- as.factor(data$number_deviation)


#Selecting experiment 1A
data <- data %>% filter(exp_version == "Exp1A")
colnames(data)

# Set reference num dev 0
data$number_deviation <- relevel(data$number_deviation, ref = "0")
levels(data$number_deviation)



correlation_matrix <- cor(data %>% select(width_deviation, 
                                          correct_space,
                                          spacing_deviation, 
                                          compression_rate), use = "complete.obs")
round(correlation_matrix, 2)

# Modelling -----------------------------------------------------------

## Width Deviation as the outcome variable -----------------------


model_1.0 <- lmer(width_deviation ~ number_deviation + 
                    correct_num + 
                    correct_width + 
                    correct_space + 
                    spacing_deviation + 
                    compression_rate +
                    (1|subID), 
                  data = data)

model_table(model_1.0)

model_1.1 <- lmer(width_deviation ~ number_deviation + 
                    correct_num + 
                    correct_width + 
                    #correct_space + 
                    spacing_deviation + 
                    compression_rate +
                    (1|subID), 
                  data = data)

anova(model_1.0, model_1.1)


# Spacing deviation and compression rate is correlated so selecting one of them


model_1.20 <- lmer(width_deviation ~ number_deviation + 
                     correct_num + 
                     correct_width + 
                     #correct_space + 
                     #spacing_deviation + 
                     compression_rate +
                     (1|subID), 
                   data = data)

model_1.21 <- lmer(width_deviation ~ number_deviation + 
                     correct_num + 
                     correct_width + 
                     #correct_space + 
                     spacing_deviation + 
                     #compression_rate +
                     (1|subID), 
                   data = data)

anova(model_1.1, model_1.20)
anova(model_1.1, model_1.21)


pairwise_results <- emmeans(model_1.1, pairwise ~ number_deviation | correct_width)


## Filtering only -1 and 0 number deviations -------------------------------


data_minus_1_n_0 <- data %>% filter(number_deviation %in% c(-1,0))
# 684 rows gone

model_2.1 <- lmer(width_deviation ~ number_deviation* 
                    correct_num* 
                    correct_width + 
                    compression_rate +
                    (1 | subID), 
                  data = data_minus_1_n_0)

model_table(model_2.1)

pairwise_results <- emmeans(model_2.1, pairwise ~ number_deviation | correct_width + correct_num)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")

