# Before modelling -----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(lme4)
library(emmeans)
library(sjPlot)

model_table <- function(modelx, filename) {
  tab_model(
    modelx,
    p.style = 'scientific_stars',
    show.se = T,
    show.stat = T,
    digits = 3,
    file = filename
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
#data$number_deviation <- as.factor(data$number_deviation)


# Selecting experiment -------------------------------------
data <- data %>% filter(exp_version == "Exp1A")
colnames(data)


#Model 1: Number deviation as the numeric outcome ----------------------------------
model_1.0 <- lmer(number_deviation ~  correct_num * 
                    correct_width + 
                    correct_space + 
                    (1|subID),
                  data = data)

model_table(model_1.0, "model_tables/number_deviation_numeric.html")

pairwise_results <- emmeans(model_1.0, pairwise ~ correct_width)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")

# Model 2: Number deviation as the binomial (RM or NOT) -----------------
data$number_deviation_binom <- as.factor(ifelse(data$number_deviation >= 0, "No_rm", "Rm"))

model_2.0 <- glmer(number_deviation_binom ~  correct_num * 
                    correct_width + 
                    correct_space + 
                    (1|subID),
                  family = "binomial",
                  data = data)

model_table(model_2.0, "model_tables/number_deviation_binomial.html")

pairwise_results <- emmeans(model_1.0, pairwise ~ correct_width)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")



