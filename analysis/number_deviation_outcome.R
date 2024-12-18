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


setwd("G:/My Drive/Projects/RM_adjustment/")
data <- read.csv("processed.csv")


data$subID <- as.factor(data$subID)
data$correct_num <- as.factor(data$correct_num)
data$correct_width <- as.factor(data$correct_width)
data$response_num <- as.factor(data$response_num)
data$exp_version <- as.factor(data$exp_version)
data$keyboard_condition <- as.factor(data$keyboard_condition)
data$trial_type <- as.factor(data$trial_type)


# Experiment 1A -------------------------------------
df <- data %>% filter(exp_version == "Exp1A")

#Model 1: Number deviation as the numeric outcome ----------------------------------
model_1.A <- lmer(number_deviation ~  correct_num * 
                    correct_width + 
                    correct_space + 
                    (1|subID),
                  data = df)

model_table(model_1.A, "model_tables/number_deviation_numeric_A.html")

pairwise_results <- emmeans(model_1.A, pairwise ~ correct_width)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")

# Model 2: Number deviation as the binomial (RM or NOT) -----------------
df$number_deviation_binom <- as.factor(ifelse(df$number_deviation >= 0, "No_rm", "Rm"))

model_2.A <- glmer(number_deviation_binom ~  correct_num * 
                    correct_width + 
                    correct_space + 
                    (1|subID),
                  family = "binomial",
                  data = df)

model_table(model_2.A, "model_tables/number_deviation_binomial_A.html")

pairwise_results <- emmeans(model_2.A, pairwise ~ correct_width)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")


# Experiment 1B ---------------------------------------------
df <- data %>% filter(exp_version == "Exp1B")

#Model 1: Number deviation as the numeric outcome ----------------------------------
model_1.B <- lmer(number_deviation ~  correct_num * 
                    correct_width + 
                    correct_space + 
                    (1|subID),
                  data = df)

model_table(model_1.B, "model_tables/number_deviation_numeric_B.html")

pairwise_results <- emmeans(model_1.B, pairwise ~ correct_width)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")

# Model 2: Number deviation as the binomial (RM or NOT) -----------------
df$number_deviation_binom <- as.factor(ifelse(df$number_deviation >= 0, "No_rm", "Rm"))

model_2.B <- glmer(number_deviation_binom ~  correct_num * 
                     correct_width + 
                     correct_space + 
                     (1|subID),
                   family = "binomial",
                   data = df)

model_table(model_2.B, "model_tables/number_deviation_binomial_B.html")

pairwise_results <- emmeans(model_2.B, pairwise ~ correct_width)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")

# Experiment 1C ------------------------------------------------------------

df <- data %>% filter(exp_version == "Exp1B")

#Model 1: Number deviation as the numeric outcome ----------------------------------
model_1.C <- lmer(number_deviation ~  correct_num * 
                    correct_width + 
                    correct_space + 
                    (1|subID),
                  data = df)

model_table(model_1.C, "model_tables/number_deviation_numeric_C.html")

pairwise_results <- emmeans(model_1.C, pairwise ~ correct_width)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")

# Model 2: Number deviation as the binomial (RM or NOT) -----------------
df$number_deviation_binom <- as.factor(ifelse(df$number_deviation >= 0, "No_rm", "Rm"))

model_2.C <- glmer(number_deviation_binom ~  correct_num * 
                     correct_width + 
                     correct_space + 
                     (1|subID),
                   family = "binomial",
                   data = df)

model_table(model_2.C, "model_tables/number_deviation_binomial_C.html")

pairwise_results <- emmeans(model_2.C, pairwise ~ correct_width)

summary(pairwise_results$contrasts, infer = TRUE, adjust = "Tukey")

# Combine all model tables ---------------------------------------------
source("src/combine_html_files.R")
# Örnek kullanım
combine_html_files(input_folder = "model_tables", output_file = "combined_models.html")
