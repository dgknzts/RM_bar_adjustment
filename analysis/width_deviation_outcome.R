# Before modelling -----------------------------------------------------------
library(tidyverse)
library(emmeans)
library(lme4)

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
data <- read.csv("Data/processed.csv")

data$subID <- as.factor(data$subID)
data$correct_num <- as.factor(data$correct_num)
data$correct_width <- as.factor(data$correct_width)
data$response_num <- as.factor(data$response_num)
data$exp_version <- as.factor(data$exp_version)
data$keyboard_condition <- as.factor(data$keyboard_condition)
data$trial_type <- as.factor(data$trial_type)

data <- data %>%
  filter(number_deviation %in% c("-1" ,"0")) %>% #comparing only -1 and 0
  mutate(number_deviation = as.factor(number_deviation))

# Set reference num dev 0
data$number_deviation <- relevel(data$number_deviation, ref = "-1")
levels(data$number_deviation)

# Experiment 1A ------------------------------------------
# Selecting experiment 1A
data_1A <- data %>%
  filter(exp_version == "Exp1A")

## Model comparisons  -------------------------------------
# Full model with all the interactions
model_1A <- lmer(width_deviation ~  number_deviation *
                    correct_num*
                    correct_width*
                    spacing +
                    (1|subID),
                  data = data_1A)
model_table(model_1A)

## No interaction between number deviation and others
model_2A <- lmer(width_deviation ~  number_deviation +
                  correct_num *
                  correct_width *
                  spacing +
                  (1|subID),
                data = data_1A)

anova(model_1A, model_2A)

# testing hierarchical effects
model_3A <- lmer(width_deviation ~ number_deviation +
                  correct_width *
                  correct_num *
                  spacing +
                  (1 | subID) +
                  (1 | subID:correct_width) +
                  (1 | subID:spacing) + 
                  (1 | subID:correct_num),
                data = data_1A)

anova(model_2A, model_3A)

isSingular(model_3A)
model_3A@optinfo$convergence

# testing hierarchical effects
model_4A <- lmer(width_deviation ~ number_deviation *
                   spacing *
                   correct_width *
                   correct_num +
                   (1 | subID) +
                   (1 | subID:correct_width) +
                   (1 | subID:spacing) + 
                   (1 | subID:correct_num),
                 data = data_1A)

anova(model_3A, model_4A)

# Best and the most simple one is model_3A with no 
# interaction between number deviation and others but with random effects FOR EXP 1A
best_model_A = model_3A
model_table(best_model_A)
## Post-hoc comparisons -------------------------------------
pairwise_results_A <- emmeans(model_4A, #Using here with all the interactions to compare pairwise????
                              pairwise ~ number_deviation | correct_width + correct_num + spacing, 
                              pbkrtest.limit = nrow(data_1A))

summary_1A <- summary(pairwise_results_A$contrasts, 
                      infer = TRUE, 
                      adjust = "Tukey")

summary_1A$p.value <- round(summary_1A$p.value, 5)

# Experiment 1B ------------------------------------------
# Selecting experiment 1B
data_1B <- data %>%
  filter(exp_version == "Exp1B")

## Model comparisons  -------------------------------------
# Full model with all the interactions
model_1B <- lmer(width_deviation ~  number_deviation *
                   correct_num*
                   correct_width*
                   spacing +
                   (1|subID),
                 data = data_1B)
model_table(model_1B)

## No interaction between number deviation and others
model_2B <- lmer(width_deviation ~  number_deviation +
                   spacing *
                   correct_num *
                   correct_width +
                   (1|subID),
                 data = data_1B)

anova(model_1B, model_2B)
# 1B is better with all interactions

# testing hierarchical effects
model_3B <- lmer(width_deviation ~ number_deviation *
                   correct_width *
                   correct_num *
                   spacing +
                   (1 | subID) +
                   (1 | subID:correct_width) +
                   (1 | subID:spacing) + 
                   (1 | subID:correct_num),
                 data = data_1B)

anova(model_1B, model_3B) #model_3B is the best!

isSingular(model_3B)
model_3B@optinfo$convergence

# testing hierarchical effects
model_4B <- lmer(width_deviation ~ number_deviation *
                   correct_width *
                   correct_num *
                   spacing +
                   #(1 | subID) +
                   (1 | subID:correct_width) +
                   (1 | subID:spacing) + 
                   (1 | subID:correct_num),
                 data = data_1B)

anova(model_3B, model_4B) #model_4B is better (no single subID as random factor)


# Best and the most simple one is model_4B with all the
# interactions but with random effects except signle subID FOR EXP 1B
best_model_B = model_4B
model_table(best_model_B)
## Post-hoc comparisons -------------------------------------
pairwise_results_b <- emmeans(best_model_B, 
                              pairwise ~ number_deviation | correct_width + correct_num + spacing, 
                              pbkrtest.limit = nrow(data_1B))

summary_1B <- summary(pairwise_results_b$contrasts, 
                      infer = TRUE, 
                      adjust = "Tukey")

summary_1B$p.value <- round(summary_1B$p.value, 5)

# Experiment 1C ------------------------------------------
# Selecting experiment 1C
data_1C <- data %>%
  filter(exp_version == "Exp1C")

# We don't have spacing and width as the separate factors. 
# Three combinations of both of them in a block for three blocks/ three factors
# 0.25 width 0.5 spacing, 0.4 width 0.5 spacing, 0.55 width 0.9 spacing

## Model comparisons  -------------------------------------
# Full model with all the interactions
model_1C <- lmer(width_deviation ~  number_deviation *
                   correct_num *
                   correct_width +
                   (1|subID),
                 data = data_1C)

model_table(model_1C)

## No interaction between number deviation and others
model_2C <- lmer(width_deviation ~  number_deviation +
                   correct_num *
                   correct_width +
                   (1|subID),
                 data = data_1C)

anova(model_1C, model_2C) #1C is better

# testing hierarchical effects
model_3C <- lmer(width_deviation ~ number_deviation *
                   correct_width *
                   correct_num +
                   (1 | subID) +
                   (1 | subID:correct_width) +
                   (1 | subID:correct_num),
                 data = data_1C)

anova(model_1C, model_3C)

# testing hierarchical effects

model_4C <- lmer(width_deviation ~ number_deviation *
                   correct_width *
                   correct_num +
                   #(1 | subID) +
                   (1 | subID:correct_width) +
                   (1 | subID:correct_num),
                 data = data_1C)

anova(model_3C, model_4C) # 3C is the best

# Best and the most simple one is model_3C with all the
# interactions with all random effects FOR EXP 1C
best_model_C = model_3C
model_table(best_model_C)

## Post-hoc comparisons -------------------------------------
pairwise_results_c <- emmeans(best_model_C)

summary_1C <- summary(pairwise_results_c$contrasts,
                      infer = TRUE, 
                      adjust = "Tukey")

summary_1C$p.value <- round(summary_1C$p.value, 5)


# Best models for each experiment ---------------------------------------
model_table(best_model_A)
model_table(best_model_B)
model_table(best_model_C)

# Combining pairwise results into a df
summary_1A <- summary_1A %>%
  mutate(Experiment = "Exp1A")

summary_1B <- summary_1B %>%
  mutate(Experiment = "Exp1B")

summary_1C <- summary_1C %>%
  mutate(Experiment = "Exp1C")

all_pairwise_results <- bind_rows(summary_1A, summary_1B, summary_1C)


# Trying edge to edge spacing instead and compare with center to center --------------------------
summary(data$edge_to_edge_spacing)
# plot the edge to edge spacing distribution
ggplot(data, aes(x = edge_to_edge_spacing, y = width_deviation, color = number_deviation)) +
  geom_point(position = position_jitter(),
             alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_classic()

# Experiment 1A ------------------------------------------
data_1A <- data %>% filter(exp_version == "Exp1A")

# Centering the width deviation to zero
#data_1A$width_deviation <- data_1A$width_deviation - mean(data_1A$width_deviation)


# Model edge to edge
Model_1A_edge <- lmer(width_deviation ~  number_deviation *
                        correct_num*
                        correct_width*
                        edge_to_edge_spacing +
                        (1|subID),
                      data = data_1A)
model_table(Model_1A_edge)

Model_1A_center <- lmer(width_deviation ~  number_deviation *
                          correct_num*
                          correct_width*
                          correct_space +
                          (1|subID),
                        data = data_1A)
model_table(Model_1A_center)
anova(Model_1A_edge, Model_1A_center)
# Edge to edge is better...

# Reducing
Model_1.1A_edge <- lmer(width_deviation ~  number_deviation *
                          #correct_num *
                          correct_width *
                          edge_to_edge_spacing +
                          (1|subID),
                        data = data_1A)
anova(Model_1A_edge, Model_1.1A_edge)

# Experiment 1B ------------------------------------------
data_1B <- data %>% filter(exp_version == "Exp1B")

# Centering the width deviation to zero
#data_1B$width_deviation <- data_1B$width_deviation - mean(data_1B$width_deviation)

# Model edge to edge
Model_1B_edge <- lmer(width_deviation ~  number_deviation *
                        correct_num *
                        correct_width *
                        edge_to_edge_spacing +
                        (1|subID),
                      data = data_1B)
model_table(Model_1B_edge)

Model_1B_center <- lmer(width_deviation ~  number_deviation *
                          correct_num *
                          correct_width *
                          correct_space +
                          (1|subID),
                        data = data_1B)
model_table(Model_1B_center)

anova(Model_1B_edge, Model_1B_center)
# Edge is better...
# Reducing
Model_1.1B_edge <- lmer(width_deviation ~  number_deviation *
                          correct_num +
                          correct_width *
                          edge_to_edge_spacing +
                          (1|subID),
                        data = data_1B)
anova(Model_1B_edge, Model_1.1B_edge)

# Experiment 1C ------------------------------------------
# Does not have spacing as sep factor

# Modelling with taking variables as continuous
data <- read.csv("Data/processed.csv")

data$subID <- as.factor(data$subID)
data$exp_version <- as.factor(data$exp_version)
data$keyboard_condition <- as.factor(data$keyboard_condition)
data$trial_type <- as.factor(data$trial_type)

data <- data %>%
  filter(number_deviation %in% c("-1" ,"0")) %>% #comparing only -1 and 0
  mutate(number_deviation = as.factor(number_deviation))

# Set reference num dev 0
data$exp_version <- relevel(data$exp_version, ref = "Exp1B")
levels(data$exp_version)


# Modellin all experiments together
model_1_all <- lmer(width_deviation ~  number_deviation +
                      correct_num +
                      correct_width +
                      edge_to_edge_spacing +
                      exp_version +
                      (1|subID),
                    data = data)
model_table(model_1_all)
# Looks like Exp1A and 1B are are not differ from each other unlike Exp1C.

# Modelling A and B together?
data_AB <- data %>%
  filter(exp_version %in% c("Exp1A", "Exp1B"))

model_1.1_AB <- lmer(width_deviation ~  number_deviation +
                        correct_num +
                        correct_width +
                        edge_to_edge_spacing +
                        exp_version +
                        (1|subID),
                      data = data_AB)

model_1.2_AB <- lmer(width_deviation ~  number_deviation +
                        correct_num +
                        correct_width +
                        edge_to_edge_spacing +
                        (1|subID),
                      data = data_AB)
anova(model_1.1_AB, model_1.2_AB)

model_1.3_AB <- lmer(width_deviation ~  number_deviation *
                        correct_num *
                        correct_width *
                        edge_to_edge_spacing +
                        (1|subID),
                      data = data_AB)
anova(model_1.2_AB, model_1.3_AB)


