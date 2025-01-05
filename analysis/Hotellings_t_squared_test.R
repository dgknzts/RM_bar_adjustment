# Libraries
#install.packages("MVTests")
library(MVTests)
library(dplyr)

# Import data
setwd("G:/My Drive/Projects/RM_adjustment")
data <- read.csv("data/processed.csv")

# Experiment 1A ---------------------------------------------------------
df_1A <- data %>% 
  filter(exp_version == "Exp1A") %>%
  filter(number_deviation %in% c("-1" ,"0")) %>% #comparing only -1 and 0
  mutate(number_deviation = as.factor(number_deviation)) %>%
  select(width_deviation, number_deviation)

# Sep datasets into number dev -1 and 0
result <- TwoSamplesHT2(data = df_1A,
                       group = "number_deviation",
                       alpha = 0.05,
                       Homogenity = FALSE)


