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