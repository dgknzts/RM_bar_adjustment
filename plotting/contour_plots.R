# libraries ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggtext)
library(viridis)

# importing and processing ------------------------------------------------
setwd("G:/My Drive/Projects/RM_adjustment/")
data <- read.csv("Data/processed.csv") 

data <- data %>% 
  filter(number_deviation %in% c(-1,0))


## by participant and correction for across participants variation  ---------------------------------
data_by_pp <- data %>%
  group_by(exp_version) %>%
  mutate(grand_avg = mean(width_deviation)) %>%
  group_by(subID, exp_version) %>%
  mutate(participant_avg = mean(width_deviation),
         temp_width_deviation = width_deviation - participant_avg,
         adj_width_deviation = temp_width_deviation + grand_avg) %>% #Cousineau, 2005
  group_by(correct_num, correct_width, number_deviation, subID, exp_version) %>%
  summarise(
    mean_width = mean(adj_width_deviation), 
    n = n()
  )

## across participant ------------------------
data_across_pp <- data_by_pp %>%
  group_by(correct_num, correct_width, number_deviation, exp_version) %>%
  summarise(
    width_mean = mean(mean_width),
    width_sd = sd(mean_width),
    n = n()
  ) %>%
  mutate(
    width_SEM = width_sd / sqrt(n),
    width_CI = width_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

# contour plot -----------------------------------------------------------
ggplot(data, aes(x = correct_num, 
                 y = correct_width)) +
  geom_contour_filled(aes(z = width_deviation), 
                      binwidth = 0.02,
                      color = "black",
                      size = 0.1) +
  facet_grid(exp_version ~ number_deviation) +
  theme_classic() +
  scale_x_continuous(breaks = c(3, 4, 5)) +
  scale_y_continuous(breaks = c(0.1, 0.25, 0.4, 0.55)) +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0))

# raster plot -------------------------------------------------------------
ggplot(data, aes(x = correct_num, 
                 y = correct_width)) +
  geom_raster(aes(fill = width_deviation), interpolate = TRUE) +
  scale_fill_gradientn(colors = viridis(100),  
                       limits = c(-0.2, 0.15),
                       oob = scales::squish) +
  facet_grid(exp_version ~ number_deviation) +
  theme_classic() +
  scale_x_continuous(breaks = c(3, 4, 5)) +
  scale_y_continuous(breaks = c(0.1, 0.25, 0.4, 0.55))
  
# raster plot with contours ----------------------------------------------
ggplot(data, aes(x = correct_num, 
                 y = correct_width)) +
  geom_raster(aes(fill = width_deviation), interpolate = TRUE) +
  geom_contour(data = data_across_pp, 
               aes(x = correct_num, y = correct_width, 
                   z = width_mean), 
               binwidth = 0.02, 
               color = "black", 
               size = 0.3) +
  scale_fill_gradientn(colors = viridis(100),  
                       limits = c(-0.2, 0.15),    
                       oob = scales::squish) +
  facet_grid(exp_version ~ number_deviation) +
  theme_classic() +
  scale_x_continuous(breaks = c(3, 4, 5)) +
  scale_y_continuous(breaks = c(0.1, 0.25, 0.4, 0.55))

# actual vs response density -------------------------------------------
data_AB <- data %>%
  filter(exp_version %in% c("Exp1A","Exp1B"))

data_A <- data_AB %>%
  filter(exp_version == "Exp1A")
data_B <- data_AB %>%
  filter(exp_version == "Exp1B")


ggplot(data_AB, aes(x = actual_width_density, 
                    y = response_width_density)) +
  geom_density_2d_filled(aes(fill = ..level..), alpha = 0.8) +  # Yoğunluk dolgu
  scale_fill_viridis_d(option = "viridis") +  # Viridis renk paleti
  facet_grid(exp_version ~ number_deviation) +  # Paneller
  theme_void() +
  labs(fill = "Density Level") +  # Legend başlığı
  scale_x_continuous(name = "Actual Width Density",
                     limits = c(0, 1)) +
  scale_y_continuous(name = "Response Width Density") +
  coord_equal()


# actual vs response spacing -------------------------------------------
ggplot(data_AB, aes(x = correct_space, 
                    y = response_space)) +
  geom_density_2d_filled(aes(fill = ..level..), alpha = 0.8) +  # Yoğunluk dolgu
  scale_fill_viridis_d(option = "viridis") +  # Viridis renk paleti
  facet_grid(exp_version ~ number_deviation) +  # Paneller
  theme_classic() +
  labs(fill = "Density Level") +  # Legend başlığı
  scale_x_continuous(name = "Actual Spacing",
                     limits = c(0.2, 1.5)) +
  scale_y_continuous(name = "Response Spacing",
                     limits = c(0.2, 1.5)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred")


