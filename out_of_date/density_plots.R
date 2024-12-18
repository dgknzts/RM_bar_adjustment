# libraries ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggridges)

# importing and processing ------------------------------------------------
data <- read.csv("processed.csv") %>% 
  filter(exp_version == "Exp1A") %>% 
  #mutate(number_deviation = ifelse(number_deviation < 0, -1, 0)) %>%
  #mutate(response_width = response_width*response_num) %>%
  filter(number_deviation %in% c(-1,0)) %>%
  mutate(number_deviation = as.factor(number_deviation),
         correct_width = as.factor(correct_width))

## width_deviation ---------------------------------
ggplot(data, 
       aes(x = width_deviation,
           y = fct_rev(correct_width),
           fill = number_deviation
       )
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density_ridges2(alpha = 0.5, 
                       scale = 0.9, 
                       bandwidth = 0.028
  ) +
  facet_wrap(~correct_num) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) 


         

## width_density_deviation ---------------------------------
ggplot(data, 
       aes(x = width_density_deviation,
           y =fct_rev(correct_width),
           fill = number_deviation
          )
       ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density_ridges2(alpha = 0.5, 
                       scale = 0.9, 
                       bandwidth = 0.13
                       ) +
  facet_wrap(~correct_num) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) 

## pooled_width_deviation ----------------------------------
ggplot(data, 
       aes(x = pooled_width_deviation,
           y = fct_rev(correct_width),
           fill = number_deviation
       )
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density_ridges2(alpha = 0.5, 
                       scale = 0.9, 
                       bandwidth = 0.125
  ) +
  facet_wrap(~correct_num) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) 
