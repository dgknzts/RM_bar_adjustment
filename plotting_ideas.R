
# libraries ---------------------------------------------------------------


library(ggplot2)
library(dplyr)

# importing and processing ------------------------------------------------


data <- read.csv("processed.csv") %>% 
  filter(exp_version == "Exp1A") %>% 
  #mutate(response_width = response_width*response_num) %>%
  filter(number_deviation %in% c(-1,0))

## by participant ---------------------------------
data_by_pp <- data %>%
  group_by(correct_num, correct_width, number_deviation, subID) %>%
  summarise(
    mean_width = mean(response_width), # Dynamically refer to the variable
    n = n()
  )

## across participants --------------------------
data_across_pp <- data_by_pp %>%
  group_by(correct_num, correct_width, number_deviation) %>%
  summarise(
    width_mean = mean(mean_width),
    width_sd = sd(mean_width),
    n = n()
  ) %>%
  mutate(
    width_SEM = width_sd / sqrt(n),
    width_CI = width_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

# Convert to factors
data_by_pp$correct_width <- as.factor(data_by_pp$correct_width)
data_across_pp$correct_width <- as.factor(data_across_pp$correct_width)
data_by_pp$number_deviation <- factor(data_by_pp$number_deviation, levels = c("-1", "0"))
data_across_pp$number_deviation <- factor(data_across_pp$number_deviation, levels = c("-1", "0"))


## adding x y axis coords to draw rectangles -------------------------------


rectangle_coords <- data_across_pp %>%
  group_by(correct_num, correct_width, number_deviation) %>%
  mutate(
    x_1 = width_mean,
    x_2 = 0,
    x_3 = 0,
    x_4 = width_mean,
    y_1 = (1.9),
    y_2 = (1.9),
    y_3 = 0,
    y_4 = 0
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = starts_with("x_"),
    names_to = "corner_x",
    values_to = "x"
  ) %>%
  pivot_longer(
    cols = starts_with("y_"),
    names_to = "corner_y",
    values_to = "y"
  ) %>%
  filter(str_sub(corner_x, -1, -1) == str_sub(corner_y, -1, -1)) # Match corners

# expected_rectangle_coords <- data_across_pp %>%
#   group_by(correct_num, correct_width) %>%
#   mutate(correct_width = as.numeric(as.character(correct_width))) %>%
#   mutate(
#     x_1 = (0 + correct_width),
#     x_2 = 0,
#     x_3 = 0,
#     x_4 = (0 + correct_width),
#     y_1 = 1.9,
#     y_2 = 1.9,
#     y_3 = 0,
#     y_4 = 0
#   ) %>%
#   ungroup() %>%
#   pivot_longer(
#     cols = starts_with("x_"),
#     names_to = "corner_x",
#     values_to = "x"
#   ) %>%
#   pivot_longer(
#     cols = starts_with("y_"),
#     names_to = "corner_y",
#     values_to = "y"
#   ) %>%
#   filter(str_sub(corner_x, -1, -1) == str_sub(corner_y, -1, -1)) %>%
#   filter(number_deviation == 0) %>%
#   mutate(number_deviation = "actual")
# 
# 
# rectangle_coords <- rbind(rectangle_coords, expected_rectangle_coords)

# Plotting -----------------------------------------------------------


ggplot(rectangle_coords, aes(x = x, y = y, fill = number_deviation)) +
  geom_polygon(color = "black", alpha = 0.5) +
  coord_fixed() +
  theme_classic() +
  labs(
    title = "Rectangles for Each Group",
    x = "X Coordinate",
    y = "Y Coordinate",
    fill = "Num Dev"
  ) +
  facet_grid(~ correct_num + correct_width)


# ggsave(x.png, plot = plot, width = 6, height = 5, units = "in", dpi = 300)