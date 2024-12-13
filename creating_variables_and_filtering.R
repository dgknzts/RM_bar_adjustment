df <- read.csv("all_exps.csv")

# Computing variables
df <- df %>%
  mutate(
    number_deviation = response_num - correct_num,
    width_deviation = response_width - correct_width,
    spacing_deviation = response_space - correct_space,
    response_stim_length = (response_space * (response_num- 1)) + response_width,
    compression_rate = response_stim_length / stim_length,
  )

# Filtering noise
df_filtered <-  df %>%
  filter(adjustment_duration > 1) %>% #adjustment time is too quick
  filter(adjustment_duration < 15) %>% #NOT SURE! 15 sec is too long
  filter(response_rt < 10) %>% #Yildirim & Sayim. Low accuracy and high confidence in redundancy masking
  filter(number_deviation < 4) %>% filter(number_deviation > -4) #same

