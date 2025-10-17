################################################################################
# Alkalinity treatment analysis -- Settlement trial August 2025
################################################################################
#
# Kenzie M. Cooke
# kmc390@miami.edu
# 9.29.2025
################################################################################

# Load libraries
library(tidyverse)
library(janitor)
library(plotly)
library(seacarb)

# Import and clean data from Metrhom
alk_data <- read_csv("data/raw/settlement_alk_data.csv") |>
  clean_names() |>
  mutate(sample_num = str_extract(sample_id, "^\\d+")) |>   # ^ start of string, \\d a digit + one or more of the preceding thing
  group_by(sample_num) |>
  summarise(ta_av = mean(final_ta, na.rm = TRUE),
         n_reps = n(),
         ta_sd = sd(final_ta, na.rm = TRUE))  # num of rows in current group

# Import sample meta data
bottle_data <- read_csv("data/raw/bottle.csv") |>
  clean_names() |>
  rename(sample_num = bottle_id) |>
  mutate(sample_num = as.character(sample_num))

# Join TA results to sample metadata
bottle_joined <- bottle_data |>
  left_join(
    alk_data |>
      select(sample_num, ta_av, ta_sd),
    by = "sample_num"
  )

ggplot(bottle_joined,
       aes(x = date, y = ta_av, color = treatment)) +
  geom_jitter(width = 0.2) +
  theme_bw()


# Calculate treatment statistics
bottle_joined |>
  group_by(treatment) |>
  summarise(mean = mean(ta_av, na.rm = TRUE),
            pH = mean(p_h_pico, na.rm = TRUE),
            sd = sd(ta_av, na.rm = TRUE),
            n = sum(!is.na(ta_av)))


# Boxplot of traetment statistics
ggplot(bottle_joined, aes(x = treatment, y = ta_av, fill = treatment)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_discrete(labels = c("2700 umol", "3000 umol", "3300 umol", "3600 umol", "source low", "source high")) +
  labs(x = "Treatment", y = "Total alkalinity (umol/kg)")


