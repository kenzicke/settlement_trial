library(tidyverse)
library(viridis)
library(ggpubr)

data <- read_csv('settlement_data.csv')
filter_data <- filter(data, contents != "blank")


ggplot(filter_data, aes(x = treat,
                        y = count_fin,
                        na.rm = TRUE)) +
  geom_point(aes(color = rep),
             na.rm = TRUE) +
  geom_jitter(alpha = 0.4,
              width = 0.4,
              height = 0) +
  facet_wrap(vars(species))

#filter_data$rep = factor(filter_data$rep)

ggplot(filter_data, aes(x = treat,
                        y = count_init,
                        na.rm = TRUE)) +
  geom_boxplot(aes(alpha = 0.4)) +
  geom_jitter( width = 0.4,
               height = 0,
               aes(color = as.factor(rep))) +
  facet_wrap(~species) +
  guides(alpha = "none") +
  labs(color = "replicate")

#Plot initial settlement count by treatment, wrap species
ggplot(filter_data, aes(x = treat,
                        y = count_init)) +
  stat_summary(geom = "pointrange",
               color = "black",
               size = 0.8) +
  geom_jitter(aes(color = as.factor(rep),
              alpha = 0.4,
              width = 0.4,
              height = 0)) +
  facet_wrap(~species) +
  labs(title = "Initial settlement count",
       x = "Treatment",
       y = "Initial count") +
  guides(alpha = "none") +
  labs(color = "tile replicate")


#Plot final settlement count by treatment, wrap species
ggplot(filter_data, aes(x = treat,
                        y = count_fin)) +
  stat_summary(geom = "pointrange",
               color = "black",
               size = 0.8) +
  geom_jitter(alpha = 0.4,
              width = 0.4,
              height = 0,
              aes(color = as.factor(rep))) +
  facet_wrap(~species) +
  labs(title = "Final settlement count",
       x = "Treatment",
       y = "Final count")


#Plot settlement all species
ggplot(filter_data, aes(x = treat,
                        y = count_fin)) +
  stat_summary(geom = "pointrange",
               color = "black",
               size = 0.8) +
  geom_jitter(aes(alpha = 0.4,
                  width = 0.4,
                  height = 0,
                  color = as.factor(rep),
                  shape = species)) +
  labs(title = "Final settlement count",
       x = "Treatment",
       y = "Final count")

#Plot settlement by tile "yes", "no"
ggplot(filter_data, aes(x = treat,
                        y = count_fin)) +
  stat_summary(geom = "pointrange",
               color = "black",
               size = 0.8) +
  geom_jitter(aes(alpha = 0.4,
                  width = 0.4,
                  height = 0,
                  color = rep)) +
  labs(title = "Final settlement count",
       x = "Treatment",
       y = "Final count")

#total settlemrs

totals <- filter_data %>%
  group_by(species, treat) %>%
  summarise(total_settlers = sum(count_fin, na.rm = TRUE), .groups = "drop")

ggplot(totals, aes(x = treat,
                   y = total_settlers,
                   fill = treat)) +
  geom_col() +
  facet_wrap(~ species) +
  labs(x = "Treatment",
       y = "Total settlers") +
  theme_classic()

#percent settlement

filter_data <- filter_data %>%
  mutate(prop_settle = count_init / 20,
         perc_settle = 100 * prop_settle)

filter_data_pstr <- filter_data[filter_data$species == "pstr",]

ggplot(filter_data_pstr, aes(x = treat,
                        y = perc_settle)) +
  stat_summary(geom = "pointrange",
               color = "black",
               size = 0.6) +
  geom_jitter(alpha = 0.4,
              width = 0.4,
              height = 0,
              aes(color = as.factor(rep))) +
  facet_wrap(~species) +
  labs(color = "tile replicate",
       x = "treatment",
       y = "% settlement",
       title = "% settled out of 20") +
  theme_bw()

