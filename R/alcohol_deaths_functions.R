library(tidyverse)
source("R/themes.R")
source("R/dhsc_colour_palette.R")

tx_alcohol_deaths <-
  read_csv("data/processed/tx_alcohol_deaths.csv")


alc_spec_deaths <-
  read_csv("data/processed/ons_alcohol-specific_deaths.csv")

alc_spec_deaths_by_cause <-
alc_spec_deaths |>
  group_by(individual_cause_of_death_description) |>
  summarise(n = sum(n), .groups = "drop") |>
  arrange(-n) |>
  filter(n > 0)

tx_alcohol_deaths_by_cause <-
tx_alcohol_deaths |>
  group_by(death_cause) |>
  summarise(count  = sum(count), .groups = "drop") |>
  arrange(-count)

total_alcohol_deaths <-
tx_alcohol_deaths_by_cause %>%
  mutate(count = case_when(death_cause == "Alcohol-specific death" ~ sum(alc_spec_deaths$n),
                   TRUE ~ count)) %>%
  mutate(death_category = if_else(
    death_cause == "Alcohol-specific death",
    "Alcohol-specific deaths (ONS)",
    "Additional deaths (NDTMS-ONS linkage)"
    ))



total_alcohol_deaths <-
total_alcohol_deaths %>%
  group_by(death_category) %>%
  summarise(count = sum(count), .groups = "drop")


total_alcohol_deaths %>%
  arrange(count) %>%
  mutate(death_category = as_factor(death_category)) %>%
  ggplot(aes(x = 1, y = count)) +
  geom_col(aes(fill = death_category), colour = "black") +
  my_theme +
  theme(legend.position = "none") +
  labs(x = NULL, y = "Deaths (n)") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(limits = c(0,3))
  scale_fill_dhsc()

tx_alcohol_deaths_by_cause %>%
  filter(death_cause != "Alcohol-specific death") %>%
  arrange(count) %>%
  mutate(death_cause = as_factor(death_cause)) %>%
  ggplot(aes(x = 1, y = count)) +
  geom_col(aes(fill = death_cause), colour = "black") +
  my_theme +
  theme(legend.position = "right") +
  labs(x = NULL, y = "Deaths (n)") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(limits = c(0,3))



