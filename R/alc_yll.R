library(tidyverse)
source("R/themes.R")
source("R/dhsc_colour_palette.R")
source("R/age_group_parse_function.R")

tx_alcohol_deaths <-
  read_csv("data/processed/tx_alcohol_deaths.csv")

tx_by_age <-
tx_alcohol_deaths |>
  group_by(age, sex) |>
  summarise(count = sum(count), .groups ="drop")

alc_spec_deaths <-
  read_csv("data/processed/ons_alcohol-specific_deaths.csv")

ons_by_age <-
alc_spec_deaths |>
  group_by(age_group, sex) |>
  summarise(n = sum(n), .groups = "drop") |>
  mutate(sex = str_remove(sex, "s"))

age_df <-
  parse_age_groups(unique(pull(ons_by_age, age_group)))

age_df <-
age_df %>%
  filter(!is.infinite(upper), !is.infinite(lower))  %>%
  rowwise() %>%
  mutate(interval = list(seq(lower, upper, 1))) %>%
  unnest(interval)  %>%
  select(-upper, -lower)

tx_deaths_by_age_group  <-
left_join(tx_alcohol_deaths, age_df, by = c("age" = "interval")) %>%
  group_by(sex, age_group) %>%
  summarise(count = sum(count), .groups = "drop")

alc_deaths_by_age_group  <-
tx_deaths_by_age_group %>%
  rename("n" = count) %>%
  bind_rows(ons_by_age)

alc_deaths_by_age_group <-
alc_deaths_by_age_group %>%
  mutate(sex = tolower(sex))
source("R/get_data.R")

get_life_tables()

life_tables  <-
left_join(life_tables, age_df, by = c("age" = "interval")) %>%
  group_by(sex, age_group)  %>%
  summarise(ex = mean(ex))

alcohol_yll  <-
left_join(alc_deaths_by_age_group, life_tables, by = c("sex", "age_group")) %>%
  mutate(yll = n * ex) %>%
  group_by(age_group) %>%
  summarise(yll = sum(yll)) %>%
  filter(!is.na(yll))

alcohol_yll %>%
  ggplot(aes(x = age_group, y = yll)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  my_theme +
  labs(title = "Years of life lost due to alcohol", subtitle = "by age group", x = "Age group", y = "Years of life lost")

