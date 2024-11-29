library(tidyverse)
#source("R/themes.R")
#source("R/dhsc_colour_palette.R")

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

 cut_age_groups <-
  function(x) {
    cut(
      x,
      breaks = c(20, 24, 34, 44, 54, 64, 74, 84, 89),
      labels = c(
        "18-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75-84",
        "85-89"
      ),
      right = TRUE
    )
  }
 
tx_by_age$age_group <- cut_age_groups(x = pull(tx_by_age, age))

tx_by_age_group <- 
tx_by_age |> 
  group_by(sex, age_group) |> 
  summarise(count = sum(count), .groups ="drop") |> 
  rename("n" = count)

alc_deaths_by_age_group <- 
bind_rows(
ons_by_age,
tx_by_age_group
) |> 
  group_by(sex, age_group) |> 
  summarise(n = sum(n), .groups = "drop")


alc_deaths_by_age_group
source("R/get_data.R")

get_life_tables()

unique(alc_deaths_by_age_group$age_group)

 cut_age_groups <-
  function(x) {
    cut(
      x,
      breaks = c(20, 24, 34, 44, 54, 64, 74, 84, 89),
      labels = c(
        "18-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75-84",
        "85-89"
      ),
      right = TRUE
    )
  }