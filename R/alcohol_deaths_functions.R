library(tidyverse)

tx_alcohol_deaths <-
  read_csv("data/processed/tx_alcohol_deaths.csv")


alc_spec_deaths <-
  read_csv("data/processed/ons_alcohol-specific_deaths.csv")

alc_spec_deaths |> 
  group_by(individual_cause_of_death_description) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(-n) |> 
  filter(n > 0) |> 
  write_csv("data/processed/ons_alcohol-specific_deaths_by_cause.csv")


tx_alcohol_deaths |> 
  group_by(death_cause) |> 
  summarise(count  = sum(count), .groups = "drop") |> 
  arrange(-count) |> 
  write_csv("data/processed/tx_alcohol_deaths_by_cause.csv")
