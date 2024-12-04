library(tidyverse)
source("R/age_group_parse_function.R")
source("R/get_data.R")
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


ons_by_age


alc_deaths_by_age_group <- 
bind_rows(
ons_by_age,
tx_by_age_group
) |> 
  group_by(sex, age_group) |> 
  summarise(n = sum(n), .groups = "drop")

alc_deaths_by_age_group <- 
alc_deaths_by_age_group |> 
  mutate(sex = tolower(sex))



get_life_tables()


age_groups <- 
  unique(alc_deaths_by_age_group$age_group)


# Define your age groups
# age_groups <- c("<1", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29",
#                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
#                 "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")


# Re-parse age groups with adjusted upper bounds
age_df <- parse_age_groups(age_groups)

# Test ages
ages <- 
  pull(life_tables, age) 

categorized_ages <-
  assign_age_group_vectorized(ages, age_df)

age_df <-
  data.frame(Age = ages, Age_Group = categorized_ages)
  
life_tables <- 
left_join(life_tables, age_df, by = c("age" = "Age")) |> 
  unique()

life_tables <- 
life_tables |> 
  group_by(Age_Group, sex) |> 
  summarise(ex = mean(ex))
life_tables

left_join(alc_deaths_by_age_group, life_tables, by = c("age_group" = "Age_Group", "sex"))

age_groups
