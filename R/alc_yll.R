
library(tidyverse)

source("R/age_group_parse_function.R")  # Helper function for parsing age groups
source("R/get_data.R")                  # Data retrieval function
# source("R/themes.R")                  # Optional theme configurations, not used for now
# source("R/dhsc_colour_palette.R")     # Optional color palette, also not used for now

# Merge data sources ------------------------------------------------------

# Load treatment (tx) alcohol-related deaths dataset from NDTMS
tx_alcohol_deaths <- read_csv("data/processed/tx_alcohol_deaths.csv")

# Summarize treatment (tx) data by age and sex (grouping and counting deaths)
tx_by_age <-
  tx_alcohol_deaths |>
  group_by(age, sex) |>
  summarise(count = sum(count), .groups = "drop")

# Load UK alcohol-specific deaths dataset from ONS
alc_spec_deaths <- read_csv("data/processed/ons_alcohol-specific_deaths.csv")

# Summarize ONS data by age group and sex, cleaning up "sex" values
ons_by_age <-
  alc_spec_deaths |>
  group_by(age_group, sex) |>
  summarise(n = sum(n), .groups = "drop") |>
  mutate(sex = str_remove(sex, "s"))  # Removes unnecessary "s" at the end of 'sex'

# Quick peek at the summarized data
tx_by_age
ons_by_age

# Extract unique age groups from ONS data
age_groups <- unique(pull(ons_by_age, age_group))

# Parse the age groups into a usable format
age_df <- parse_age_groups(age_groups)

# Get the individual ages from treatment (tx) data
ages <- pull(tx_by_age, age)

# Assign these ages to corresponding age groups
categorized_ages <- assign_age_group_vectorized(ages, age_df)

# Add the new "age_group" column to the treatment (tx) data
tx_by_age <-
  tx_by_age %>%
  mutate(age_group = categorized_ages)

# Summarize the treatment (tx) data by age group and sex
tx_by_age_group <-
  tx_by_age %>%
  group_by(age_group, sex) %>%
  summarise(count = sum(count), .groups = "drop")

# Rename the "count" column to "n" to match the ONS dataset
tx_by_age_group <-
  tx_by_age_group %>%
  rename("n" = count)

# Combine (bind) treatment (tx) and ONS data, then re-summarize by age group and sex
alc_deaths_by_age_group <-
  bind_rows(tx_by_age_group, ons_by_age) %>%
  group_by(age_group, sex) %>%
  summarise(n = sum(n), .groups = "drop")

# Clean up the "sex" column by making everything lowercase for consistency
alc_deaths_by_age_group <-
  alc_deaths_by_age_group |>
  mutate(sex = tolower(sex))


# Get life table data (assuming a pre-defined function)
get_life_tables()

# Parse age groups for life table integration
age_groups <- unique(alc_deaths_by_age_group$age_group)
age_df <- parse_age_groups(age_groups)

# Extract ages from the life tables
ages <- pull(life_tables, age)

# Categorize life table ages into age groups
categorized_ages <- assign_age_group_vectorized(ages, age_df)

# Create a new dataframe linking ages to age groups
age_df <- data.frame(Age = ages, Age_Group = categorized_ages)

# Merge life table data with the age group information
life_tables <-
  left_join(life_tables, age_df, by = c("age" = "Age")) |>
  unique()

# Summarize life table data by age group and sex, calculating average life expectancy (ex)
life_tables <-
  life_tables |>
  group_by(Age_Group, sex) |>
  summarise(ex = mean(ex))

# Combine alcohol deaths data with life table data
data <-
  left_join(alc_deaths_by_age_group, life_tables, by = c("age_group" = "Age_Group", "sex"))

# Test to ensure the total deaths (n) across datasets add up as expected
test_that(
  "deaths (n) consistent",
  expect_equal(
    sum(data$n),  # Total deaths in the combined dataset
    sum(tx_alcohol_deaths$count) + sum(ons_by_age$n)  # Sum of deaths from individual sources
  )
)
