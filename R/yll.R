library(testthat)
source("R/drug_deaths_functions.R")
source("R/themes.R")
source("R/dhsc_colour_palette.R")


drug_poisoning_deaths_file <-
  "data/raw/ndtms_mortality_data.parquet"
deaths_in_treatment_file <-
  "data/raw/tx_deaths_la_2122_2223.parquet"

# Process national data for testing ---------------------------------------


national_data <-
  combine_national_data(
    poisoning_data = process_poisoning_data(
      file_path = drug_poisoning_deaths_file,
      date_of = "occurrence",
      years = 2022
    ),
    treatment_deaths_data = process_deaths_in_treatment(
      file_path = deaths_in_treatment_file,
      years = 2022,
      exclude_poisoning = TRUE,
      by_treatment_status = TRUE,
      by_death_cause = FALSE,
      exclude_alcohol_specific_deaths = TRUE
    )
  )

national_data <-
  relabel_national_data(national_data = national_data)

national_data <- 
national_data |> 
  filter(death_category != "Non-poisoning deaths: Died one or more years following discharge")

# Load deaths data by age and sex -----------------------------------------


poisoning_deaths_by_age <-
  process_poisoning_data(
  file_path = drug_poisoning_deaths_file,
  date_of = "occurrence",
  years = 2022,
  by = "age",
  by_sex = TRUE
) |> 
  rename("age" = ageinyrs) |> 
  group_by(age, sex) |> 
  summarise(count = sum(count), .groups = "drop") 


non_poisoning_deaths_by_age <-
  process_deaths_in_treatment(
  file_path = deaths_in_treatment_file,
  years = 2022,
  by = "age",
  exclude_poisoning = TRUE,
  by_treatment_status = TRUE,
  by_death_cause = FALSE,
  by_sex = TRUE,
  exclude_alcohol_specific_deaths = TRUE
) |>
  filter(treatment_status != "Died one or more years following discharge") |> 
  group_by(age, sex) |> 
  summarise(count = sum(count), .groups = "drop")


deaths_by_age <-
  bind_rows(
    poisoning_deaths_by_age,
    non_poisoning_deaths_by_age
  ) |>
  group_by(age, sex) |>
  summarise(count = sum(count), .groups = "drop")

# Test --------------------------------------------------------------------


# testthat::test_that("Total deaths by age group equal to total deaths in national data", {
#   expect_equal(
#     sum(deaths_by_age$count),
#     sum(national_data$count)
#   )
# })




life_tables <- read_csv("data/processed/life_tables.csv")


deaths_and_life_ex <- 
  left_join(deaths_by_age, life_tables, by = c("age", "sex"))

# Method I ----------------------------------------------------------------

# Methods S4 Global Burden of Disease Approach
# Page 9 of supplementary PDF Chudasama et al. (2022)

yll <-   
deaths_and_life_ex |> 
  mutate(YLL = count * ex)


cut_age_groups <-
  function(x) {
    cut(
      x,
      breaks = c(17, 24, 34, 44, 54, 64, 74, 84, Inf),
      labels = c(
        "18-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75-84",
        "85+"
      ),
      right = TRUE
    )
  }

yll_age_group <-
  yll |>
  mutate(age_group = cut_age_groups(age)) |>
  group_by(age_group) |>
  summarise(
    count = sum(count),
    YLL = sum(YLL)
  )


# Test --------------------------------------------------------------------


# testthat::test_that("Total deaths by age group equal to total deaths in national data", {
#   expect_equal(
#     sum(yll_age_group$count),
#     sum(national_data$count))
# })
# 

# Output table ------------------------------------------------------------


yll_age_group |>
  rename("Deaths (n)" = count) |>
  rename_with(.cols = c(age_group), .fn = snakecase::to_sentence_case) |>
  janitor::adorn_totals(where = "row") |>
  write_csv("data/processed/yll_age_group.csv")


yll_m1 <- scales::comma(sum(yll_age_group$YLL))

# Age group plot ----------------------------------------------------------


yll_plot <-
  yll_age_group |>
  ggplot(aes(x = age_group, y = YLL)) +
  geom_col(colour = "black", fill = afcolours::af_colours(n = 1)[1], width = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Age group", y = "YLL", title = "Years of life lost due to deaths associated with drug use", subtitle = glue::glue("The crude expected years of life lost is {scales::comma(sum(yll_age_group$YLL))}"))


png(filename = "plots/yll_plot_age_group.png", height = 20, width = 26, units = "cm", res = 200)
yll_plot
dev.off()


# Method II ---------------------------------------------------------------

calculate_yll <-
  function(number.deaths,
           average.age.death,
           model.life.expectancy,
           discount.rate = 0.03,
           beta.constant = 0.04,
           modulation.constant = 0,
           adjustment.constant = 0.1658) {
    ## abbreviate inputs
    N <- number.deaths
    a <- average.age.death
    L <- model.life.expectancy
    r <- discount.rate
    b <- beta.constant
    K <- modulation.constant
    CC <- adjustment.constant
    ## do calculations
    if (discount.rate == 0) {
      N * (K * CC * ((exp(-b * a)) / b^2) * ((exp(-b * L)) * (-b * (L + a) - 1) - (-b * a - 1)) + ((1 - K) * L))
    } else {
      N * (K * ((CC * exp(r * a)) / (-(r + b)^2)) * ((exp(-(r + b) * (L + a)) * (-(r + b) * (L + a) - 1)) - (exp(-(r + b) * a) * (-(r + b) * a - 1))) + ((1 - K) / r) * ((1 - exp(-r * L))))
    }
  }


yll <-
  yll |>
  rowwise() |>
  mutate(YLL_ii = calculate_yll(number.deaths = count, average.age.death = age, model.life.expectancy = ex))

yll_age_group_ii <-
  yll |>
  mutate(age_group = cut_age_groups(age)) |>
  group_by(age_group) |>
  summarise(
    count = sum(count),
    YLL = sum(YLL),
    YLL_ii = sum(YLL_ii)
  )

yll_age_group_ii |>
  ggplot(aes(x = age_group, y = YLL_ii)) +
  geom_col(
    colour = "black",
    fill = afcolours::af_colours(n = 1)[1],
    width = 0.6
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Age group",
    y = "YLL",
    title = "Years of life lost due to deaths associated with drug use",
    subtitle = glue::glue(
      "The discounted expected years of life lost is {scales::comma(sum(yll_age_group_ii$YLL_ii))}"
    )
  )

yll_age_group_ii |> 
  pivot_longer(cols = c(YLL, YLL_ii), names_to = "method", values_to = "YLL") |> 
  mutate(method = case_match(method, "YLL" ~ "Method I", "YLL_ii" ~ "Method II")) |> 
  ggplot(aes(x = age_group, y = YLL)) + 
  geom_col(aes(fill = method), position = "dodge", colour = "black")  + 
  my_theme +
  labs(fill = NULL, title = "YLL method results comparison", x = "Age group") +
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_dhsc()

yll_age_group_ii |> 
  pivot_longer(cols = c(YLL, YLL_ii), names_to = "method", values_to = "YLL") |> 
  mutate(method = case_match(method, "YLL" ~ "Method I", "YLL_ii" ~ "Method II")) |> 
  group_by(method) |> 
  summarise(YLL = sum(YLL), .groups = "drop") |> 
  ggplot(aes(x = method, y = YLL)) + 
  geom_col(aes(fill = method), position = "dodge", colour = "black",  width = 0.3)  + 
  my_theme +
  labs(fill = NULL, title = "YLL method results comparison", x = NULL) +
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_dhsc()

# Per population (for comparison w/ ONS) ----------------------------------

population_2022 <- 
  read_csv("data/processed/population_2022.csv")

population_2022_age_group <- 
population_2022 |> 
  group_by(age) |> 
  summarise(population_2022 = sum(population_2022), .groups = "drop") |> 
  mutate(age_group = cut_age_groups(age)) |> 
  filter(!is.na(age_group)) |> 
  group_by(age_group) |> 
  summarise(population_2022 = sum(population_2022), .groups = "drop") 

yll_age_group_ii <- 
left_join(
  yll_age_group_ii,
  population_2022_age_group
) |> 
  mutate(YLL_per_population  = YLL / (population_2022/100e03)) |> 
  mutate(YLL_ii_per_population  = YLL_ii / (population_2022/100e03))

yll_age_group_ii |> 
  janitor::adorn_totals(where = "row")


# ONS avoidable mortality data --------------------------------------------

avoidable_mortality <- 
  read_csv("data/processed/avoidable_mortality_2022.csv")

avoidable_mortality <- 
avoidable_mortality |> 
  group_by(cause) |> 
  summarise(across(england_syll_per_population:england_upper_95_percent_confidence_limit, sum), .groups = "drop")

avoidable_mortality

yll_age_group_ii |> 
  select(-contains("_per")) |> 
  summarise(across(YLL:population_2022, sum)) |> 
  mutate(across(YLL:YLL_ii, ~.x/(population_2022/100e03)))
  