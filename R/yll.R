library(testthat)
source("R/drug_deaths_functions.R")

drug_poisoning_deaths_file <-
  "data/raw/ndtms_mortality_data.parquet"
deaths_in_treatment_file <-
  "data/raw/tx_deaths_la_2122_2223.parquet"



poisoning_deaths_by_age <-
  process_deaths_in_treatment(
    file_path = deaths_in_treatment_file, years = 2022, by_treatment_status = TRUE, exclude_poisoning = TRUE, exclude_alcohol_specific_deaths = TRUE, by = "age"
  ) |>
  filter(treatment_status != "Died one or more years following discharge") |>
  group_by(age) |>
  summarise(count = sum(count))


non_poisoning_deaths_by_age <-
  process_poisoning_data(
    file_path = drug_poisoning_deaths_file, date_of = "occurence", years = 2022, by = "age"
  ) |>
  rename("age" = ageinyrs) |>
  group_by(age) |>
  summarise(count = sum(count))


deaths_by_age <-
  bind_rows(
    poisoning_deaths_by_age,
    non_poisoning_deaths_by_age
  ) |>
  group_by(age) |>
  summarise(count = sum(count))

life_tables <- read_csv("data/processed/life_tables.csv")


life_tables <-
  life_tables |>
  group_by(age) |>
  summarise(across(ex_male:ex_female, mean)) |>
  rowwise() |>
  mutate(ex_average = mean(x = c(ex_male, ex_female))) |>
  select(age, ex_average)

# Method I ----------------------------------------------------------------

# Methods S4 Global Burden of Disease Approach
# Page 9 of supplementary PDF Chudasama et al. (2022)

yll <-
  left_join(deaths_by_age, life_tables, by = "age") |>
  mutate(YLL = count * ex_average)


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

# Process national data for testing ---------------------------------------


national_data <-
  combine_national_data(
    poisoning_data = process_poisoning_data(
      file_path = drug_poisoning_deaths_file,
      date_of = "occurence",
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

# Test --------------------------------------------------------------------


testthat::test_that("Total deaths by age group equal to total deaths in national data", {
  expect_equal(
    sum(yll_age_group$count),
    sum(pull(filter(national_data, death_category != "Non-poisoning deaths: Died one or more years following discharge"), count))
  )
})


# Output table ------------------------------------------------------------


yll_age_group |>
  rename("Deaths (n)" = count) |>
  rename_with(.cols = c(age_group), .fn = snakecase::to_sentence_case) |>
  janitor::adorn_totals(where = "row") |>
  write_csv("data/processed/yll_age_group.csv")



# Age group plot ----------------------------------------------------------


yll_plot <-
  yll_age_group |>
  ggplot(aes(x = age_group, y = YLL)) +
  geom_col(colour = "black", fill = afcolours::af_colours(n = 1)[1], width = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Age group", y = "YLL", title = "Years of life lost due to deaths associated with drug use", subtitle = glue::glue("The crude expected years of life lost is {scales::comma(sum(yll_age_group$YLL))}"))


png(filename = "plots/yll_plot_age_group.png", height = 20, width = 26, units = "cm", res = 200)
yll_plot
invisible(dev.off())



# Process national data for testing ---------------------------------------


national_data <-
  combine_national_data(
    poisoning_data = process_poisoning_data(
      file_path = drug_poisoning_deaths_file,
      date_of = "occurence",
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

# Test --------------------------------------------------------------------


testthat::test_that("Total deaths by age group equal to total deaths in national data", {
  expect_equal(
    sum(yll_age_group$count),
    sum(pull(filter(national_data, death_category != "Non-poisoning deaths: Died one or more years following discharge"), count))
  )
})


# Output table ------------------------------------------------------------


yll_age_group |>
  rename("Deaths (n)" = count) |>
  rename_with(.cols = c(age_group), .fn = snakecase::to_sentence_case) |>
  janitor::adorn_totals(where = "row") |>
  write_csv("data/processed/yll_age_group.csv")



# Age group plot ----------------------------------------------------------


yll_plot <-
  yll_age_group |>
  ggplot(aes(x = age_group, y = YLL)) +
  geom_col(colour = "black", fill = afcolours::af_colours(n = 1)[1], width = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Age group", y = "YLL", title = "Years of life lost due to deaths associated with drug use", subtitle = glue::glue("The crude expected years of life lost is {scales::comma(sum(yll_age_group$YLL))}"))


png(filename = "plots/yll_plot_age_group.png", height = 20, width = 26, units = "cm", res = 200)
yll_plot
invisible(dev.off())

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
  mutate(YLL_ii = calculate_yll(number.deaths = count, average.age.death = age, model.life.expectancy = ex_average))

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
  mutate(method = case_match(method, "YLL" ~ "Method I", "YLL_ii" ~ "Method II"))



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
