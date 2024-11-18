library(testthat)

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


life_tables<- 
life_tables |> 
  group_by(age) |> 
  summarise(across(ex_male:ex_female, mean)) |> 
  rowwise() |> 
  mutate(ex_average = mean(x = c(ex_male, ex_female))) |> 
  select(age,ex_average)

yll <- 
left_join(deaths_by_age, life_tables, by = "age")  |> 
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


testthat::test_that("Total deaths by age group equal to total deaths in national data",{
  expect_equal(
    sum(yll_age_group$count),
    sum(pull(filter(national_data, death_category != "Non-poisoning deaths: Died one or more years following discharge"), count))
  )}
  )


yll_age_group |> 
  select(-count) |> 
  janitor::adorn_percentages(denominator = "col")

yll_plot <- 
yll_age_group |> 
  ggplot(aes(x = age_group, y= YLL)) + 
  geom_col(colour = "black", fill = afcolours::af_colours(n = 1)[1], width = 0.6) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Age group", y = "YLL", title = "Years of life lost due to deaths associated with drug use", subtitle = glue::glue("The crude expected years of life lost is {scales::comma(sum(yll_age_group$YLL))}"))


png(filename = "plots/yll_plot_age_group.png", height = 20,  width = 26, units = "cm", res = 200)
yll_plot
invisible(dev.off())
