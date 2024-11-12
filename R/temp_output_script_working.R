

library(flextable)

source("R/drug_deaths_functions.R")
source("R/plotting_functions.R")
source("R/themes.R")
source("R/dhsc_colour_palette.R")

drug_poisoning_deaths_file <-
  "data/raw/ndtms_mortality_data.parquet"
deaths_in_treatment_file <-
  "data/raw/tx_deaths_la_2122_2223.parquet"

national_data <- 
combine_national_data(
  poisoning_data = process_poisoning_data(file_path = drug_poisoning_deaths_file,
                                          date_of = "occurence",
                                          years = 2022),
  treatment_deaths_data = process_deaths_in_treatment(
    file_path = deaths_in_treatment_file,
    years = 2022,
    exclude_poisoning = TRUE,
    by_treatment_status = TRUE,
    by_death_cause = FALSE
  )
)

national_data <- 
  relabel_national_data(national_data = national_data)

p1 <- plot_national_data(plot_data = national_data)


p1 <- add_plot_annotations(plot = p1, data = national_data)

 create_cause_of_death_table(
  process_deaths_in_treatment(
    file_path = deaths_in_treatment_file,
    years = 2022,
    exclude_poisoning = TRUE,
    by_treatment_status = TRUE,
    by_death_cause = TRUE
  )
)
  
drug_misuse_age_data <- function(){
 df1 <- 
process_poisoning_data(file_path = drug_poisoning_deaths_file, by = "age", years = 2022) |> 
  mutate(death_cause = "Drug poisoning") |> 
  select(-additional_poisoning_deaths) |> 
  select(-dod_year) |> 
  rename("age" = ageinyrs)

 df2 <- 
process_deaths_in_treatment(file_path = deaths_in_treatment_file, by = "age", by_treatment_status = TRUE, by_death_cause = TRUE, years = 2022) |> 
  filter(treatment_status != "Died one or more years following discharge") |> 
  group_by(age, death_cause) |> 
  summarise(count = sum(count))
 
 bind_rows(df1, df2) |> 
   mutate(death_category = "Deaths associated with drug use")
 
}

drug_misuse_age_data() |> 
  group_by(age, death_category) |> 
  summarise(count = sum(count))


ons_leading_mortality_causes <- 
read_csv("data/processed/ons_leading_mortality_causes.csv") |> 
  filter(age_group != "All ages")


cut_age_groups <- 
  function(x) {
    cut(
      x,
      breaks = c(19, 34, 49, 64, 79, Inf),
      labels = c(
        "20 to 34 years",
        "35 to 49 years",
        "50 to 64 years",
        "65 to 79 years",
        "80 years and over"
      ),
      right = TRUE
    )
  }

deaths_related_to_drug_misuse <- 
drug_misuse_age_data() |>
  group_by(age, death_category) |>
  summarise(count = sum(count)) |>
  filter(age > 19) |> 
  rowwise() |>
  mutate(age_group = cut_age_groups(age)) |> 
  group_by(age_group, death_category) |> 
  summarise(count = sum(count))


ons_leading_mortality_causes <- 
  read_csv("data/processed/ons_leading_mortality_causes.csv") |> 
  filter(age_group != "All ages") |> 
  select(age_group, leading_cause, deaths) |> 
  filter(age_group %in% c(
    "20 to 34 years",
    "35 to 49 years",
    "50 to 64 years",
    "65 to 79 years",
    "80 years and over"
  )) |> 
  rename("death_category" = leading_cause) |> 
  rename("count" = deaths)


bind_rows(deaths_related_to_drug_misuse, ons_leading_mortality_causes) |> 
  filter(age_group != "80 years and over") |> 
  arrange(age_group, count) |> 
  mutate(death_category = as_factor(death_category)) |> 
  ggplot(aes(x = death_category, y = count, fill = death_category)) + 
  geom_col(colour = "black") + 
  facet_wrap(~age_group, scales = "free") + 
  coord_flip() + 
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_manual(values = c("Deaths associated with drug use" = "red")) + 
  my_theme +
  theme(legend.position = "none") + 
  labs(y = "Number of deaths (2022)", x = "Cause of death")
