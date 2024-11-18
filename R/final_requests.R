
# Plot 1 -----------------------------------------------------------------



library(flextable)

source("R/drug_deaths_functions.R")
source("R/plotting_functions.R")


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
      by_death_cause = FALSE,
      exclude_alcohol_specific_deaths = TRUE
    )
  )

national_data <- 
  relabel_national_data(national_data = national_data)

p1 <- plot_national_data(plot_data = national_data)

p1 <- add_plot_annotations(plot = p1, data = national_data)

png(filename = "plots/plot_1.png", height = 11.72,  width = 23.44, units = "cm", res = 200)
p1
dev.off()
data_list[["plot_1"]] <- national_data



# Plot 2 ------------------------------------------------------------------

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

plot2_data <- 
  bind_rows(deaths_related_to_drug_misuse, ons_leading_mortality_causes) |> 
  filter(age_group != "80 years and over") 
  
data_list[["plot_2"]] <- plot2_data


plot_leading_death_causes <- 
function(age_grp){
  plot2_data %>%  
  ungroup() %>% 
  filter(age_group == age_grp) %>% 
  arrange(count) %>% 
  mutate(death_category = as_factor(death_category)) %>% 
  ggplot(aes(x = death_category, y = count, fill = death_category)) + 
  geom_col(colour = "black") + 
  facet_wrap(~age_group, scales = "free") + 
  coord_flip() + 
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_manual(values = c("Deaths associated with drug use" = "red")) + 
  my_theme +
  theme(legend.position = "none", plot.title.position = "plot") + 
  labs(
    title    = NULL,
    subtitle = NULL,
    y        = NULL,
    x        = NULL
  )
  
}

age_groups <- unique(pull(plot2_data, age_group))

plots2 <- lapply(age_groups, plot_leading_death_causes)

plot_leading_death_causes(age_groups[1])

cowplot::plot_grid(plotlist = plots2) 
plot_grid()

table2_data <-
process_deaths_in_treatment(
  file_path = deaths_in_treatment_file,
  years = 2022,
  exclude_poisoning = TRUE,
  by_treatment_status = TRUE,
  by_death_cause = TRUE
)

data_list[["table_2"]] <- table2_data


# Plot 3 ------------------------------------------------------------------

library(tidyverse)
library(afcolours)
library(openxlsx)
library(curl)

# Getting all cause deaths data, initially to see which age groups to use for
# drug/alcohol deaths; then to compare.

df <-
  read_csv("data/processed/drug_deaths_national.csv")


if (!file.exists("data/raw/ons_deaths_data_2023.xlsx")) {
  url <-
    "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2023/annualdeathregistrations2023.xlsx"
  
  curl_download(url = url,
                destfile = "data/raw/ons_deaths_data_2023.xlsx")
  
}


ons_all_deaths_age <-
  read.xlsx(
    xlsxFile = "data/raw/ons_deaths_data_2023.xlsx",
    sheet = 6,
    sep.names = "_",
    startRow = 5
  ) |>
  janitor::clean_names()

ons_all_deaths_age <-
  ons_all_deaths_age |>
  as_tibble()

period_age_all_cause <-
  # Deaths from all causes by year and age only.
  ons_all_deaths_age |>
  filter(
    year_of_registration %in% c(2022, 2023),
    sex != "All people",
    age_years != "All ages",
    marital_status == "All marital statuses"
  ) |>
  group_by(year_of_registration, age_years) |>
  summarise(number_of_deaths = sum(number_of_deaths))

period_age_all_cause <-
  period_age_all_cause |>
  mutate(age_years = as.numeric(str_remove(age_years, "[^\\d]+"))) |>
  # Exclude ages outside the range of the drug-related deaths figures
  filter(age_years >= 18) |>
  filter(age_years <= 100)


period_age_drd <-
  # All deaths we've categorized as drug-related, by year and age
  df |>
  group_by(period, age) |>
  summarise(count = sum(count))

colnames(period_age_all_cause) <-
  c("period", "age", "deaths_all_causes")

colnames(period_age_drd) <-
  c("period", "age", "deaths_related_to_drug_misuse")


data <-
  left_join(period_age_all_cause, period_age_drd) |>
  mutate(deaths_related_to_drug_misuse = zoo::na.fill(deaths_related_to_drug_misuse, 0))

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


plot3_data <- 
  data |> 
  filter(age < 55) |> 
  rowwise() |> 
  mutate(age_group = cut_age_groups(age)) |> 
  group_by(period, age_group) |> 
  summarise(across(where(is.numeric), sum)) |> 
  group_by(age_group) |> 
  summarise(across(where(is.numeric), mean)) |>  
  mutate(deaths_all_causes = deaths_all_causes - deaths_related_to_drug_misuse) |> 
  pivot_longer(cols = deaths_all_causes:deaths_related_to_drug_misuse) |> 
  mutate(name = str_replace(name, "all_causes", "all_other_causes")) |> 
  mutate(name = snakecase::to_sentence_case(name)) 

plot3_data %>% 
  ggplot(aes(x = age_group, y = value)) + 
  geom_col(aes(fill = name), position = "fill", width = 0.5, colour = "black", alpha = 0.8) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = af_colours(n = 4)) + 
  theme(
    text = element_text(size =50),
    legend.position = "bottom",
    legend.justification = "left"
  ) + 
  labs(fill = NULL,
       y = NULL,
       x = "Age group",
       title = "Deaths related to drug misuse as a proportion of all deaths, by age group",
       subtitle = "Average of 2022 and 2023 data"
  )


data_list[["plot_3"]] <- plot3_data


# Plot 4 ------------------------------------------------------------------



plot4_data <- 
  data |> 
  filter(age < 55) |> 
  rowwise() |> 
  mutate(age_group = cut_age_groups(age)) |> 
  group_by(period, age_group) |> 
  summarise(across(where(is.numeric), sum)) |> 
  group_by(age_group) |> 
  summarise(across(where(is.numeric), mean)) |>  
  mutate(deaths_all_causes = deaths_all_causes - deaths_related_to_drug_misuse) |> 
  pivot_longer(cols = deaths_all_causes:deaths_related_to_drug_misuse) |> 
  mutate(name = str_replace(name, "all_causes", "all_other_causes")) |> 
  mutate(name = snakecase::to_sentence_case(name)) 

plot4_data %>% 
  ggplot(aes(x = age_group, y = value)) + 
  geom_col(aes(fill = name), position = "identity", width = 0.5, colour = "black", alpha = 0.8) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = af_colours(n = 4)) + 
  theme(
    text = element_text(size = 50),
    legend.position = "bottom",
    legend.justification = "left"
  ) + 
  labs(fill = NULL,
       y = NULL,
       x = "Age group",
       title = "Count of deaths related to drug misuse deaths by all other causes\nby age group",
       subtitle = "Average of 2022 and 2023 data"
  )
p2

data_list[["plot_4"]] <- plot4_data

writexl::write_xlsx(data_list, path = "deaths_associated_substance_use_plot_data.xlsx")
