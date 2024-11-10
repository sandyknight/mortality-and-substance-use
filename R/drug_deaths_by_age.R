library(tidyverse)
library(openxlsx)
library(curl)

# Getting all cause deaths data, initially to see which age groups to use for 
# drug/alcohol deaths; then to compare. 

df <-
  read_csv("data/processed/drug_deaths_national.csv") 
  #filter(treatment_status != "Died one or more years following discharge")

if (!file.exists("data/raw/ons_deaths_data_2023.xlsx")){
  
url <-
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2023/annualdeathregistrations2023.xlsx"

curl_download(
  url = url,
  destfile = "data/raw/ons_deaths_data_2023.xlsx"
  )

}


ons_all_deaths_age <- 
read.xlsx(xlsxFile = "data/raw/ons_deaths_data_2023.xlsx", sheet = 6, sep.names = "_", startRow = 5) |> 
  janitor::clean_names()

ons_all_deaths_age <- 
ons_all_deaths_age |> 
  as_tibble()

period_age_all_cause <- # Deaths from all causes by year and age only.  
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


period_age_drd <- # All deaths we've categorized as drug-related, by year and age
  df |> 
  group_by(period, age) |> 
  summarise(count = sum(count))

colnames(period_age_all_cause) <- c("period", "age", "deaths_all_causes")

colnames(period_age_drd) <- c("period", "age", "deaths_related_to_drug_misuse")


data <-  
left_join(period_age_all_cause, period_age_drd) |> 
  mutate(deaths_related_to_drug_misuse = zoo::na.fill(deaths_related_to_drug_misuse, 0))


data |> 
  filter(age < 50) |> 
  mutate(deaths_all_causes = deaths_all_causes - deaths_related_to_drug_misuse) |> 
  pivot_longer(cols = deaths_all_causes:deaths_related_to_drug_misuse) |> 
  ggplot(aes(x = age, y = value)) + 
  geom_col(aes(fill = name), position = "fill") + 
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~period)

data |> 
  mutate(p_dr = deaths_related_to_drug_misuse / deaths_all_causes) |>  
  ggplot(aes(x = age, y = p_dr)) + 
  geom_col(aes(fill = p_dr)) + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~period) + 
  theme_dark() +
  scale_x_continuous(position = "top", n.breaks = 20) +
  theme(
    strip.placement = "outside",
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.justification = "left"
    ) + 
  viridis::scale_fill_viridis() + 
  labs(fill = NULL)

  data |> 
    mutate(p_dr = deaths_related_to_drug_misuse / deaths_all_causes) |>
    filter(age < 55) |> 
    arrange(-p_dr)
  