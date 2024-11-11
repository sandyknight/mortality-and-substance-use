library(testthat)
library(tidyverse)
library(openxlsx)
library(arrow)






# NDTMS data linkage poisoning deaths -------------------------------------


if (!file.exists("data/raw/ndtms_mortality_data.parquet")){
  #  This file is NDTMS-ONS data linkage; received by email from Stefan and named:
  # "table1_all deaths_Cocaine version 1.xlsx"
  
  df <- # Load deaths data
    openxlsx::read.xlsx("data/raw/table1_all deaths_Cocaine version 1.xlsx", sheet = "table1_all deaths")
  
  write_parquet(df, "data/raw/ndtms_mortality_data.parquet")
  
}



# ONS published drug poisoning deaths -------------------------------------

url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningenglandandwalesreferencetable/current/2023registrations.xlsx"

ons_registrations <- openxlsx::read.xlsx(url, sheet = "Table 1", startRow = 4, sep.names = "_", fillMergedCells = TRUE, skipEmptyCols = TRUE, check.names = TRUE) |> 
  select(1,2,5,9)

colnames(ons_registrations) <- c("sex", "year", "all_drug_poisoning", "drug_misuse")

ons_registrations <- 
ons_registrations |> 
  slice(3:100) |> 
  mutate(sex = zoo::na.locf(sex)) |> 
  filter(sex == "Persons") |> 
  as_tibble() |> 
  filter(!is.na(year))

ons_registrations |> 
  write_csv("data/processed/published_ons_figures.csv")

# Non-poisoning deaths ----------------------------------------------------

# Data received by email from mailto Chioma.Amasiatu@dhsc.gov.uk, Mon 08/07/2024 10:28 and named `post election data for Jon- sent.xlsx`

tx_deaths <- 
  openxlsx::read.xlsx("data/raw/post election data for Jon- sent.xlsx", sheet = "NDTMS_ONS") |> 
  janitor::clean_names()

england_tx_deaths <- 
tx_deaths |> 
  filter(area_name == "England")

tx_deaths <- 
tx_deaths |> 
  filter(geography == "LA")

la_count <- 
tx_deaths |> 
  filter(period_range == "April 2022 to March 2023") |> 
  group_by(agegrp, death_cause) |> 
  summarise(count = sum(count)) |> 
  pivot_wider(names_from = agegrp, values_from = count, values_fill = 0)

england_count <- 
england_tx_deaths |> 
  filter(period_range == "April 2022 to March 2023") |> 
  group_by(agegrp, death_cause) |> 
  summarise(count = sum(count)) |> 
  pivot_wider(names_from = agegrp, values_from = count, values_fill = 0)


test_that(
 "Sum of LAs = England total",{ expect_equal(la_count, england_count)}
)

rm(la_count);rm(england_count)

tx_deaths %>% 
  write_parquet("data/raw/tx_deaths_la_2122_2223.parquet")

tx_deaths |> 
  filter(period_range == "April 2022 to March 2023") |> 
  filter(death_cause != "Drug poisoning") |> 
  filter(death_cause != "Alcohol-specific death") |> 
  filter(drug_group  != "alcohol only") |> 
  filter(treatment_status != "Died one or more years following discharge") |> 
  as_tibble() |> 
  group_by(area_code, area_name, death_cause, age, agegrp, drug_group, treatment_status) |> 
  summarise(count = sum(count)) |> 
  pivot_wider(names_from = treatment_status, values_from = count, values_fill = 0) 










