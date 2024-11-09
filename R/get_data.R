library(testthat)
library(tidyverse)
library(openxlsx)
library(arrow)




# IMD ---------------------------------------------------------------------

url <- # UTLA IMD summaries URL
  "https://assets.publishing.service.gov.uk/media/5d8b3d7aed915d0369518030/File_11_-_IoD2019_Local_Authority_District_Summaries__upper-tier__.xlsx"

imd <-
read.xlsx(xlsxFile = url, sheet = "IMD") |>
  janitor::clean_names()

imd |> 
  select(upper_tier_local_authority_district_code_2019, upper_tier_local_authority_district_name_2019, imd_average_score) |>
  mutate(imd_decile = ntile(imd_average_score, 10)) |> 
  write_csv("data/raw/utla_imd_decile.csv")


# Drug poisoning deaths ---------------------------------------------------

if (!file.exists("data/raw/ndtms_mortality_data.parquet")){
  #  This file is NDTMS-ONS data linkage; received by email from Stefan and named:
  # "table1_all deaths_Cocaine version 1.xlsx"
  
  df <- # Load deaths data
    openxlsx::read.xlsx("data/raw/table1_all deaths_Cocaine version 1.xlsx", sheet = "table1_all deaths")
  
  write_parquet(df, "data/raw/ndtms_mortality_data.parquet")
  
}


# dt <- # Deaths from drug misuse & suspected deaths from drug misuse but not recorded as such
#   dt[(drug_misuse_combined == 0 & Treatment_Status != "no match with NDTMS")| drug_misuse_combined == 1,]
# 
# write_parquet(dt, "data/raw/deaths_related_to_misuse_inc_not_rec.parquet")


# poisoning_deaths_ons <- read_parquet("data/raw/ndtms_mortality_data.parquet")
# 
# poisoning_deaths_ons |> 
#   mutate(ndtms_match = if_else(Treatment_Status == "no match with NDTMS", "Record of contact with treatment system", "No record of contact with treatment system")) |> 
#   filter(drug_group == "Total Deaths") |> 
#   mutate(ons_misuse = if_else(drug_misuse_combined == 1, "Recorded as drug misuse by ONS", "Not recorded as drug misuse by ONS")) |> 
#   group_by(reg_year, ndtms_match, ons_misuse) |> 
#   summarise(n = n()) |> 
#   filter(reg_year > 2022) |> 
#   pivot_wider(names_from = ndtms_match, values_from = n) |> 
#   ungroup() |> 
#   select(-reg_year) |> 
#   janitor::adorn_totals(where = c("row", "col"))



# Non-poisoning deaths ----------------------------------------------------

# Data received by email from mailto:Chioma.Amasiatu@dhsc.gov.uk, Mon 08/07/2024 10:28 and named `post election data for Jon- sent.xlsx`

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










