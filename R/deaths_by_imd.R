library(fedmatch)
library(tidyverse)
library(openxlsx)



# IMD data ----------------------------------------------------------------

url <- # UTLA IMD summaries URL
  "https://assets.publishing.service.gov.uk/media/5d8b3d7aed915d0369518030/File_11_-_IoD2019_Local_Authority_District_Summaries__upper-tier__.xlsx"

imd <-
  read.xlsx(xlsxFile = url, sheet = "IMD") |>
  janitor::clean_names()

imd |> 
  select(upper_tier_local_authority_district_code_2019, upper_tier_local_authority_district_name_2019, imd_average_score) |>
  filter(upper_tier_local_authority_district_name_2019 != "")
  mutate(imd_decile = ntile(desc(imd_average_score), 10)) |> 
  
