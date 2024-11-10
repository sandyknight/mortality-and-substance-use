library(tidyverse)
library(openxlsx)
library(curl)

# Getting all cause deaths data, initially to see which age groups to use for 
# drug/alcohol deaths; then to compare. 

df <-
  read_csv("data/processed/drug_deaths_national.csv")


url <-
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2023/annualdeathregistrations2023.xlsx"

curl_download(
  url = url,
  destfile = "data/raw/ons_deaths_data_2023.xlsx"
  )

#TODO : need to this on work laptop with Excel

readxl::excel_sheets("data/raw/ons_deaths_data_2023.xlsx")
