library(data.table)
library(tidyverse)
library(openxlsx)
library(curl)

# IMD ---------------------------------------------------------------------

url <- # UTLA IMD summaries URL
  "https://assets.publishing.service.gov.uk/media/5d8b3d7aed915d0369518030/File_11_-_IoD2019_Local_Authority_District_Summaries__upper-tier__.xlsx"


imd <- 
read.xlsx(xlsxFile = url, sheet = "IMD") |> 
  janitor::clean_names()


# This file is NDTMS-ONS data linkage; recieved by email from JK

sht <- # Identify the actual data sheet
  openxlsx::getSheetNames("data/raw/table1_all deaths_2022_updated.xlsx")[3]

df <- openxlsx::read.xlsx("data/raw/table1_all deaths_2022_updated.xlsx", sheet = sht)


