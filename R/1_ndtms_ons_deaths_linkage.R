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


# This file is NDTMS-ONS data linkage; received by email from Stefan 

df <- # Load deaths data
  openxlsx::read.xlsx("data/raw/table1_all deaths_Cocaine version 1.xlsx", sheet = "table1_all deaths")

dt <-
  as.data.table(df)

dt <- # Deaths from drug misuse & suspected deaths from drug misuse but not recorded as such
  dt[(drug_misuse_combined == 0 & Treatment_Status != "no match with NDTMS")| drug_misuse_combined == 1,]


  

