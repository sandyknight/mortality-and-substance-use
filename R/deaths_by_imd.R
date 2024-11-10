library(openxlsx)
library(fedmatch)
library(tidyverse)

url <- 
  "https://assets.publishing.service.gov.uk/media/5d8b3d7aed915d0369518030/File_11_-_IoD2019_Local_Authority_District_Summaries__upper-tier__.xlsx"

# TODO again need to this on a laptop with Excel...

read.xlsx(url)

