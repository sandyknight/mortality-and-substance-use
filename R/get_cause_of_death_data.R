library(openxlsx)
library(tidyverse)
 

url <-
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2023/annualdeathregistrations2023.xlsx"

df <- read.xlsx(url, sheet = 'Table_4', rows = c(5:3714)) 

df |> head() |> janitor::clean_names() |> 
  select(leading_cause_of_death) |> 
  unique()
