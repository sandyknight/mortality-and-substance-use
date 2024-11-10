library(tidyverse)
library(fedmatch)


geo_match <- 
  read_csv("data/processed/geography_match.csv") |> 
  mutate(FID = seq(1, 300, 1))

txd <- 
  read_csv("data/processed/tx_deaths_la.csv")


pd <- 
  read_csv("data/processed/drug_poisoning_deaths_misuse_la.csv")


fedmatch::merge_plus(data1 = txd, data2 = geo_match, by = c("area_name", "area_code"), unique_key_1 = c("period", "area_code"), unique_key_2 = "FID")