library(arrow)
library(testthat)
library(tidyverse)
library(fedmatch)

rgx_rm <- # Patterns to remove
  "\\.| UA|, County of|, City of"

rplc <- # Function to replace patterns
function(x){
case_when(
str_detect(x, "-On-") ~ str_replace(x, "-On-", "-on-"),
str_detect(x, "Scily") ~ str_replace(x, "Scily", "Scilly"),
TRUE ~ x
)
}

gpd <- # Geography columns of poisoning data
  read_parquet("data/raw/ndtms_mortality_data.parquet") %>% 
  janitor::clean_names() %>% 
  select(reg_year, dat, dat_nm, regdatrs_nm) %>% 
  filter(reg_year > 2021) %>% 
  mutate(dat_nm = str_remove(dat_nm, rgx_rm)) %>% 
  mutate(dat_nm = rplc(dat_nm)) %>% 
  unique()


gtxd <- # Geography columns of treat deaths data
  read_parquet("data/raw/tx_deaths_la_2122_2223.parquet") %>% 
  mutate(period = as.integer(str_extract(period_range, pattern =  "\\d{4}")) + 1) %>% 
  select(period, area_code, area_name) %>% 
  mutate(area_name = str_remove(area_name, rgx_rm)) %>% 
  mutate(area_name = rplc(area_name)) %>% 
  unique()

gtxd <- 
gtxd %>% 
  mutate(gss_name = area_name)

full_join(gpd, gtxd, by = c("reg_year" = "period", "dat_nm" = "area_name")) %>% 
  filter(!if_any(c(area_code, dat), is.na)) %>% 
  write_csv("data/processed/geography_match.csv")


geo_match <-
  read_csv("data/processed/geography_match.csv") 

l <- nrow(geo_match)

geo_match <- 
  geo_match %>% 
  mutate(FID = seq(1, l, 1))

txd <-
  read_csv("data/processed/tx_deaths_la.csv")

txd <-
  txd %>%
  mutate(uid = paste0(period, area_code))

pd <-
  read_csv("data/processed/drug_poisoning_deaths_misuse_la.csv")

pd <-
  pd %>%
  mutate(uid = paste0(reg_year, dat))

fedmatch::merge_plus(
  data1 = txd,
  data2 = geo_match,
  by = "area_code",
  unique_key_1 = "uid" ,
  unique_key_2 = "FID"
)

fedmatch::merge_plus(
  data1 = pd,
  data2 = geo_match,
  by = "dat",
  unique_key_1 = "uid" ,
  unique_key_2 = "FID"
)

