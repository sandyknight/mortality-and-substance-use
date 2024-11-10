library(arrow)
library(testthat)
library(tidyverse)

rgx_rm <- "\\.| UA|, County of|, City of"

rplc <- 
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
  mutate(dat_nm = str_remove(dat_nm, rgx)) %>% 
  mutate(dat_nm = rplc(dat_nm)) %>% 
  unique()


gtxd <- # Geography columns of treat deaths data
  read_parquet("data/raw/tx_deaths_la_2122_2223.parquet") %>% 
  mutate(period = as.integer(str_extract(period_range, pattern =  "\\d{4}")) + 1) %>% 
  select(period, area_code, area_name) %>% 
  mutate(area_name = str_remove(area_name, rgx)) %>% 
  mutate(area_name = rplc(area_name)) %>% 
  unique()

gtxd <- 
gtxd %>% 
  mutate(gss_name = area_name)

# full_join(gpd, gtxd, by = c("reg_year" = "period", "dat_nm" = "area_name")) %>% View()


full_join(gpd, gtxd, by = c("reg_year" = "period", "dat_nm" = "area_name")) %>% 
  filter(if_any(c(area_code, dat), is.na))



vignette("colwise")
