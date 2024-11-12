library(tidyverse)    # Essential data manipulation tools
library(arrow)        # For handling parquet files



drug_poisoning_deaths_file <- 
  "data/raw/ndtms_mortality_data.parquet"
deaths_in_treatment_file <- 
  "data/raw/tx_deaths_la_2122_2223.parquet"

process_poisoning_data <- function(file_path, date_of = "occurence", years = c(2022, 2023), by = NULL) {
  
  date_of_var <- switch(date_of,
                        "occurence" = "dod_year",
                        "registration" = "reg_year",
                        stop("Only 'occurence' or 'registration'")
  )
  
  if(!is.null(by)){
  by_var <- switch(by,
                        "area" = c("dat", "dat_nm"),
                        "age" = "ageinyrs"
  )} else {
    by_var <- NULL
  }
  
  read_parquet(file_path) %>%
    janitor::clean_names() %>% 
    mutate(
      ndtms_match = if_else(
        treatment_status == "no match with NDTMS",
        "No record of contact with treatment system",
        "Record of contact with treatment system"
      ),
      ons_misuse = if_else(
        drug_misuse_combined == 1,
        "Recorded as drug misuse by ONS",
        "Not recorded as drug misuse by ONS"
      )
    ) %>%
    filter(
      drug_group == "Total Deaths",
      .data[[date_of_var]] %in% years
    ) %>% 
    filter(
      drug_misuse_combined == 1 | 
        (drug_misuse_combined == 0 & treatment_status == "no match with NDTMS")
    ) %>%
    mutate(
      additional_poisoning_deaths = if_else(
        drug_misuse_combined == 1, 
        "Initial poisoning deaths", 
        "Additional poisoning deaths"
      )
    ) %>% 
    group_by(pick(by_var), pick(date_of_var), additional_poisoning_deaths) %>% 
    summarise(count = n(), .groups = "drop")
}


process_deaths_in_treatment <- function(file_path,
                                        years = c(2022, 2023),
                                        by = NULL,
                                        by_treatment_status = FALSE,
                                        by_death_cause = FALSE,
                                        exclude_poisoning = TRUE) {
  # Initialize grouping variables with 'period'
  grouping_vars <- "period"
  
  # Handle 'by' parameter
  if (!is.null(by)) {
    by_vars <- switch(by,
                      "area" = c("area_code", "area_name"),
                      "age" = "age",
                      stop("Invalid 'by' value. Choose 'area' or 'age', or leave as NULL.")
    )
    grouping_vars <- c(grouping_vars, by_vars)
  }
  
  # Add 'treatment_status' to grouping variables if required
  if (isTRUE(by_treatment_status)) {
    grouping_vars <- c(grouping_vars, "treatment_status")
  }
  
  # Add 'death_cause' to grouping variables if required
  if (isTRUE(by_death_cause)) {
    grouping_vars <- c(grouping_vars, "death_cause")
  }
  
  # Read and process data
  data <- read_parquet(file_path) %>%
    mutate(
      period = as.Date(period, origin = "1899-12-30"),
      period = lubridate::year(period)
    )
  
  # Filter data based on 'years' and 'exclude_poisoning'
  data <- data %>%
    filter(period %in% years)
  
  if (isTRUE(exclude_poisoning)) {
    data <- data %>%
      filter(death_cause != "Drug poisoning")
  }
  
  # Group and summarize data
  result <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(count = sum(count), .groups = "drop")
  
  return(result)
}


process_deaths_in_treatment(file_path = deaths_in_treatment_file, by_death_cause = TRUE)
process_poisoning_data(file_path = drug_poisoning_deaths_file)

