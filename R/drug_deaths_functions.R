library(tidyverse)    
library(arrow)       




process_poisoning_data <- function(file_path, date_of = "occurence", years = c(2022, 2023), by = NULL) {
  if (is.null(by)){message(paste("Drug poisoning: national level data, all ages", years, sep = ", "))}
  # Determine the date variable based on 'date_of' parameter
  date_of_var <- switch(
    date_of,
    "occurence" = "dod_year",
    "registration" = "reg_year",
    stop("Only 'occurence' or 'registration'")
  )
  
  # Determine grouping variables based on 'by' parameter
  if (!is.null(by)) {
    by_var <- switch(
      by,
      "area" = c("dat", "dat_nm"),
      "age" = "ageinyrs"
    )
  } else {
    by_var <- NULL
  }
  
  read_parquet(file_path) %>%
    janitor::clean_names() %>%
    mutate(
      # Create 'ndtms_match' variable based on 'treatment_status'
      ndtms_match = if_else(
        treatment_status == "no match with NDTMS",
        "No record of contact with treatment system",    # If no match with NDTMS
        "Record of contact with treatment system"        # Else, record of contact
      ),
      # Create 'ons_misuse' variable based on 'drug_misuse_combined'
      ons_misuse = if_else(
        drug_misuse_combined == 1,                       # If flag is 1
        "Recorded as drug misuse by ONS",                # Then recorded as drug misuse
        "Not recorded as drug misuse by ONS"
      )
    ) %>%
    filter(
      drug_group == "Total Deaths",                      # Get total deaths (not by substance group)
      .data[[date_of_var]] %in% years                    # Filter by date variable and years
    ) %>%
    filter(
      drug_misuse_combined == 1 |                        # Keep records where misuse flag is 1
        (drug_misuse_combined == 0 & treatment_status == "no match with NDTMS")  # Or flag is 0 but no treatment record
    ) %>%
    mutate(
      # Create 'additional_poisoning_deaths' variable
      additional_poisoning_deaths = if_else(
        drug_misuse_combined == 1,
        "Initial poisoning deaths",
        "Additional poisoning deaths"
      )
    ) %>%
    group_by(
      pick(by_var),
      pick(date_of_var),
      additional_poisoning_deaths
    ) %>%
    summarise(
      count = n(),
      .groups = "drop"
    )

}


process_deaths_in_treatment <- function(file_path,
                                        years = c(2022, 2023),
                                        by = NULL,
                                        by_treatment_status = FALSE,
                                        by_death_cause = FALSE,
                                        exclude_poisoning = TRUE,
                                        exclude_alcohol_specific_deaths = TRUE
                                        ) {
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
  
  if (isTRUE(exclude_alcohol_specific_deaths)) {
    data <- data %>%
      filter(death_cause != "Alcohol-specific death")
  }
  
  # Group and summarize data
  result <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(count = sum(count), .groups = "drop")
  
  return(result)
}


combine_national_data <- # Combine data at national level, only grouped by death category
  function(poisoning_data, treatment_deaths_data){
    bind_rows(
      rename(poisoning_data, "death_category" = additional_poisoning_deaths) |>
        rename_with(.cols = 1, .fn = ~ str_remove(.x, "dod_|reg_")),
      rename(treatment_deaths_data, "death_category" = treatment_status, "year" = period)
    )
  }

relabel_national_data <-
  function(national_data){
    national_data |> 
      mutate(death_category = case_when(
        str_detect(death_category, "poisoning") ~ death_category,
        TRUE ~ paste("Non-poisoning deaths:", death_category)
      )) |> 
      mutate(
        death_category = factor(
          death_category,
          levels = rev(c(
            "Initial poisoning deaths",
            "Additional poisoning deaths",
            "Non-poisoning deaths: Died in treatment",
            "Non-poisoning deaths: Died within a year of discharge",
            "Non-poisoning deaths: Died one or more years following discharge"
          )),
          ordered = TRUE
        )
      )
  }


