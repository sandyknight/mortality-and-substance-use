library(tidyverse)    
library(arrow)       

#' Process drug poisoning data
#' 
#' Reads, cleans, and processes drug poisoning data from a file. Filters and groups the data 
#' based on user-defined parameters, such as years, date type, and grouping.
#'
#' @param file_path Path to the parquet file containing drug poisoning data.
#' @param date_of Either "occurrence" (year of death) or "registration" (year of registration).
#' @param years Vector of years to include in the analysis.
#' @param by Grouping variable: "area", "age", or NULL (national-level data).
#' @param by_sex Whether to group by sex 
#' @return A tibble with grouped and summarized poisoning data.
process_poisoning_data <- function(file_path, date_of = "occurrence", years = c(2022, 2023), by = NULL, by_sex = FALSE) {
  if (is.null(by)) {
    message(paste("Drug poisoning: national level data, all ages", years, sep = ", "))
  }
  # Decide which date variable to use
  date_of_var <- switch(
    date_of,
    "occurrence" = "dod_year",
    "registration" = "reg_year",
    stop("Only 'occurrence' or 'registration' are valid options!")
  )
  
  # Handle grouping variable(s)
  if (!is.null(by)) {
    by_var <- switch(
      by,
      "area" = c("dat", "dat_nm"),
      "age" = "ageinyrs",
      stop("Invalid 'by' value. Use 'area', 'age', or leave as NULL.")
    )
  } else {
    by_var <- NULL
  }
 
 if (isTRUE(by_sex)){
   by_vars <- c(by_var, "sex")
 } else {
   by_vars <- by_var
 }
   
  # Read and process data
  result <- 
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
      # 'Total Deaths' only, not by substance. NB the substance groups are not mutually exclusive
      drug_group == "Total Deaths", 
      # Year of occurrence OR registration as selected in the parameters
      .data[[date_of_var]] %in% years 
    ) %>%
    filter(
      # Retain rows that were recoreded as drug misuse OR not recorded as drug misuse but have a record of contact with the treatment system
      drug_misuse_combined == 1 | (drug_misuse_combined == 0 & treatment_status != "no match with NDTMS") 
    ) %>%
    mutate(
      additional_poisoning_deaths = if_else(
        # Where drug_misuse_combined equals 1
        drug_misuse_combined == 1,
        "Initial poisoning deaths",
        "Additional poisoning deaths"
      )
    ) %>%
    group_by(
      !!!syms(by_vars),
      !!!syms(date_of_var),
      additional_poisoning_deaths
    ) %>%
    summarise(
      count = n(),
      .groups = "drop"
    )
 
    if (isTRUE(by_sex)) {
    result <- 
      result |> 
      mutate(sex = case_match(sex,"M" ~ "male", "F" ~ "female"))
    }
  
  return(result)
   
}

#' Process deaths in treatment data
#' 
#' Reads and processes data about deaths in treatment. Filters, groups, and summarizes the data
#' based on various parameters.
#'
#' @param file_path Path to the parquet file containing treatment data.
#' @param years Vector of years to include in the analysis.
#' @param by Grouping variable: "area", "age", or NULL.
#' @param by_treatment_status Whether to group by treatment status.
#' @param by_death_cause Whether to group by cause of death.
#' @param by_sex Whether to group by sex 
#' @param exclude_poisoning Whether to exclude drug poisoning deaths.
#' @param exclude_alcohol_specific_deaths Whether to exclude alcohol-specific deaths.
#' @return A tibble with grouped and summarized treatment data.
process_deaths_in_treatment <- function(file_path,
                                        years = c(2022, 2023),
                                        by = NULL,
                                        by_treatment_status = FALSE,
                                        by_death_cause = FALSE,
                                        by_sex = FALSE,
                                        exclude_poisoning = TRUE,
                                        exclude_alcohol_specific_deaths = TRUE
) {
  # Start with grouping variable
  grouping_vars <- "period"
  
  # Handle additional grouping by 'by' parameter
  if (!is.null(by)) {
    by_vars <- switch(by,
                      "area" = c("area_code", "area_name"),
                      "age" = "age",
                      stop("Invalid 'by' value. Choose 'area' or 'age', or leave as NULL.")
    )
    grouping_vars <- c(grouping_vars, by_vars)
  }
  
  # Add grouping for treatment status or death cause if requested
  if (isTRUE(by_treatment_status)) {
    grouping_vars <- c(grouping_vars, "treatment_status")
  }
  
  if (isTRUE(by_death_cause)) {
    grouping_vars <- c(grouping_vars, "death_cause")
  }

 if (isTRUE(by_sex)) {
    grouping_vars <- c(grouping_vars, "sex")
  }
  
    
  # Read and process data
  data <- read_parquet(file_path) %>%
    mutate(
      period = as.Date(period, origin = "1899-12-30"),
      period = lubridate::year(period)
    )
  
  # Apply filters
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
  
  # Group and summarize
  result <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(count = sum(count), .groups = "drop")

  # If grouped by sex column, standardise sex coding  
   
  if (isTRUE(by_sex)) {
    result <- 
    result |> 
      mutate(sex = tolower(sex))
  }
  
   
  return(result)
}

#' Combine national data
#' 
#' Merges poisoning data and treatment death data, ensuring consistent grouping by death category.
#'
#' @param poisoning_data Processed poisoning data.
#' @param treatment_deaths_data Processed treatment death data.
#' @return A tibble combining both datasets.
combine_national_data <- function(poisoning_data, treatment_deaths_data) {
  bind_rows(
    rename(poisoning_data, "death_category" = additional_poisoning_deaths) |>
      rename_with(.cols = 1, .fn = ~ str_remove(.x, "dod_|reg_")),
    rename(treatment_deaths_data, "death_category" = treatment_status, "year" = period)
  )
}

#' Relabel national data
#' 
#' Adjusts labels for death categories, emphasizing poisoning and non-poisoning distinctions.
#'
#' @param national_data Combined national dataset.
#' @return A tibble with updated and ordered death category labels.
relabel_national_data <- function(national_data) {
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


partially_process_poisoning_data <- # Partially process data to demonstrate method of IDing additonal drug poisoning deaths
  function(file_path, date_of = "occurence", years = c(2022, 2023), by = NULL) {
  if (is.null(by)) {
    message(paste("Drug poisoning: national level data, all ages", years, sep = ", "))
  }
  # Decide which date variable to use
  date_of_var <- switch(
    date_of,
    "occurence" = "dod_year",
    "registration" = "reg_year",
    stop("Only 'occurence' or 'registration' are valid options!")
  )
  
  # Handle grouping variable(s)
  if (!is.null(by)) {
    by_var <- switch(
      by,
      "area" = c("dat", "dat_nm"),
      "age" = "ageinyrs",
      stop("Invalid 'by' value. Use 'area', 'age', or leave NULL.")
    )
  } else {
    by_var <- NULL
  }
  
  # Read and process data
  # Read and process data
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
      # 'Total Deaths' only, not by substance. NB the substance groups are not mutually exclusive
      drug_group == "Total Deaths", 
      # Year of occurrence OR registration as selected in the parameters
      .data[[date_of_var]] %in% years 
    ) %>%
    # filter(
    #   # Retain rows that were recoreded as drug misuse OR not recorded as drug misuse but have a record of contact with the treatment system
    #   drug_misuse_combined == 1 | (drug_misuse_combined == 0 & treatment_status != "no match with NDTMS") 
    # ) %>%
    mutate(
      additional_poisoning_deaths = if_else(
        # Where drug_misuse_combined equals 1, "Initial"
        drug_misuse_combined == 1,
        "Initial poisoning deaths",
        "Additional poisoning deaths"
      )) |> 
    select(ndtms_match:ons_misuse) |> 
    group_by(pick(everything())) |> 
    summarise(count = n(), .groups = "drop") 
}
