library(showtext)   
library(ggsci)     
library(testthat) 
library(tidyverse)
library(arrow)   
library(flextable)

# Define file paths
raw_data_path <- "data/raw/ndtms_mortality_data.parquet"
tx_deaths_path <- "data/raw/tx_deaths_la_2122_2223.parquet"
processed_data_path <- "data/processed/drug_deaths_national.csv"
plot_output_path <- "plots/drugs_additional_deaths_broad_cat.png"

# Function to load data
load_data <- function(file_path) {
  read_parquet(file_path)
}

# Function to process mortality data
process_mortality_data <- function(data) {
  data %>%
    mutate(
      ndtms_match = if_else(
        Treatment_Status == "no match with NDTMS",
        "Record of contact with treatment system",
        "No record of contact with treatment system"
      ),
      ons_misuse = if_else(
        drug_misuse_combined == 1,
        "Recorded as drug misuse by ONS",
        "Not recorded as drug misuse by ONS"
      )
    ) %>%
    filter(drug_group == "Total Deaths", reg_year > 2022) %>%
    group_by(reg_year, ndtms_match, ons_misuse) %>%
    summarise(n = n(), .groups = "drop")
}

# Function to create summary table
create_summary_table <- function(data) {
  data %>%
    pivot_wider(names_from = ndtms_match, values_from = n) %>%
    select(-reg_year) %>%
    janitor::adorn_totals("col") %>%
    flextable(cwidth = 1.5) %>%
    theme_booktabs() %>%
    set_header_labels(values = c(ons_misuse = ""))
}

# Function to prepare poisoning deaths data
prepare_poisoning_deaths <- function(data) {
  data %>%
    filter(
      drug_misuse_combined == 1 | 
        (drug_misuse_combined == 0 & Treatment_Status == "no match with NDTMS")
    ) %>%
    mutate(
      additional_poisoning_death = if_else(
        drug_misuse_combined == 1, 
        "Initial poisoning deaths", 
        "Additional poisoning deaths"
      )
    )
}

# Function to process poisoning deaths data
process_poisoning_deaths <- function(data) {
  data %>%
    filter(drug_group == "Total Deaths", reg_year %in% c(2022, 2023)) %>%
    mutate(
      ons_misuse = if_else(
        drug_misuse_combined == 1,
        "Recorded as drug misuse by ONS",
        "Not recorded as drug misuse by ONS"
      ),
      additional_poisoning_deaths = if_else(
        drug_misuse_combined == 1, 
        "Initial poisoning deaths", 
        "Additional poisoning deaths"
      ),
      sex = case_when(
        sex == "M" ~ "Male",
        sex == "F" ~ "Female",
        TRUE ~ sex
      )
    ) %>%
    group_by(
      reg_year, DAT, DAT_NM, ageinyrs, sex, Treatment_Status, 
      additional_poisoning_deaths
    ) %>%
    summarise(count = n(), .groups = "drop")
}


# Function to process treatment deaths data
process_treatment_deaths <- function(file_path) {
  read_parquet(file_path) %>%
    mutate(period = as.integer(str_extract(period_range, "\\d{4}")) + 1) %>%
    filter(
      !death_cause %in% c("Drug poisoning", "Alcohol-specific death"),
      drug_group != "alcohol only"
    ) %>%
    select(-geography, -period_range)
}

# Function to save processed treatment deaths data
save_treatment_deaths <- function(data, file_path) {
  data %>%
    group_by(period, area_code, area_name) %>%
    select(-age) %>%
    summarise(across(where(is.numeric), sum), .groups = "drop") %>%
    write_csv(file_path)
}

# Function to create national summary data
create_national_summary <- function(txd_data, pd_data) {
  txd_national <- txd_data %>%
    group_by(period, age, sex, treatment_status, death_cause) %>%
    summarise(count = sum(count), .groups = "drop")
  
  pd_national <- pd_data %>%
    group_by(reg_year, ageinyrs, sex, additional_poisoning_deaths) %>%
    summarise(count = sum(count), .groups = "drop")
  
  # Align column names
  colnames(pd_national) <- colnames(txd_national)
  print(head(pd_national));print(head(txd_national))
  # Combine datasets
  bind_rows(txd_national, pd_national) %>%
    mutate(
      treatment_status = if_else(
        is.na(treatment_status), 
        "Poisoning w/ drug misuse", 
        treatment_status
      )
    )
}

processed_poisoning_deaths
treatment_deaths_data

# Function to save national summary data
save_national_summary <- function(data, file_path) {
  if (!file.exists(file_path)) {
    write_csv(data, file_path)
  }
}

# Function to create death cause table
create_death_cause_table <- function(data) {
  data %>%
    group_by(period, treatment_status, death_cause) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    pivot_wider(
      names_from = period, 
      values_from = count, 
      values_fill = 0
    ) %>%
    mutate(
      treatment_status = factor(
        treatment_status,
        levels = c(
          "Died in treatment", 
          "Died within a year of discharge", 
          "Died one or more years following discharge", 
          "Poisoning w/ drug misuse"
        )
      )
    ) %>%
    arrange(treatment_status, desc(`2023`)) %>%
    write_csv("data/processed/death_cause_table.csv")
}

# Function to prepare data for plotting
prepare_plot_data <- function(data) {
  data %>%
    mutate(
      death_category = case_when(
        str_detect(death_cause, "poisoning") ~ death_cause,
        TRUE ~ paste("Non-poisoning deaths:", treatment_status)
      )
    ) %>%
    group_by(period, death_category) %>%
    summarise(count = sum(count), .groups = "drop") %>%
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

# Function to create the plot
create_death_plot <- function(plot_data) {
  ggplot(plot_data, aes(x = period, y = count)) + 
    geom_col(
      aes(
        fill = death_category, 
        alpha = death_category, 
        linetype = death_category
      ), 
      colour = "black", 
      width = 0.5
    ) +
    geom_text(
      aes(
        label = scales::comma(count), 
        group = death_category, 
        alpha = death_category
      ), 
      position = position_stack(0.5)
    ) + 
    scale_fill_lancet(alpha = 0.8) + 
    theme_bw() + 
    theme(
      legend.position = "none",
      axis.ticks.x = element_blank(),
      panel.grid = element_blank()
    ) +
    scale_y_continuous(
      labels = scales::comma, 
      breaks = seq(2500, 10000, 2500)
    ) + 
    scale_x_continuous(
      limits = c(2021, 2024), 
      breaks = c(2022, 2023)
    ) +
    scale_alpha_manual(values = c(
      "Initial poisoning deaths" = 1,
      "Additional poisoning deaths" = 1,
      "Non-poisoning deaths: Died in treatment" = 1,
      "Non-poisoning deaths: Died within a year of discharge" = 1,
      "Non-poisoning deaths: Died one or more years following discharge" = 0.3
    )) +
    scale_linetype_manual(values = c(
      "Initial poisoning deaths" = 1,
      "Additional poisoning deaths" = 1,
      "Non-poisoning deaths: Died in treatment" = 1,
      "Non-poisoning deaths: Died within a year of discharge" = 1,
      "Non-poisoning deaths: Died one or more years following discharge" = 3
    )) + 
    labs(
      fill = NULL, 
      x = NULL, 
      y = "Count of deaths", 
      title = "Deaths Related to Drug Misuse"
    )
}

# Function to add annotations to the plot
add_plot_annotations <- function(plot, data) {
  # Filter data for the latest year
  latest_year <- max(data$period)
  annotation_data <- data %>%
    filter(period == latest_year) %>%
    arrange(death_category) %>%
    mutate(
      cumulative_count = cumsum(count),
      y_start = lag(cumulative_count, default = 0),
      y_end = cumulative_count,
      y_mid = (y_start + y_end) / 2
    )
  
  # Create a mapping of death categories to labels
  labels <- c(
    "Initial poisoning deaths" = "Drug poisoning deaths related to drug misuse\nas classified in ONS data",
    "Additional poisoning deaths" = "Drug poisoning deaths in treatment or within a year of leaving\nbut not classified as related to drug misuse by ONS",
    "Non-poisoning deaths: Died in treatment" = "Deaths in treatment with a cause other than poisoning",
    "Non-poisoning deaths: Died within a year of discharge" = "Deaths within a year of leaving treatment with a cause\nother than poisoning",
    "Non-poisoning deaths: Died one or more years following discharge" = "Deaths a year or more after leaving treatment with a cause\nother than poisoning"
  )
  
  # Add annotations
  for (i in seq_len(nrow(annotation_data))) {
    row <- annotation_data[i, ]
    color <- ifelse(
      row$death_category == "Non-poisoning deaths: Died one or more years following discharge", 
      "darkgrey", 
      "black"
    )
    plot <- plot +
      geom_segment(
        x = latest_year + 0.1, 
        xend = latest_year + 0.1, 
        y = row$y_start, 
        yend = row$y_end,
        arrow = arrow(
          ends = "both", 
          angle = 90, 
          length = unit(0.2, "cm")
        ), 
        colour = color
      ) +
      annotate(
        "text",
        x = latest_year + 0.2,
        y = row$y_mid,
        label = labels[[row$death_category]],
        hjust = 0,
        colour = color
      )
  }
  return(plot)
}

# Main execution

# Load and process mortality data
mortality_data <- load_data(raw_data_path)
demo_data <- process_mortality_data(mortality_data)
summary_table <- create_summary_table(demo_data)

# Prepare poisoning deaths data
poisoning_deaths_data <- prepare_poisoning_deaths(mortality_data)
processed_poisoning_deaths <- process_poisoning_deaths(poisoning_deaths_data)



# Process treatment deaths data
treatment_deaths_data <- process_treatment_deaths(tx_deaths_path)

treatment_deaths_data
processed_poisoning_deaths
# Create national summary data
national_summary <- create_national_summary(
  treatment_deaths_data, 
  processed_poisoning_deaths
)
save_national_summary(national_summary, processed_data_path)

# Create death cause table
create_death_cause_table(national_summary)

# Prepare data for plotting
plot_data <- prepare_plot_data(national_summary)

# Create and annotate the plot
death_plot <- create_death_plot(plot_data)
death_plot_annotated <- add_plot_annotations(death_plot, plot_data)

death_plot_annotated

# Save the plot
ggsave(
  filename = plot_output_path, 
  plot = death_plot_annotated, 
  width = 30, 
  height = 20, 
  units = "cm", 
  dpi = 200
)
