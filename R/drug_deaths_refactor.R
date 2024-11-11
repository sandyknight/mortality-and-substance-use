# Load necessary libraries for processing, plotting, and table generation
library(showtext)
library(ggsci)
library(testthat)
library(tidyverse)
library(arrow)
library(flextable)

# Function to load data
load_data <- function(file_path) {
  read_parquet(file_path)
}

# Function to process demo data for treatment and misuse categories
process_demo_data <- function(data) {
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
    summarise(n = n(), .groups = 'drop') %>%
    collect() %>%
    pivot_wider(names_from = ndtms_match, values_from = n) %>%
    janitor::adorn_totals("col") %>%
    flextable::flextable(cwidth = 1.5) %>%
    theme_booktabs() %>%
    set_header_labels(values = c(ons_misuse = ""))
}

# Function to prepare and summarize deaths for local authority and national level
prepare_death_data <- function(data, reg_years = c(2022, 2023)) {
  data %>%
    filter(drug_group == "Total Deaths", reg_year %in% reg_years) %>%
    mutate(
      ons_misuse = if_else(
        drug_misuse_combined == 1,
        "Recorded as drug misuse by ONS",
        "Not recorded as drug misuse by ONS"
      ),
      additional_poisoning_deaths = if_else(drug_misuse_combined == 1, "Initial poisoning deaths", "Additional poisoning deaths")
    ) %>%
    group_by(reg_year, DAT, DAT_NM, ageinyrs, sex, Treatment_Status, additional_poisoning_deaths) %>%
    tally(name = "count") %>%
    janitor::clean_names() %>%
    ungroup()
}

# Function to create national summary of drug-related deaths by category
create_national_summary <- function(tx_data, pd_data) {
  txd_national <- tx_data %>%
    group_by(period, age, sex, treatment_status, death_cause) %>%
    summarise(count = sum(count), .groups = 'drop')
  
  pd_national <- pd_data %>%
    group_by(reg_year, ageinyrs, sex, additional_poisoning_deaths) %>%
    summarise(count = sum(count), .groups = 'drop')
  
  # Standardize column names
  colnames(pd_national) <- grep(pattern = "treatment_status", colnames(txd_national), perl = TRUE, value = TRUE, invert = TRUE)
  
  # Bind data together
  bind_rows(txd_national, pd_national) %>%
    mutate(
      treatment_status = if_else(is.na(treatment_status), "Poisoning w/ drug misuse", treatment_status)
    )
}

# Function to save processed data
save_processed_data <- function(data, file_path) {
  if (!file.exists(file_path)) {
    write_csv(data, file_path)
  }
}

# Load data
df <- load_data("data/raw/ndtms_mortality_data.parquet")

# Process and display the demo summary table
demo_summary <- process_demo_data(df)

# Filter and process for specific drug misuse or treatment match data
death_data <- prepare_death_data(df)

# Save summarized data at the local authority level
save_processed_data(death_data, "drug_poisoning_deaths_misuse_la.csv")

# Load additional data for treatment-related deaths
tx_death_data <- load_data("data/raw/tx_deaths_la_2122_2223.parquet") %>%
  mutate(period = as.integer(str_extract(period_range, pattern =  "\\d{4}")) + 1) %>%
  filter(
    death_cause != "Drug poisoning",
    death_cause != "Alcohol-specific death",
    drug_group != "alcohol only"
  ) %>%
  select(-geography, -period_range)

# Save summarized local authority death data
save_processed_data(tx_death_data, "data/processed/tx_deaths_la.csv")

# Create a national summary dataset and save it
drug_deaths_national <- create_national_summary(tx_death_data, death_data)
save_processed_data(drug_deaths_national, "data/processed/drug_deaths_national.csv")

# Function to plot drug-related death categories
plot_death_categories <- function(data) {
  data %>%
    mutate(death_category = case_when(
      str_detect(death_cause, "poisoning") ~ death_cause,
      TRUE ~ paste("Non-poisoning deaths", treatment_status, sep = ": ")
    )) %>%
    group_by(period, death_category) %>%
    summarise(count = sum(count), .groups = 'drop') %>%
    mutate(death_category = factor(death_category, levels = rev(c(
      "Initial poisoning deaths",
      "Additional poisoning deaths",
      "Non-poisoning deaths: Died in treatment",
      "Non-poisoning deaths: Died within a year of discharge",
      "Non-poisoning deaths: Died one or more years following discharge"
    )), ordered = TRUE)) %>%
    ggplot(aes(x = period, y = count, fill = death_category)) +
    geom_col(colour = "black", width = 0.5) +
    geom_text(aes(label = scales::comma(count)), position = position_stack(0.5)) +
    scale_fill_lancet() +
    theme_bw() +
    labs(title = "Deaths related to drug misuse", y = "Count of deaths") +
    theme(legend.position = "none") 
}

# Plot and save the figure
p1 <- plot_death_categories(drug_deaths_national)

all_except_morethan_year_count <- drug_deaths_national |> 
  filter(treatment_status != "Died one or more years following discharge") |> 
  group_by(period) |> 
  summarise(count = sum(count)) |> 
  filter(period == 2023) |> pull(count)

# Helper function for adding annotated segments to the plot
add_segment_annotation <- function(plot, x_start, y_start, y_end, x_text, y_text, label, color = "black") {
  plot +
    geom_segment(
      x = x_start, xend = x_start, y = y_start, yend = y_end,
      arrow = arrow(ends = "both", angle = 90, length = unit(.2, "cm")),
      colour = color
    ) +
    annotate(
      "text", x = x_text, y = y_text, label = label, hjust = 0, colour = color
    )
}

# Start with the main plot
p1_parts <- p1 +
  # Set x and y scales
  scale_x_continuous(breaks = c(2022, 2023), limits = c(2021, 2026)) +
  scale_y_continuous(breaks = c(2500, 5000, all_except_morethan_year_count, 7500, 10000), labels = scales::comma)

# FIXME : these annotations are placed on x-axis for the 2023 data but spaced on y-axis for 2022 data

# Adding annotated segments to the plot
p1_parts <- add_segment_annotation(
  p1_parts, x_start = 2023.4, y_start = 0, y_end = 3250, 
  x_text = 2023.5, y_text = 1668,
  label = "Drug poisoning deaths related to drug misuse\nas classified in ONS data"
)

p1_parts <- add_segment_annotation(
  p1_parts, x_start = 2023.4, y_start = 3336, y_end = 3250 + 936, 
  x_text = 2023.5, y_text = 3250 + (936 / 2),
  label = "Drug poisoning deaths in drug treatment or within a year of\nleaving treatment, but not classified as related to\ndrug misuse in ONS data"
)

p1_parts <- add_segment_annotation(
  p1_parts, x_start = 2023.4, y_start = 3336 + 930, y_end = 3250 + 936 + 1386, 
  x_text = 2023.5, y_text = 3250 + 936 + (1386 / 2),
  label = "Deaths in treatment with a cause other than poisoning"
)

p1_parts <- add_segment_annotation(
  p1_parts, x_start = 2023.4, y_start = 3336 + 930 + 1380, y_end = 3250 + 936 + 1386 + 541, 
  x_text = 2023.5, y_text = 3250 + 936 + 1386 + (541 / 2),
  label = "Deaths within a year of leaving treatment with a cause\nother than poisoning"
)

p1_parts <- add_segment_annotation(
  p1_parts, x_start = 2023.4, y_start = 3336 + 930 + 1380 + 541, y_end = 3250 + 936 + 1386 + 541 + 2808, 
  x_text = 2023.5, y_text = 3250 + 936 + 1386 + 541 + (2808 / 2),
  label = "Deaths a year or more after leaving treatment with\na cause other than poisoning",
  color = "darkgrey"
)

# Additional segment with different orientation
p1_parts <- p1_parts +
  geom_segment(
    x = 2023, xend = 2020.75, y = all_except_morethan_year_count,
    arrow = arrow(length = unit(.3, "cm"))
  )

p1_parts


# Save the plot as a PNG file with specified dimensions and resolution
png(filename = "plots/drugs_additional_deaths_broad_cat.png", width = 30, height = 20, unit = "cm", res = 200)
p1_parts
dev.off()
ggsave("plots/drugs_additional_deaths_broad_cat.png", plot = p1, width = 30, height = 20, units = "cm", dpi = 200)
