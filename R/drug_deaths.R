# Load necessary libraries for font, color themes, testing, data manipulation, and table creation
library(showtext)     # Adds custom fonts for plots
library(ggsci)        # Provides scientific journal color schemes
library(testthat)     # For testing code accuracy (useful for development)
library(tidyverse)    # Essential data manipulation tools
library(arrow)        # For handling parquet files
library(flextable)    # Creates flexible tables for reporting

# Load the dataset from a Parquet file
df <- read_parquet("data/raw/ndtms_mortality_data.parquet")

df

df |> 
  select(Treatment_Status) |> 
  unique() |> 
  write_csv("tx_statuses.csv")

# Generate a summarized dataset with custom groupings and filters
demo_sus_drd_method <- 
  arrow_table(df) %>%
  mutate(
    # Check if there was contact with treatment and categorize accordingly
    ndtms_match = if_else(
      Treatment_Status == "no match with NDTMS",
      "No record of contact with treatment system",
      "Record of contact with treatment system"
    )
  ) |>
  filter(drug_group == "Total Deaths") |>
  mutate(
    # Mark if the death is classified as drug misuse by ONS
    ons_misuse = if_else(
      drug_misuse_combined == 1,
      "Recorded as drug misuse by ONS",
      "Not recorded as drug misuse by ONS"
    )
  ) |>
  # Group by year, treatment match, and ONS misuse classification
  group_by(reg_year, ndtms_match, ons_misuse) |>
  summarise(n = n()) |>
  filter(reg_year > 2022) %>%
  collect()

# Pivot the data to a wider format and create a nicely formatted table with totals
demo_sus_drd_method %>% 
  pivot_wider(names_from = ndtms_match, values_from = n) |>
  ungroup() |>
  select(-reg_year) %>% 
  janitor::adorn_totals("col") %>% 
  flextable::flextable(cwidth = 1.5) %>% 
  theme_booktabs() %>% 
  set_header_labels(values = c(ons_misuse = ""))

# Filter dataset for drug misuse or lack of treatment match, with additional categorization
df <- 
  arrow_table(df) %>% 
  filter(drug_misuse_combined == 1 | (drug_misuse_combined == 0 & Treatment_Status != "no match with NDTMS")) %>% 
  mutate(additional_poisoning_death = if_else(drug_misuse_combined == 1, "Initial poisoning deaths", "Additional poisoning deaths")) %>% 
  collect()

# Filter and process data for poisoning deaths by year
pd <- 
  df %>% 
  filter(drug_group == "Total Deaths", reg_year %in% c(2022, 2023))  %>% 
  mutate(
    ons_misuse = if_else(
      drug_misuse_combined == 1,
      "Recorded as drug misuse by ONS",
      "Not recorded as drug misuse by ONS"
    ),
    additional_poisoning_deaths = if_else(drug_misuse_combined == 1, "Initial poisoning deaths", "Additional poisoning deaths")
  ) |>
  group_by(reg_year, DAT, DAT_NM, ageinyrs, sex, Treatment_Status, additional_poisoning_deaths) %>% 
  tally(name = "count") %>% 
  janitor::clean_names() %>% 
  ungroup()

# Update the 'sex' column with readable labels
pd <- pd %>% mutate(sex = case_match(sex, "M" ~ "Male", "F" ~ "Female"))

# Summarize data at the local authority level for IMD analysis
pd |> 
  group_by(reg_year, dat, dat_nm) |> 
  summarise(count = sum(count)) |> 
  write_csv("drug_poisoning_deaths_misuse_la.csv")

# Load additional mortality data for treatment-related deaths
txd <- read_parquet("data/raw/tx_deaths_la_2122_2223.parquet")

# Extract the year from the 'period_range' field for easier time grouping
txd <- txd %>% mutate(period = as.integer(str_extract(period_range, pattern =  "\\d{4}")) + 1)

# Filter out specific death causes and keep essential columns
txd <- 
  txd %>% 
  filter(death_cause != "Drug poisoning") |> 
  filter(death_cause != "Alcohol-specific death") |> 
  filter(drug_group != "alcohol only") %>% 
  select(-geography, -period_range)

# Aggregate counts by period, area code, and name
txd |> 
  group_by(period, area_code, area_name) |> 
  select(-age) |> 
  summarise(across(where(is.numeric), sum)) |> 
  write_csv("data/processed/tx_deaths_la.csv")

# National-level summaries for both poisoning and non-poisoning deaths
txd_national <- 
  txd %>% 
  group_by(period, age, sex, treatment_status, death_cause) %>% 
  summarise(count = sum(count)) 

pd_national <- 
  pd %>% 
  group_by(reg_year, ageinyrs, sex, additional_poisoning_deaths) %>% 
  summarise(count = sum(count))

# Align column names between datasets
colnames(pd_national) <- grep(pattern = "treatment_status", colnames(txd_national), perl = TRUE, value = TRUE, invert = TRUE)

# Combine national data into a single table
drug_deaths_national <- bind_rows(txd_national, pd_national)

# Assign "Poisoning w/ drug misuse" as the default treatment status for missing entries
drug_deaths_national <- 
  drug_deaths_national |> 
  ungroup() |> 
  mutate(treatment_status = if_else(is.na(treatment_status), "Poisoning w/ drug misuse", treatment_status))

# Create a summary table for national drug-related deaths by treatment status and cause
drug_deaths_national |> 
  group_by(period, treatment_status, death_cause) |> 
  summarise(count = sum(count)) |> 
  pivot_wider(names_from = period, values_from = count, values_fill = 0) |> 
  mutate(treatment_status = factor(treatment_status, levels = c("Died in treatment", "Died within a year of discharge", "Died one or more years following discharge", "Poisoning related to drug misuse"))) |> 
  arrange(treatment_status, -`2023`) |> 
  write_csv("data/processed/death_cause_table.csv")

# Save the dataset if it doesn't already exist
if (!file.exists("data/processed/drug_deaths_national.csv")){
  write_csv(drug_deaths_national, "data/processed/drug_deaths_national.csv")
}

# I think this is best represented as a table or as just one year
# or the average of two years. 

drug_deaths_national |>
  group_by(period, treatment_status, death_cause) |>
  summarise(count = sum(count)) |>
  filter(treatment_status !=  "Died one or more years following discharge") |> 
  group_by(period, death_cause) |> 
  summarise(count = sum(count)) |> 
  arrange(count) |> 
  mutate(death_cause = as_factor(death_cause)) |> 
  ggplot(aes(x = death_cause, y = count)) +
  geom_col() +
  facet_wrap(~period, nrow = 2) + 
  coord_flip()


p0 <- 
drug_deaths_national |>
  group_by(period, treatment_status, death_cause) |>
  summarise(count = sum(count)) |>
  filter(treatment_status !=  "Died one or more years following discharge") |> 
  group_by(period, death_cause) |> 
  summarise(count = sum(count)) |> 
  arrange(count) |> 
  mutate(death_cause = as_factor(death_cause)) |> 
  mutate(period = as_factor(period)) |> 
  ggplot(aes(x = death_cause, y = count, fill = period)) +
  geom_col( position= "dodge", alpha = 0.8, colour = "black") +
  coord_flip() + 
  scale_fill_lancet() + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Count of causes of death in cohort", x = "Cause of death", y = "Count of deaths", fill = "") + 
  theme(legend.position = "bottom", legend.justification = "left")

png(filename = "plots/causes_of_death.png", width = 30, height = 20, unit = "cm", res = 200)
p0
dev.off()

# Filter deaths by category (e.g., "poisoning") for plotting
drug_deaths_national_dc <- 
  drug_deaths_national %>% 
  mutate(death_category = case_when(
    str_detect(death_cause, "poisoning") ~ death_cause,
    TRUE ~ paste("Non-poisoning deaths", treatment_status, sep = ": ")
  )) %>% 
  group_by(period, death_category) %>% 
  summarise(count = sum(count)) 

# Order categories for consistent plot legend
drug_deaths_national_dc <- 
  drug_deaths_national_dc %>% 
  mutate(death_category = factor(death_category, levels = rev(c(
    "Initial poisoning deaths",
    "Additional poisoning deaths",
    "Non-poisoning deaths: Died in treatment",
    "Non-poisoning deaths: Died within a year of discharge",
    "Non-poisoning deaths: Died one or more years following discharge"
  )), ordered = TRUE))

# There are defintional differences between ONS and NDTMS here, so at the national
# level I'm going to replace the aggregate drug_misuse_combined count with the
# published ONS figure. The difference in 2023 is only 17.

published_national_misuse_figures <- 
  read_csv("data/processed/published_ons_figures.csv") |> 
  filter(year %in% c(2022, 2023)) |>
  pull(drug_misuse) |>
  `names<-`(c("2023", "2022"))

published_national_misuse_figures
drug_deaths_national_dc <-
drug_deaths_national_dc |> 
  mutate( count = case_when(
    period == 2022 & death_category == "Initial poisoning deaths" ~ published_national_misuse_figures["2022"],
    period == 2023 & death_category == "Initial poisoning deaths" ~ published_national_misuse_figures["2023"],
    TRUE ~ count
    
  ))

total_deaths_associated <- 
  drug_deaths_national_dc |> 
  filter(death_category != "Non-poisoning deaths: Died one or more years following discharge") |> 
  group_by(period) |> 
  summarise(count = sum(count)) |> 
  filter(period == 2023) |> pull(count)

# Generate a bar plot showing the count of deaths per year by category
p1 <- 
  drug_deaths_national_dc %>% 
  ggplot(aes(x = period, y = count)) + 
  geom_col(aes(fill = death_category, alpha = death_category, linetype = death_category), colour = "black", width = 0.5) +
  geom_text(aes(label = scales::comma(count), group = death_category, alpha = death_category), position = position_stack(0.5)) + 
  scale_fill_lancet(alpha = 0.8) + 
  theme_bw() + 
  theme(legend.position = "none", legend.justification = "left", legend.direction = "vertical", axis.ticks.x = element_blank(), panel.grid = element_blank(), text = element_text()) +
  scale_y_continuous(labels = scales::comma, breaks = c(2500, 5000, total_deaths_associated, 7500)) + 
  scale_x_continuous(limits = c(2021,2026), breaks = c(2022, 2023)) +
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
  )) + labs(fill = NULL, x = NULL, y = "Count of deaths", title = "Deaths related to drug misuse")





# Add annotations to the plot to clarify categories for the viewer
p1_parts <- 
  p1 + 
  geom_segment(x = 2023.4, y = 0, yend = 3265, arrow = arrow(ends = "both", angle = 90, length = unit(.2, "cm"))) +
  annotate("text", x = 2023.5, y = 1668, label = "Drug poisoning deaths related to drug misuse\nas classified in ONS data", hjust = 0) +
  geom_segment(x = 2023.4, y = 3353, yend = 3265 + 936, arrow = arrow(ends = "both", angle = 90, length = unit(.2, "cm"))) +
  annotate("text", x = 2023.5, y = 3265 + (936 / 2), label = "Drug poisoning deaths with contact with treatment system\nbut not classified as related to drug misuse in ONS data", hjust = 0) +
  geom_segment(x = 2023.4, y = 3353 + 930, yend = 3265 + 936 + 1386, arrow = arrow(ends = "both", angle = 90, length = unit(.2, "cm"))) +
  annotate("text", x = 2023.5, y = 3265 + 936 + (1386 / 2), label = "Deaths in treatment with a cause other than poisoning", hjust = 0) +
  geom_segment(x = 2023.4, y = 3353 + 930 + 1380, yend = 3265 + 936 + 1386 + 541, arrow = arrow(ends = "both", angle = 90, length = unit(.2, "cm"))) +
  annotate("text", x = 2023.5, y = 3265 + 936 + 1386 + (541 / 2), label = "Deaths within a year of leaving treatment with a cause\nother than poisoning", hjust = 0) +
  geom_segment(x = 2023.4, y = 3353 + 930 + 1380 + 541, yend = 3265 + 936 + 1386 + 541 + 2808, arrow = arrow(ends = "both", angle = 90, length = unit(.2, "cm")), colour = "darkgrey") +
  annotate("text", x = 2023.5, y = 3265 + 936 + 1386 + 541 + (2808 / 2), label = "Deaths a year more after leaving treatment with\na cause other than poisoning", hjust = 0, colour = "darkgrey") +
  geom_segment(x = 2023, xend = 2020.75, y = total_deaths_associated, arrow = arrow(length = unit(.3, "cm")))

p1_parts

# Save the plot as a PNG file with specified dimensions and resolution
png(filename = "plots/drugs_additional_deaths_broad_cat.png", width = 30, height = 20, unit = "cm", res = 200)
p1_parts
dev.off()
