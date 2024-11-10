library(showtext)
library(ggsci)
library(testthat)
library(tidyverse)
library(arrow)
library(flextable)


df <- read_parquet("data/raw/ndtms_mortality_data.parquet")

demo_sus_drd_method <- 
  arrow_table(df) %>%
  mutate(
    ndtms_match = if_else(
      Treatment_Status == "no match with NDTMS",
      "Record of contact with treatment system",
      "No record of contact with treatment system"
    )
  ) |>
  filter(drug_group == "Total Deaths") |>
  mutate(
    ons_misuse = if_else(
      drug_misuse_combined == 1,
      "Recorded as drug misuse by ONS",
      "Not recorded as drug misuse by ONS"
    )
  ) |>
  group_by(reg_year, ndtms_match, ons_misuse) |>
  summarise(n = n()) |>
  filter(reg_year > 2022) %>%
  collect()


demo_sus_drd_method %>% 
pivot_wider(names_from = ndtms_match, values_from = n) |>
  ungroup() |>
  select(-reg_year) %>% 
  janitor::adorn_totals("col") %>% 
  flextable::flextable(cwidth = 1.5) %>% 
  theme_booktabs() %>% 
  set_header_labels(values = c(ons_misuse = ""))



df <- 
arrow_table(df) %>% 
 filter(drug_misuse_combined == 1 | (drug_misuse_combined == 0 & Treatment_Status == "no match with NDTMS")) %>% 
  mutate(additional_poisoning_death = if_else(drug_misuse_combined == 1, "Initial poisoning deaths", "Additional poisoning deaths")) %>% 
  collect()

pd <- 
df %>% 
  filter(drug_group == "Total Deaths", reg_year %in% c(2022, 2023))  %>% 
  mutate(
    ons_misuse = if_else(
      drug_misuse_combined == 1,
      "Recorded as drug misuse by ONS",
      "Not recorded as drug misuse by ONS"
    )
  ) |>
  mutate(additional_poisoning_deaths = if_else(drug_misuse_combined == 1, "Initial poisoning deaths", "Additional poisoning deaths")) %>% 
  group_by(reg_year, DAT, DAT_NM,ageinyrs,sex,Treatment_Status, additional_poisoning_deaths) %>% 
  tally(name = "count") %>% 
  janitor::clean_names() %>% 
  ungroup()


pd <- 
pd %>% 
  mutate(sex = case_match(sex, "M" ~ "Male", "F" ~ "Female"))


txd <- 
  read_parquet("data/raw/tx_deaths_la_2122_2223.parquet")


txd <- 
  txd %>% 
  mutate(period = as.integer(str_extract(period_range, pattern =  "\\d{4}")) + 1)

txd <- 
txd %>% 
#  filter(period_range == "April 2022 to March 2023") |> 
  filter(death_cause != "Drug poisoning") |> 
  filter(death_cause != "Alcohol-specific death") |> 
  filter(drug_group  != "alcohol only") %>% 
  select(-geography, -period_range)

txd
pd

txd_national <- 
txd %>% 
  group_by(period, age, sex, treatment_status, death_cause) %>% 
  summarise(count = sum(count)) 
  
pd_national <- 
pd %>% 
  group_by(reg_year, ageinyrs, sex, additional_poisoning_deaths) %>% 
  summarise(count = sum(count))



colnames(pd_national) <- grep(pattern = "treatment_status", colnames(txd_national), perl = TRUE, value = TRUE, invert = TRUE)

drug_deaths_national <- 
  bind_rows(txd_national, pd_national)


drug_deaths_national_dc <- # Deaths by death category and year only
drug_deaths_national %>% 
  mutate(death_category = case_when(
    str_detect(death_cause, "poisoning") ~ death_cause,
    TRUE ~ paste("Non-poisoning deaths", treatment_status, sep = ": ")
  )) %>% 
  group_by(period, death_category) %>% 
  summarise(count = sum(count)) 

unique(drug_deaths_national_dc$death_category)

drug_deaths_national_dc <- 
drug_deaths_national_dc %>% 
  mutate(death_category = factor(death_category, levels = rev(c(
    "Initial poisoning deaths",
    "Additional poisoning deaths",
    "Non-poisoning deaths: Died in treatment",
    "Non-poisoning deaths: Died within a year of discharge",
    "Non-poisoning deaths: Died one or more years following discharge"
  )), ordered = TRUE))

p1 <- 
drug_deaths_national_dc %>% 
  ggplot(aes(x = period, y = count)) + 
  geom_col(aes(fill = death_category, alpha = death_category, linetype = death_category), colour = "black", width = 0.5) +
  geom_text(aes(label = scales::comma(count), group = death_category, alpha = death_category), position = position_stack(0.5)) + 
  scale_fill_lancet(alpha = 0.8) + 
  theme_bw() + 
  theme(legend.position = "none",
        legend.justification = "left",
        legend.direction = "vertical",
        #plot.margin = margin(0,10,0,0,unit = "cm"),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        text = element_text()
        ) +
  scale_y_continuous(labels = scales::comma, breaks = c(2500, 5000, 6199, 7500)) + 
  scale_x_continuous(limits = c(2021,2026), breaks = c(2022, 2023)) +
  scale_alpha_manual(
    values = c(
      "Initial poisoning deaths" = 1,
      "Additional poisoning deaths" = 1,
      "Non-poisoning deaths: Died in treatment" = 1,
      "Non-poisoning deaths: Died within a year of discharge" = 1,
      "Non-poisoning deaths: Died one or more years following discharge" = 0.3
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Initial poisoning deaths" = 1,
      "Additional poisoning deaths" = 1,
      "Non-poisoning deaths: Died in treatment" = 1,
      "Non-poisoning deaths: Died within a year of discharge" = 1,
      "Non-poisoning deaths: Died one or more years following discharge" = 3
    )
  ) + labs(fill = NULL, x = NULL, y = "Count of deaths", title = "Deaths related to drug misuse")




p1_parts <- 
  p1 + 
  geom_segment(
    x = 2023.4,
    y = 0,
    yend = 3250,
    arrow = arrow(
      ends = "both",
      angle = 90,
      length = unit(.2, "cm")
    )
  ) +
  annotate(
    "text",
    x = 2023.5,
    y = 1668,
    label = "Drug poisoning deaths related to drug misuse\nas classified in ONS data",
    hjust = 0
    
  ) +
  geom_segment(
    x = 2023.4,
    y = 3336,
    yend = 3250 + 936,
    arrow = arrow(
      ends = "both",
      angle = 90,
      length = unit(.2, "cm")
    )
  ) +
  annotate(
    "text",
    x = 2023.5,
    y = 3250 + (936 / 2),
    label = "Drug poisoning deaths in drug treatment or within a year of\nleaving treatment, but not classified as related to\ndrug misuse in ONS data",
    hjust = 0
  ) +
  geom_segment(
    x = 2023.4,
    y = 3336 + 930,
    yend = 3250 + 936 + 1386,
    arrow = arrow(
      ends = "both",
      angle = 90,
      length = unit(.2, "cm")
    )
  ) +
  annotate(
    "text",
    x = 2023.5,
    y = 3250 + 936 + (1386 / 2),
    label = "Deaths in treatment with a cause other than poisoning",
    hjust = 0
  ) +
  geom_segment(
    x = 2023.4,
    y = 3336 + 930 + 1380,
    yend = 3250 + 936 + 1386 + 541,
    arrow = arrow(
      ends = "both",
      angle = 90,
      length = unit(.2, "cm")
    )
  ) +
  annotate(
    "text",
    x = 2023.5,
    y = 3250 + 936 + 1386 + (541 / 2),
    label = "Deaths within a year of leaving treatment with a cause\nother than poisoning",
    hjust = 0
  ) +
  geom_segment(
    x = 2023.4,
    y = 3336 + 930 + 1380 + 541,
    yend = 3250 + 936 + 1386 + 541 + 2808,
    arrow = arrow(
      ends = "both",
      angle = 90,
      length = unit(.2, "cm")
    ),
    colour = "darkgrey"
  ) +
  annotate(
    "text",
    x = 2023.5,
    y = 3250 + 936 + 1386 + 541 + (2808 / 2),
    label = "Deaths a year more after leaving treatment with\na cause other than poisoning",
    hjust = 0,
    colour = "darkgrey"
  ) +
  geom_segment(
    x = 2023,
    xend = 2020.75,
    y = 6199,
    arrow = arrow(length = unit(.3, "cm"))
  )



