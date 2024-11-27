library(tidyverse)
source("R/drug_deaths_functions.R")
source("R/plotting_functions.R")


drug_poisoning_deaths_file <-
  "data/raw/ndtms_mortality_data.parquet"
deaths_in_treatment_file <-
  "data/raw/tx_deaths_la_2122_2223.parquet"

national_data <- 
  combine_national_data(
    poisoning_data = process_poisoning_data(file_path = drug_poisoning_deaths_file,
                                            date_of = "occurrence",
                                            years = 2022),
    treatment_deaths_data = process_deaths_in_treatment(
      file_path = deaths_in_treatment_file,
      years = 2022,
      exclude_poisoning = TRUE,
      by_treatment_status = TRUE,
      by_death_cause = FALSE,
      exclude_alcohol_specific_deaths = TRUE
    )
  )

national_data <- 
  relabel_national_data(national_data = national_data)

df <-
  national_data %>%
  filter(!str_detect(death_category, "one or more")) %>%
  select(-year)

df <-
bind_rows(total_alcohol_deaths, df) %>%
  mutate(death_category = str_replace(death_category, "poisoning deaths","poisoning drug deaths"))

df <-
df %>%
  mutate(substance = c("alcohol","alcohol", rep("drugs", 4)))

df[c(1,2),"death_category"] <- 
  c(
    "Additional alcohol deaths",
    "Initial alcohol-specific deaths"
)

df <- 
df %>%
  mutate(death_category = factor(death_category, levels = rev(c(
    "Initial alcohol-specific deaths",
    "Additional alcohol deaths",
    "Initial poisoning drug deaths",
    "Additional poisoning drug deaths",
    "Non-poisoning drug deaths: Died in treatment",
    "Non-poisoning drug deaths: Died within a year of discharge"
  )))) %>%
    arrange(rev(death_category)) 

p1 <- 
df %>%
 ggplot(aes(x = 1, y = count)) +
  geom_hline(yintercept = sum(df$count), linetype = 2) +
  geom_col(aes(group = death_category, fill = substance), width = 0.5, alpha = 0.7, colour = "black") +
    geom_text(
      aes(
        label = scales::comma(count),
        group = death_category
      ),
      position = position_stack(0.5),
      size = 4,
      colour = "black",
      fontface = "bold"
    ) +
  geom_text(aes(label = death_category, group = death_category, x = 1.4),
            position = position_stack(0.5), hjust = 0)+
  my_theme +
  scale_fill_dhsc() +
  scale_x_continuous(limits = c(0.5, 5)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 5000, 10000, sum(df$count))) +
  theme(legend.position = 'none', axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = NULL, y = "Deaths (n)")




# Open a PNG device with the specified dimensions and resolution
png(filename = "plots/drugs_and_alcohol_plot.png", height = 20, width = 23.44, units = "cm", res = 200)

# Render the plot
p1

# Close the device
dev.off()




df <- 
df |> 
  arrange(rev(death_category))

# Assuming 'df' is your data frame and 'my_theme' and 'scale_fill_dhsc()' are defined
plots <- list()

for (i in 1:nrow(df)) {
  df_sub <- df[1:i, ]  # Subset the data frame
  
  p <- df_sub %>%
    ggplot(aes(x = 1, y = count)) +
    geom_hline(yintercept = sum(df$count), linetype = 2) +
    geom_col(aes(group = death_category, fill = substance), alpha = 0.7, colour = "black") +
    geom_text(
      aes(
        label = scales::comma(count),
        group = death_category
      ),
      position = position_stack(0.5),
      size = 4,
      colour = "black",
      fontface = "bold"
    ) +
    geom_text(aes(label = death_category, group = death_category, x = 1.55),
              position = position_stack(0.5), hjust = 0) +
    my_theme +
    scale_fill_dhsc() +
    scale_x_continuous(limits = c(0.5, 5)) +
    scale_y_continuous(labels = scales::comma, breaks = c(0, 5000, 10000, sum(df$count))) +
    theme(
      legend.position = 'none',
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x = NULL, y = "Deaths (n)")
  
  plots[[i]] <- p  # Store each plot in a list
}

# Display each plot
for (i in 1:length(plots)) {
  print(plots[[i]])
}



# Loop over each plot in the 'plots' list and save it as a PNG file
for (i in 1:length(plots)) {
  # Create a filename with leading zeros (e.g., "01_drug_and_alcohol_plot.png")
  filename <- sprintf("plots/%02d_drug_and_alcohol_plot.png", i)
  
  # Open a PNG device with the specified dimensions and resolution
  png(filename = filename, height = 11.72, width = 23.44, units = "cm", res = 200)
  
  # Render the plot
  print(plots[[i]])
  
  # Close the device
  dev.off()
}











