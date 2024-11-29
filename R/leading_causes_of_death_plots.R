plot_data <-
  bind_rows(deaths_related_to_drug_misuse, ons_leading_mortality_causes) |>
  filter(age_group != "80 years and over")


age_groups <-
  unique(pull(plot_data, age_group))


plot_leading_causes <-
  function(ages) {
    data <- plot_data |>
      filter(age_group == ages) |>
      arrange(count) |>
      mutate(death_category = as_factor(death_category)
             ) |> 
      mutate(fill_colour = if_else(death_category == "Deaths associated with drug use", "Drug misuse", "Other")) |> 
      mutate(fill_colour = factor(fill_colour, levels = c(
        "Other", "Drug misuse"
      )))
    data |>
      ggplot(aes(x = death_category, y = count, fill = death_category)) +
      geom_col(aes(fill = fill_colour), colour = "black", width = 0.65) +
      #  facet_wrap(~age_group, scales = "free") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
#      scale_fill_manual(values = c("Deaths associated with drug use" = "red", "black")) +
      my_theme +
      scale_fill_dhsc() +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 12, colour = "black"),
            title = element_text(size = 14),
            plot.title.position = "plot") +
      labs(
        title = glue::glue("Leading causes of death for ages {ages}"),
        subtitle =  "Compared with estimated deaths associated with drug misuse",
        y = "Number of deaths (2022)",
        x = NULL 
      )
  }


plots <- list()


for (i in seq_along(age_groups)){
plots[[i]] <- plot_leading_causes(ages = age_groups[i])
}

# Loop over each plot in the 'plots' list and save it as a PNG file
for (i in 1:length(plots)) {
  # Create a filename with leading zeros (e.g., "01_drug_and_alcohol_plot.png")
  filename <- sprintf("plots/%02d_leading_causes_plot.png", i)
  
  # Open a PNG device with the specified dimensions and resolution
  png(filename = filename, height = 11.72, width = 23.44, units = "cm", res = 200)
  
  # Render the plot
  print(plots[[i]])
  
  # Close the device
  dev.off()
}
