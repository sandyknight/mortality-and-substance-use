# Function to create the plot
plot_national_data <- function(plot_data) {
  ttl <- 
    plot_data |> 
    filter(death_category != "Non-poisoning deaths: Died one or more years following discharge") |> 
    pull(count) |> sum()
  
  
  ggplot(plot_data, aes(x = year, y = count)) + 
    geom_col(
      aes(
        fill = death_category, 
        alpha = death_category, 
        linetype = death_category
      ), 
      colour = "black", 
      width = 0.3
    ) +
    geom_text(
      aes(
        label = scales::comma(count), 
        group = death_category, 
        alpha = death_category
      ), 
      position = position_stack(0.5),
      size = 4
    ) + 
    ggsci::scale_fill_lancet(alpha = 0.8) + 
    theme_bw() + 
    theme(
      text = element_text(size = 18),
      legend.position = "none",
      axis.ticks.x = element_blank(),
      panel.grid = element_blank()
    ) +
    scale_y_continuous(
      labels = scales::comma, 
      breaks = c(2500, 5000, ttl, 10000)
    ) + 
    scale_x_continuous(
      limits = c(2021, 2026), 
      breaks = c(2022)
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
  ttl <- 
    data |> 
    filter(death_category != "Non-poisoning deaths: Died one or more years following discharge") |> 
    pull(count) |> sum()
  
  latest_year <- max(data$year)
  annotation_data <- data %>%
    filter(year == latest_year) %>%
    arrange(rev(death_category)) %>%
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
  labels <- rev(labels)
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
        x = latest_year + 0.3, 
        xend = latest_year + 0.3, 
        y = row$y_start, 
        yend = row$y_end,
        arrow = arrow(
          ends = "both", 
          angle = 90, 
          length = unit(0.2, "cm")
        ), 
        linewidth = 1,
        colour = color
      ) +
      annotate(
        "text",
        x = latest_year + 0.4,
        y = row$y_mid,
        label = labels[[row$death_category]],
        hjust = 0,
        colour = color,
        size = 4
      )
  }
  plot <- 
  plot + 
    geom_segment(x = latest_year - 0.2, xend = latest_year - 1.1, y = ttl, yend = ttl ,linewidth = 1 , arrow = arrow(length = unit(.2, "cm")))
  
  
  return(plot)
}



create_cause_of_death_table <-
  function(cause_of_death_data) {
    cause_of_death_data |>
      group_by(period, treatment_status, death_cause) |>
      summarise(count = sum(count), .groups = "drop") |>
      pivot_wider(
        names_from = treatment_status,
        values_from = count,
        values_fill = 0
      ) |>
      select(
        death_cause,
        `Died in treatment`,
        `Died within a year of discharge`,
        `Died one or more years following discharge`
      ) |>
      arrange(`Died in treatment`) |>
      rename("Cause of death" = death_cause) |>
      flextable(cwidth = 2) |>
      theme_booktabs()
  }
