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

df %>%
  mutate(death_category = factor(death_category, levels = rev(c(
    "Initial alcohol-specific deaths",
    "Additional alcohol deaths",
    "Initial poisoning drug deaths",
    "Additional poisoning drug deaths",
    "Non-poisoning drug deaths: Died in treatment",
    "Non-poisoning drug deaths: Died within a year of discharge"
  )))) %>%
  ggplot(aes(x = 1, y = count)) +
  geom_hline(yintercept = sum(df$count), linetype = 2) +
  geom_col(aes(group = death_category, colour = substance), alpha = 0.7, fill = "white") +
    geom_text(
      aes(
        label = scales::comma(count),
        group = death_category
      ),
      position = position_stack(0.5),
      size = 4
    ) +
  geom_text(aes(label = death_category, group = death_category, x = 1.7),
            position = position_stack(0.5), hjust = 0)+
  my_theme +
  scale_colour_dhsc() +
  scale_x_continuous(limits = c(0.5, 5)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 5000, 10000, sum(df$count))) +
  theme(legend.position = 'none', axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = NULL, y = "Deaths (n)")



