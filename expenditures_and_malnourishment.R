health_expenditures_df <- all_health_expenditures_df %>% slice(1:217)

expenditures_and_malnourishment <- function(input) {
start_year <- reactive(input$plot_year[1])
end_year <- reactive(input$plot_year[2])
categories <- c("Wasting", "Overweight", "Underweight", "Stunting")

health_expenditures_average_rate_df <- health_expenditures_df %>%
  pivot_longer(cols = matches(year_regex), 
               names_pattern = year_regex,
               names_to = c(".value", "year")
  ) %>% 
  select(Country.Name, Country.Code, year, amount) %>%
  rename(average_health_expenditure = amount) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% (start_year:end_year)) %>%
  group_by(Country.Code, Country.Name) %>%
  summarize(average_health_expenditure = mean(average_health_expenditure, na.rm = TRUE), .groups="drop")

malnourishment_average_df <- malnourishment_df %>%
  rename(Country.Code = ISO.code) %>%
  filter(Year %in% (start_year:end_year)) %>%
  group_by(Country.Code) %>%
  summarize(Wasting = mean(Wasting, na.rm = TRUE),
            Overweight = mean(Overweight, na.rm = TRUE),
            Underweight = mean(Underweight, na.rm = TRUE),
            Stunting = mean(Stunting, na.rm = TRUE),
            Income.Classification = mean(Income.Classification, na.rm = TRUE),
            U5.Population...000s. = mean(U5.Population...000s., na.rm = TRUE))

pivot_malnourishment_average_df <- malnourishment_average_df %>%
  pivot_longer(cols = all_of(categories),
               names_to = "category",
               values_to = "rate") 

correlation_plot_df <- health_expenditures_average_rate_df %>%
  left_join(pivot_malnourishment_average_df, by = c("Country.Code")) %>%
  select(Country.Name, category, rate, average_health_expenditure, Income.Classification) %>%
  mutate(category = factor(category, levels=categories)) %>%
  filter(category == input$categories) %>%
  na.omit()

correlation_plot <- ggplot(correlation_plot_df,
                           aes(x = average_health_expenditure, y = rate, group = category)) +
  geom_point(aes(color=category), alpha = 0.4) +
  # use quantile regression to fit and plot the "median" curve
  geom_quantile(aes(color=category), method = "rqss", lambda = 0.3, quantiles = 0.5, size=0.75) + 
  scale_colour_brewer(labels=input$categories, palette = "Dark2") +
  labs(title="Correlation of Countries' Health Expenditures and Malnourishment Rates",
       x ="Health Expenditure in USD (per capita)", y = "Malnourishment Rates in %", color = "Malnourishment\nCategory") +
  scale_x_continuous(labels = comma, trans = "log10") +
  scale_y_continuous(limits = c(0.0, 60.0))

  return(correlation_plot)
}

# alter the plot by year range and the malnourishment categories it graphs