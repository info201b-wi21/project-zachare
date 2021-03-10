countries = c("World", "Russian Federation", "United Kingdom", "Germany", "France", "Japan", "Israel", "United States", "China", "India")
country_trends_df <- all_health_expenditures_df %>%
  pivot_longer(cols = matches(year_regex),
               names_pattern = year_regex,
               names_to = c(".value", "year")
  ) %>%
  select(Country.Name, Country.Code, year, amount) %>%
  mutate(year = as.numeric(year))

make_trend_plot <- function(input) {
start_year <- input$year[1]
end_year <- input$year[2]
country_trend_plot <-
  ggplot(data=country_trends_df %>%
           filter(Country.Name %in% input$countries) %>%
           mutate(Country.Name = factor(Country.Name, levels=input$countries)),
         aes(x=year, y=amount, group=Country.Name)) +
  geom_line(aes(color=Country.Name)) +
  geom_point(aes(color=Country.Name)) +
  scale_colour_brewer(labels=input$countries, palette = "Paired") +
  labs(title=sprintf("Health Expenditure in USD (per capita) from %d to %d", start_year, end_year),
       x ="Year", y = "Health Expenditure in USD (per capita)", color = "Country") +
  scale_x_continuous(limits = c(start_year, end_year)) +
  scale_y_continuous(label=comma)

return(country_trend_plot)
}