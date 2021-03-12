countries = c("World", "Russian Federation", "United Kingdom", "Germany", 
              "France", "Japan", "Israel", "United States", "China", "India")
colors = list("mediumvioletred", "midnightblue", "mediumturquoise", "slateblue1",
              "gold", "firebrick1", "darkolivegreen", "yellowgreen", "plum", "coral")
names(colors) <- countries

country_trends_df <- all_health_expenditures_df %>%
  pivot_longer(cols = matches(year_regex),
               names_pattern = year_regex,
               names_to = c(".value", "year")) %>%
  select(Country.Name, Country.Code, year, amount) %>%
  mutate(year = as.numeric(year))

make_line_plot <- function(input) {
  start_year <- input$line_year[1]
  end_year <- input$line_year[2]
  selected_countries <- c(input$line_countries)
  selected_colors <- unlist(colors[names(colors) %in% selected_countries])
  
  country_line_plot <-
    ggplot(data=country_trends_df %>%
             filter(Country.Name %in% selected_countries) %>%
             mutate(Country.Name = factor(Country.Name, levels=selected_countries)),
           aes(x=year, y=amount, group=Country.Name, 
               text = paste("Country:", Country.Name, 
                            "<br>Health Expenditure: $", round(amount, digit = 2)))) +
    geom_line(aes(color=Country.Name), size = 0.75) +
    geom_point(aes(color=Country.Name)) +
    scale_colour_manual(labels=selected_countries, values=selected_colors, name="") +
    labs(title=sprintf("Health Expenditure per capita in USD from %d to %d", start_year, end_year),
         x ="Year", y = "Health Expenditure per capita in USD", color = "Country") +
    scale_x_continuous(limits = c(start_year, end_year)) +
    scale_y_continuous(label=comma)
  
  return(ggplotly(country_line_plot, tooltip = "text") %>%
           layout(legend = list(y = 0.5, title=list(text = 'Country<br>'))))
}
