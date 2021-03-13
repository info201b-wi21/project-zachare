countries = c("World", "Russian Federation", "United Kingdom", "Germany", 
              "France", "Japan", "Israel", "United States", "China", "India")
colors = list("mediumvioletred", "midnightblue", "mediumturquoise", "slateblue1",
              "gold", "firebrick1", "darkolivegreen", "yellowgreen", "plum", "coral")
names(colors) <- countries

make_line_plot <- function(input) {
  start_year <- input$line_year[1]
  end_year <- input$line_year[2]
  selected_countries <- c(input$line_countries)
  selected_colors <- unlist(colors[names(colors) %in% selected_countries])
  
  title <- sprintf("Health Expenditure per capita in USD from %d to %d", start_year, end_year)
  subtitle <- "Hover over the lines for more info."
  
  country_trends_df <- all_health_expenditures_df %>%
    filter(Country.Name %in% selected_countries) %>%
    mutate(difference = (!!as.name(paste0("amount_", end_year)) - !!as.name(paste0("amount_", start_year))))

  country_trends_df_no_world <- country_trends_df %>% filter(Country.Name != 'World')
  min_diff <- min(country_trends_df_no_world$difference, na.rm = TRUE)
  max_diff <- max(country_trends_df_no_world$difference, na.rm = TRUE)
  mean_diff <- mean(country_trends_df_no_world$difference, na.rm = TRUE)
  
  caption <- paste("Expenditure difference stats: min=", dollar(min_diff),
                   ", max=", dollar(max_diff), ", mean=", dollar(mean_diff), sep="")
  
  pivoted_country_trends_df <- country_trends_df %>%
    pivot_longer(cols = matches(year_regex),
               names_pattern = year_regex,
               names_to = c(".value", "year")) %>%
    select(Country.Name, Country.Code, year, amount) %>%
    mutate(year = as.numeric(year))
  
  country_line_plot <-
    ggplot(data=pivoted_country_trends_df %>%
             mutate(Country.Name = factor(Country.Name, levels=selected_countries)),
           aes(x=year, y=amount, group=Country.Name, 
               text = paste("Country:", Country.Name, 
                            "<br>Health Expenditure: $", round(amount, digit = 2)))) +
    geom_line(aes(color=Country.Name), size = 0.5) +
    geom_point(aes(color=Country.Name)) +
    scale_colour_manual(labels=selected_countries, values=selected_colors, name="") +
    labs(title=title, subtitle=subtitle, 
         x ="Year", y = "Health Expenditure per capita in USD", color = "Country") +
    scale_x_continuous(limits = c(start_year, end_year)) +
    scale_y_continuous(label=comma)
  
  return(ggplotly(country_line_plot, tooltip = "text") %>%
           layout(margin=list(t = 75),
                  title = list(text = paste0(title, '<br>', '<sub><i>', subtitle, '</i></sub>')),
                  legend = list(itemsizing='constant', y = 0.5, title=list(text = 'Country<br>')), 
                                annotations = 
                                  list(x = 1, y = 1, text = caption, 
                                       showarrow = F, xref='paper', yref='paper', 
                                       xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                       font=list(size=11, color="gray29"))))
  
}
