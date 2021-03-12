make_regression_line_plot <- function(input) {
  start_year <- input$regression_year[1]
  end_year <- input$regression_year[2]
  categories <- c("Wasting", "Overweight", "Underweight", "Stunting")
  colors <- list("mediumvioletred", "midnightblue", "mediumturquoise", "slateblue1")
  names(colors) <- categories
  
  health_expenditures_average_rate_df <- health_expenditures_group_df %>%
    filter(year %in% (start_year:end_year)) %>%
    summarize(average_health_expenditure = mean(amount, na.rm = TRUE), .groups="drop")
  
  malnourishment_average_df <- malnourishment_group_df %>%
    filter(Year %in% (start_year:end_year)) %>%
    summarize(Wasting = mean(Wasting, na.rm = TRUE),
              Overweight = mean(Overweight, na.rm = TRUE),
              Underweight = mean(Underweight, na.rm = TRUE),
              Stunting = mean(Stunting, na.rm = TRUE))
  
  pivot_malnourishment_average_df <- malnourishment_average_df %>%
    pivot_longer(cols = all_of(categories),
                 names_to = "category",
                 values_to = "rate") 
  
  selected_categories <- c(input$regression_categories)
  selected_colors <- unlist(colors[names(colors) %in% selected_categories])
  
  correlation_plot_df <- health_expenditures_average_rate_df %>%
    left_join(pivot_malnourishment_average_df, by = c("Country.Code")) %>%
    select(Country.Name, category, rate, average_health_expenditure) %>%
    filter(category %in% selected_categories) %>%
    mutate(category = factor(category, levels=selected_categories)) %>%
    na.omit()
  
  title <- "Correlation of Countries' Health Expenditures and Under 5 y/o Malnourishment Rates"
  subtitle <- "Hover over the dots for more info."
  
  correlation_regression_line_plot <- 
    ggplot(correlation_plot_df,
          aes(x = average_health_expenditure, y = rate, group = category, 
          text = paste0("Country: ", Country.Name,
                        "<br>Avg Health Expenditure per capita: $",
                        round(average_health_expenditure, digits = 2),
                        "<br>Rate: ", round(rate, digits = 1), "%"), collapse = "")) +
    geom_point(aes(color=category), alpha = 0.4) +
    # use quantile regression to fit and plot the "median" curve
    geom_quantile(aes(color=category), 
                  method = "rqss", lambda = 0.3, quantiles = 0.5, size=0.75) + 
    scale_colour_manual(labels=selected_categories, values=selected_colors) +
    labs(title=title, subtitle = subtitle,
         x ="Health Expenditure per capita in USD (log scale)",
         y = "Under 5 y/o Malnourishment Rates in %",
         color = "") +
    scale_x_continuous(labels = comma, trans = "log10") +
    scale_y_continuous(limits = c(0.0, 60.0))
  
    return(ggplotly(correlation_regression_line_plot, tooltip="text") %>%
          layout(margin=list(t = 75), 
                title = list(text = paste0(title, '<br>', '<sub><i>', subtitle, '</i></sub>')),
                legend = list(y = 0.5, title=list(text = 'Malnourishment<br>Category<br>'))))
}
