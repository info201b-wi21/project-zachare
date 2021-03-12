make_bubble_plot <- function(input) {
  start_year = 2011
  end_year = 2018
  selected_continents <- c(input$bubble_continents)
  
  continents <- c("Europe", "Asia", "Africa", "Americas", "Oceania")
  colors <- list("mediumvioletred", "midnightblue", "mediumturquoise", "slateblue1", "firebrick1")
  names(colors) <- continents
  
  health_expenditures_average_rate_df <- health_expenditures_group_df %>%
    filter(year %in% (start_year:end_year)) %>%
    summarize(average_health_expenditure = mean(amount, na.rm = TRUE), .groups="drop")
  
  overweight_average_df <- malnourishment_group_df %>%
    filter(Year %in% (start_year:end_year)) %>%
    summarize(Overweight = mean(Overweight, na.rm = TRUE),
              U5.Population = mean(U5.Population...000s., na.rm = TRUE))
  
  selected_colors <- unlist(colors[names(colors) %in% selected_continents])
  
  overweight_expenditure_df <- health_expenditures_average_rate_df %>%
    left_join(overweight_average_df, by = c("Country.Code")) %>%
    select(Country.Name, continent, Overweight, average_health_expenditure, U5.Population) %>%
    filter(continent %in% selected_continents) %>%
    mutate(continent = factor(continent, levels=selected_continents), 
           tooltip_text = paste("Country: ", Country.Name, "\nUnder 5 y/o Population: ", 
                                format(round(U5.Population), 
                                       big.mark=",", trim=TRUE), "k", sep = "")) %>%
    na.omit()
  
  lower_expenditure <- max(input$bubble_expenditure[1], min(overweight_expenditure_df %>% pull(average_health_expenditure)))
  higher_expenditure <- min(input$bubble_expenditure[2], max(overweight_expenditure_df %>% pull(average_health_expenditure)))
  
  title <- sprintf("Average Health Expenditures and Under 5 y/o Overweight Rates By Country (%d-%d)", start_year, end_year)
  subtitle <- "Bubble sizes reflect the Under 5 y/o population sizes. Hover over the bubbles for country info."
  overweight_bubble_plot <- ggplot(overweight_expenditure_df,
                                        aes(x=average_health_expenditure,
                                            y=Overweight,  color = continent, text = tooltip_text)) +
    geom_point(aes(size=U5.Population), alpha=0.5) +
    scale_colour_manual(labels=selected_continents, values=selected_colors, name = "") +
    scale_size(range = c(.1, 24), name = "") +
    labs(title = title,
         subtitle = subtitle,
         x = "Average Health Expenditures per Capita in USD (log scale)",
         y = "Average Percentage of Under 5 y/o Overweight") +
    scale_x_continuous(labels = comma, limits = c(lower_expenditure, higher_expenditure), trans = "log10") + 
    scale_y_continuous(labels = comma)
  
  return(ggplotly(overweight_bubble_plot, tooltip = "text") %>%
           layout(margin=list(t = 75), 
                  title = list(text = paste0(title, '<br>', '<sub><i>', subtitle, '</i></sub>')),
                  legend = list(itemsizing='constant', y = 0.5)))
}
