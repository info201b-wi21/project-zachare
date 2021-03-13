make_bubble_plot <- function(input) {
  start_year = 2011
  end_year = 2018
  selected_continents <- c(input$bubble_continents)
  selected_category <- input$bubble_category

  continents <- c("Europe", "Asia", "Africa", "Americas", "Oceania")
  colors <- list("mediumvioletred", "midnightblue", "mediumturquoise", "slateblue1", "firebrick1")
  names(colors) <- continents
  
  health_expenditures_average_rate_df <- health_expenditures_group_df %>%
    filter(year %in% (start_year:end_year)) %>%
    summarize(average_health_expenditure = mean(amount, na.rm = TRUE), .groups="drop")
  
  category_average_df <- malnourishment_group_df %>%
    filter(Year %in% (start_year:end_year)) %>%
    summarize(category = mean(!!as.name(selected_category), na.rm = TRUE),
              U5.Population = mean(U5.Population...000s., na.rm = TRUE))
  
  selected_colors <- unlist(colors[names(colors) %in% selected_continents])
  
  category_expenditure_df <- health_expenditures_average_rate_df %>%
    left_join(category_average_df, by = c("Country.Code")) %>%
    select(Country.Name, continent, category, average_health_expenditure, U5.Population) %>%
    filter(continent %in% selected_continents) %>%
    mutate(continent = factor(continent, levels=selected_continents), 
           tooltip_text = paste("Country: ", Country.Name, "\nUnder 5 y/o Population: ", 
                                format(round(U5.Population), 
                                       big.mark=",", trim=TRUE), "k", sep = "")) %>%
  na.omit()
  
  lower_expenditure <- max(input$bubble_expenditure[1], min(category_expenditure_df %>% pull(average_health_expenditure)))
  higher_expenditure <- min(input$bubble_expenditure[2], max(category_expenditure_df %>% pull(average_health_expenditure)))
  
  weighted_mean <- weighted.mean(category_expenditure_df %>% pull(category), 
                                 category_expenditure_df %>% pull(U5.Population))
  
  caption <- paste0("The aggregated ", tolower(selected_category), " rate is ", round(weighted_mean, digits = 2), "%", collapse = "")

  title <- sprintf("Average Health Expenditures and Under 5 y/o Malnourishment Rates By Country (%d-%d)", start_year, end_year)
  subtitle <- "Bubble sizes reflect the Under 5 y/o population sizes. Hover over the bubbles for country info."
  category_bubble_plot <- ggplot(category_expenditure_df,
                                        aes(x=average_health_expenditure,
                                            y=category,  color = continent, text = tooltip_text)) +
    geom_point(aes(size=U5.Population), alpha=0.5) +
    scale_colour_manual(labels=selected_continents, values=selected_colors, name = "") +
    scale_size(range = c(.1, 24), name = "") +
    labs(title = title,
         subtitle = subtitle,
         x = "Average Health Expenditures per Capita in USD (log scale)",
         y = paste("Average Percentage of Under 5 y/o ", selected_category)) +
    scale_x_continuous(labels = comma, limits = c(lower_expenditure, higher_expenditure), trans = "log10") + 
    scale_y_continuous(labels = comma)
  
  return(ggplotly(category_bubble_plot, tooltip = "text") %>%
           layout(margin=list(t = 75),
                  title = list(text = paste0(title, '<br>', '<sub><i>', subtitle, '</i></sub>')),
                  legend = list(itemsizing='constant', y = 0.5), 
                  annotations = 
                    list(x = 1, y = 1, text = caption, 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=0, yshift=0,
                         font=list(size=11, color="gray29"))))
}
