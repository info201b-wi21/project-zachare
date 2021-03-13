africaData <- map_data('world') %>%
  mutate(Country.Code = countrycode(region, origin = 'country.name', destination = 'iso3c'),
         continent = countrycode(region, origin = "country.name", destination = "continent")) %>%
  filter(continent == 'Africa')

malnourishment_average_df <- malnourishment_df %>%
  rename(Country.Code = ISO.code) %>%
  filter(Year %in% (2011:2018)) %>%
  group_by(Country.Code) %>%
  summarize(Wasting = mean(Wasting, na.rm = TRUE),
            Overweight = mean(Overweight, na.rm = TRUE),
            Underweight = mean(Underweight, na.rm = TRUE),
            Stunting = mean(Stunting, na.rm = TRUE), 
            U5.Population = mean(U5.Population...000s.))

africa_malnourishment_rate_data <- africaData %>%
  inner_join(malnourishment_average_df, by="Country.Code")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = 'darkgrey'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

make_choropleth_map <- function(input) {
  selected_category <- input$map_category
  weighted_mean <- weighted.mean(africa_malnourishment_rate_data %>% pull(!!as.name(selected_category)), 
                        africa_malnourishment_rate_data %>% pull(U5.Population))
  title <- paste(selected_category, "Rate in Under 5 y/o Africa Population")
  subtitle <- paste0("The aggregated ", tolower(selected_category), " rate is ", 
                     round(weighted_mean, digits = 2), "%. Hover over the countries for more info.", collapse = "")
  
  africa_choropleth_map <- ggplot(africa_malnourishment_rate_data) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, 
                               fill=!!as.name(selected_category), 
                               text = paste("Country:", region, 
                                            "<br>Rate:", 
                                            round(!!as.name(selected_category), 
                                                  digits = 2), "%")),
                 color = "grey", size = .05) + 
    coord_map(xlim=c(-20, 52), ylim=c(-36, 36)) +
    scale_fill_gradientn(colours = brewer.pal(9, name="RdPu")) +
    labs(title= title, subtitle=subtitle,  fill = paste(selected_category, "%")) +
    blank_theme
  
  plot(africa_choropleth_map)
  
  return(ggplotly(africa_choropleth_map, tooltip = "text") %>%
           layout(margin=list(t = 75),
                  title = list(text = paste0(title, '<br>', '<sub><i>', subtitle, '</i></sub>')),
                  legend = list(itemsizing='constant', y = 0.5)))
}