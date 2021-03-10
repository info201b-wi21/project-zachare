library('knitr')
library('ggplot2')
library('scales')
library('maps')
library('RColorBrewer')
library('countrycode', exclude = "select")
library('dplyr')
library('tidyverse')
library('naniar')
# loading datasets

malnourishment_df <- read.csv(
  'https://raw.githubusercontent.com/info201b-wi21/project-zachare/main/malnutrition-estimates%20(1).csv?token=ASLWDXAQUFCKJYCV6R5QMBDAJUQ7Y')

my_server <- function(input, output){
  
  output$summary_analysis_malnourishment <- renderPlot({
    
    start_year = input$year[1]
    end_year = input$year[2]
    
    malnourishment_average_df <- malnourishment_df %>%
      rename(Country.Code = ISO.code) %>%
      filter(Year %in% (start_year:end_year)) %>%
      group_by(Country.Code) %>%
      mutate(
        average_wasting = mean(Wasting, na.rm = TRUE),
        average_overweight = mean(Overweight, na.rm = TRUE),
        average_underweight = mean(Underweight, na.rm = TRUE),
        average_stunting = mean(Stunting, na.rm = TRUE)
      ) %>%
      select(-X, -Survey.Year, -Income.Classification, -LDC, -LIFD, -LLDC.or.SID2, 
             -Survey.Sample..N., -Severe.Wasting, -Notes, -Report.Author, -Source, -Short.Source, 
             -U5.Population...000s.) 
    
    mean_wasting <- mean(malnourishment_average_df$Wasting, na.rm=TRUE)
    mean_underweight <- mean(malnourishment_average_df$Underweight, na.rm=TRUE)
    mean_overweight <- mean(malnourishment_average_df$Overweight, na.rm=TRUE)
    mean_stunting <- mean(malnourishment_average_df$Stunting, na.rm=TRUE)
    
    avg_mal_effects <- c(mean_overweight, mean_stunting, mean_underweight, mean_wasting)
    
    malnourishment_categories_names <- c("Overweight", "Stunting", "Underweight", "Wasting")
    
    mal_df <- data.frame(malnourishment_categories_names, avg_mal_effects)
    
    knit_mal_df <- mal_df %>%
      rename("Effects" = malnourishment_categories_names, "Average Percentage" = avg_mal_effects)
    
    malnourishment_categories_bar <- ggplot(mal_df, aes(x=(malnourishment_categories_names),
                                                        y = (avg_mal_effects), fill = as.factor(malnourishment_categories_names)) ) +
      geom_bar(stat = "identity" ) +
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position="none") +
      labs(title = "Global Rates of Malnourishment Effects",
           x = "Malnourishment Effect", y = "Percentage Effected")
  })
  
  output$expenditure_graph_message <- renderText({
    
    place <- paste(" ", input$Country.Name)
    
    year_range <- paste(min(input$year), " to ", max(input$year))
    
    expenditure_graph_message <- paste("This graph is showing the health expenditures from ", year_range, " in", place, ".")
    
    expenditure_graph_message
  })
  
  output$malnourishment_graph_message <- renderText({
    
    year_range <- paste(min(input$year), " to ", max(input$year))
    
    malnourishment_graph_message <- paste("This graph is showing the mean of the malnourishment rates from ", year_range, ".")
  
    malnourishment_graph_message
  })
}
