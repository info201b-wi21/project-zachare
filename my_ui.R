health_expenditures_and_malnourishment_info <- sidebarPanel(
      sliderInput(
        inputId = "plot_year",
        label = h3("Year Range to Plot"),
        min=2011, max=2018, value = c(2012, 2016), sep="", ticks=FALSE),
      checkboxGroupInput(
        inputId = "categories",
        label = h3("Categories to Plot"),
        selected = c("Wasting", "Overweight", "Underweight", "Stunting"), 
        inline = FALSE, 
        choices = c("Wasting", "Overweight", "Underweight", "Stunting")))

  
  health_expenditures_and_malnourishment <- tabPanel("Health Expenditures and Malnourishment", 
                                                     sidebarLayout(
                                                       health_expenditures_and_malnourishment_info, 
                                                       mainPanel(plotOutput("correlation_plot"))
                                                     ))
  
health_expenditures_over_time_info <- sidebarPanel(
      sliderInput(
        inputId = "year",
        label = h3("Year Range to Plot"),
        min = 2011, max = 2018, value = c(2011, 2018), sep="", ticks=FALSE),
      checkboxGroupInput(
        inputId = "countries",
        label = h3("Countries to Plot"),
        selected = c("World"), 
        inline = FALSE, 
        choices = c("World", "Russian Federation", "United Kingdom", "Germany", "France", "Japan", "Israel", "United States", "China", "India")))
  
  health_expenditures_over_time <- tabPanel("Health Expenditures Over Time", 
                                                     sidebarLayout(
                                                       health_expenditures_over_time_info, 
                                                       mainPanel(plotOutput("country_trend_plot"))
                                                     ))
  
ui <- navbarPage(strong("Health Expenditures and Malnourishment"), 
                 health_expenditures_and_malnourishment, health_expenditures_over_time)

