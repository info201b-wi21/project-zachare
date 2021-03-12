  regression_line_plot_info <- sidebarPanel(
    sliderInput(
      inputId = "regression_year",
      label = h3("Year Range to Plot"),
      min=2011, max=2018, value = c(2011, 2018), sep="", ticks=FALSE),
    checkboxGroupInput(
      inputId = "regression_categories",
      label = h3("Categories to Plot"),
      selected = c("Wasting", "Overweight", "Underweight", "Stunting"), 
      inline = FALSE, 
      choices = c("Wasting", "Overweight", "Underweight", "Stunting")))

  
  health_expenditures_and_malnourishment <- 
    tabPanel("Expenditure and Malnourishment Correlation Line Plot", 
    sidebarLayout(regression_line_plot_info, 
    mainPanel(plotlyOutput("correlation_regression_line_plot", height="700"))))
  
  line_plot_info <- sidebarPanel(
    sliderInput(
      inputId = "line_year",
      label = h3("Year Range to Plot"),
      min = 2011, max = 2018, value = c(2011, 2018), sep="", ticks=FALSE),
    checkboxGroupInput(
      inputId = "line_countries",
      label = h3("Countries to Plot"),
      selected = c("World", "Russian Federation", "United Kingdom", "Germany", 
                   "France", "Japan", "Israel", "United States", "China", "India"), 
      inline = FALSE, 
      choices = c("World", "Russian Federation", "United Kingdom", "Germany", 
                  "France", "Japan", "Israel", "United States", "China", "India")))
  
  health_expenditures_over_time <- 
    tabPanel("Health Expenditure Trends Over Time", 
    sidebarLayout(line_plot_info, 
    mainPanel(plotlyOutput("country_line_plot", height = "700"))))
  
  bubble_plot_info <- sidebarPanel(
    sliderInput(
      inputId = "bubble_expenditure",
      label = h3("Expenditure Range to Plot"),
      min = 0, max = 10000, value = c(0,10000), sep=",", ticks=FALSE),
    checkboxGroupInput(
      inputId = "bubble_continents",
      label = h3("Continents to Plot"),
      selected = c("Europe", "Asia", "Africa", "Americas", "Oceania"),
      inline = FALSE,
      choices = c("Europe", "Asia", "Africa", "Americas", "Oceania")))
    
    expenditures_and_overweight <- 
      tabPanel("Health Expenditure and Overweight Rate Bubble Plot", 
      sidebarLayout(bubble_plot_info,
      mainPanel(plotlyOutput("overweight_bubble_plot", height="700"))))
    
  choropleth_map_info <- sidebarPanel(
    radioButtons(
      inputId = "map_category",
      label = h3("Malnourishment Category"),
      choices = list("Wasting", "Overweight", "Underweight", "Stunting"),
      selected = "Wasting"))
  
  africa_map <- 
    tabPanel("Africa Malnourishment Rates", 
    sidebarLayout(choropleth_map_info, 
    mainPanel(plotlyOutput("africa_choropleth_map", height = "700"))))

ui <- navbarPage(strong("Health Expenditures and Malnourishment"), 
                 expenditures_and_overweight, health_expenditures_and_malnourishment,
                 africa_map, health_expenditures_over_time)

