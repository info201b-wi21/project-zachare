  regression_line_plot_info <- sidebarPanel(
    sliderInput(
      inputId = "regression_year",
      label = h3("Year Range"),
      min=2011, max=2018, value = c(2011, 2018), sep="", ticks=FALSE),
    checkboxGroupInput(
      inputId = "regression_categories",
      label = h3("Malnourishment Effects"),
      selected = c("Wasting", "Overweight", "Underweight", "Stunting"), 
      inline = FALSE, 
      choices = c("Wasting", "Overweight", "Underweight", "Stunting")))

  
  regression_line_plot_layout <- 
    tabPanel("Expenditure and Malnourishment Correlation Line Plot", 
    sidebarLayout(regression_line_plot_info, 
    mainPanel(
      h1("How are different malnourishment effects correlated with health expenditures?"),
      plotlyOutput("correlation_regression_line_plot", height="700"))))
  
  line_plot_info <- sidebarPanel(
    sliderInput(
      inputId = "line_year",
      label = h3("Year Range"),
      min = 2011, max = 2018, value = c(2011, 2018), sep="", ticks=FALSE),
    checkboxGroupInput(
      inputId = "line_countries",
      label = h3("Countries"),
      selected = c("World", "Russian Federation", "United Kingdom", "Germany", 
                   "France", "Japan", "Israel", "United States", "China", "India"), 
      inline = FALSE, 
      choices = c("World", "Russian Federation", "United Kingdom", "Germany", 
                  "France", "Japan", "Israel", "United States", "China", "India")))
  
  line_plot_layout <- 
    tabPanel("Health Expenditure Trends Over Time", 
    sidebarLayout(line_plot_info, 
    mainPanel(
      h1("How have different countries' health expenditures changed in the past decade?"),
      plotlyOutput("country_line_plot", height = "700"))))
  
  bubble_plot_info <- sidebarPanel(
    sliderInput(
      inputId = "bubble_expenditure",
      label = h3("Health Expenditure Range"),
      min = 0, max = 10000, value = c(0,10000), sep=",", ticks=FALSE),
    checkboxGroupInput(
      inputId = "bubble_continents",
      label = h3("Continents"),
      selected = c("Europe", "Asia", "Africa", "Americas", "Oceania"),
      inline = FALSE,
      choices = c("Europe", "Asia", "Africa", "Americas", "Oceania")),
    radioButtons(
      inputId = "bubble_category",
      label = h3("Malnourishment Effect"),
      choices = list("Wasting", "Overweight", "Underweight", "Stunting"),
      selected = "Underweight"))
    
    bubble_plot_layout <- 
      tabPanel("Expenditure and Malnourishment Bubble Plot", 
      sidebarLayout(bubble_plot_info,
      mainPanel(
        h1("How are different malnourishment effects related to health expenditures 
           and under 5 year old population sizes?"),
        plotlyOutput("category_bubble_plot", height="700"))))
    
  choropleth_map_info <- sidebarPanel(
    radioButtons(
      inputId = "map_category",
      label = h3("Malnourishment Effect"),
      choices = list("Wasting", "Overweight", "Underweight", "Stunting"),
      selected = "Wasting"))
  
  choropleth_map_layout <- 
    tabPanel("Africa Malnourishment Rates", 
    sidebarLayout(choropleth_map_info, 
    mainPanel(
      h1("How prevalent are different malnourishment effects in Africa?"), 
      plotlyOutput("africa_choropleth_map", height = "700"))))
  
  introduction <- 
    tabPanel("Analysis Introduction", 
    mainPanel(
      h1(strong("Health Expenditures and Malnourishment")),
      h3("An exploration of the relationship between health expenditures 
         and malnourishment effects around the world."),
      h4("Creators: Liza Volozin, Zachary Grieser, and Heather Yang"), 
      br(),
      h2("Project Domain:"), 
      p("Adult malnutrition is a severe humanitarian and public health issue 
      that primarily affects developing countries. Malnutrition refers to one's 
      condition when they are lacking necessary nutrients, often due to 
      inadequate food supply. Malnutrition compromises one's immune system,
      making them more susceptible to disease and causes delayed recovery times.
      It also is associated with many societally damaging issues such as poverty,
      food insecurity, and mortality. This problem affects millions of people 
      globally and can detriment their health long term. We are examining this 
      issue of malnutrition because of its many damaging ramifications that 
      we previously mentioned. This is both a social and health crisis that has 
      many implications and consequences on communities and individuals worldwide."),
      h2("The Data:"), 
      h4("Health Expenditure Data"),
      HTML("<p>The <a href='https://databank.worldbank.org/reports.aspx?source=2&series=SH.XPD.CHEX.PC.CD'>
      health expenditure data set</a> includes health expenditure rates per capita for every country. The
      data is broken up by year. It is derived from data prepared by the 
      World Health Organization. 'The health expenditure estimates have been 
      prepared by the World Health Organization under the framework of the 
      System of Health Accounts 2011 (SHA 2011). The Health SHA 2011 tracks 
      all health spending in a given country over a defined period of time 
      regardless of the entity or institution that financed and managed that 
      spending. It generates consistent and comprehensive data on health 
      spending in a country, which in turn can contribute to evidence-based 
      policy-making.' (World Development Indicators)."), 
      h4("Malnourishment Data"),
      HTML("<p>The <a href='https://www.kaggle.com/ruchi798/malnutrition-across-the-globe'>malnourishment data set</a> comes from data compiled by Ruchi Bhatia,
      originally sourced from UNICEF. It breaks down each set of statistics by country
      and focuses on the following malnutrition effects:"), br(), 
      HTML("1. Wasting: below -2 standard deviations from the median weight-for-height.<br>
        2. Overweightness: above 2 standard deviations from the median weight-for-height.<br>
        3. Underweightness: below -2 standard deviations from the median weight-for-age.<br>
        4. Stunting: below -2 standard deviations from median height-for-age."),
      p(em("All reported in percentage of children aged 0-59 months who meet these criteria.")), 
      br(),
      br(),
      img(src = "malnutrition.png"),
      br(),
      h2("Exploratory Questions:"),
      p("Our interactive models attempt to answer the following questions:"), 
      HTML("1. How are different malnourishment effects related to health expenditures 
           and under 5 year old population sizes?<br>
        2. How are different malnourishment effects correlated with health expenditures?<br>
        3. How prevalent are different malnourishment effects in Africa?<br>
        4. How have different countries' health expenditures changed from 2011 to 2018?"), br(), 
      p(strong("Feel free to use the interactive widgets on each page to pose similar questions of your own")), 
      br(), 
      HTML("<p>For more information, check out our <a href='https://info201b-wi21.github.io/project-zachare/index.html'>
      exploratory analysis</a>!")))

ui <- navbarPage(strong("Health Expenditures and Malnourishment"), introduction, 
                 bubble_plot_layout, regression_line_plot_layout,
                 choropleth_map_layout, line_plot_layout)

